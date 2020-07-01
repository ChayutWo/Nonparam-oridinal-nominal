library(abind)
probitBayesImputation <- function(y, N = 40, Mon = 1000, B = 5000, thin.int = 5){
  # Perform nonparametric Bayesian ordinal model for missing data imputation
  # y: dataframe containing NA
  # N: maximum number of clusters used in Dirichlet process
  # Mon: final sample size
  # B: Burn-in period
  # thin.init: thinning
  # Compute gamma.list specifying the cutoff of the latent variables for each class
  y <- y %>% mutate_all(as.numeric)
  d <- ncol(y)
  R <- ncol(y)
  n <- nrow(y)
  IndicatorMat <- is.na(y)
  
  #### define variables to record time #####
  mu_time = c()
  L_time = c()
  pprobs_time = c()
  alpha_time = c()
  MV_time = c()
  S_time = c()
  z_time = c()
  saving_time = c()
  ####prior specification####
  
  # The cutoffs according to this paper can be arbitrarily set
  gamma.list = list()
  for (z_index in 1:R) {
    numlevels <- sum(!is.na(unique(y[,z_index])))
    n_miss <- sum(IndicatorMat[,z_index])
    marginal_prob <- table(y[!IndicatorMat[,z_index],z_index])
    marginal_prob <- marginal_prob/sum(marginal_prob)
    # create cutoff that center at 0
    if (numlevels%%2==0) {
      gamma.list[[z_index]] <- c(-10000,-as.integer(numlevels/2-1):as.integer(numlevels/2-1),10000)
    } else {
      gamma.list[[z_index]] <- c(-10000,seq(from = -(numlevels-2)/2, to=(numlevels-2)/2, by = 1 ), 10000)
    }
    # initially do random imputation from marginal pmf
    y[IndicatorMat[,z_index],z_index] <- sample(1:numlevels, n_miss, 
                                                replace = TRUE, prob = marginal_prob)
  }

  # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
  R.z = rep(0, R)
  for (z_index in 1:R) {
    Ki = length(gamma.list[[z_index]])-1
    R.z[z_index] = gamma.list[[z_index]][Ki] - gamma.list[[z_index]][2]
    if (Ki==2) {
      # has only 2 levels, need to fix R.z to 0.5
      R.z[z_index] = 0.5
    }
  }

  # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
  
  ####Hyperparameters####
  
  # Prior for alpha = gamma(a.alpha, b.alpha)
  a.alpha=.5
  b.alpha=.5
  
  # Prior for NIW distribution on DP
  # N(mu| m, V)IW(sigma|v, S)
  
  # m - N(a.m, B.m)
  a.m=rep(0,R)
  B.m=diag(c((R.z/4)^2),d)/2
  
  # V - IW(a.V, B.V)
  a.V=d+2
  B.V=(a.V-d-1)*diag(c((R.z/4)^2),d)/2
  
  # S - W(a.S, B.S), fix nu = d+2
  a.S=d+2
  nu=d+2
  B.S=(nu-d-1)*(diag(c((R.z/4)^2),d))/(2*a.S)
  
  #####Store parameters####
  
  Mon.true=(Mon)*thin.int # True number of samples after thining
  
  k.mu=matrix(0,(Mon)*(d),N)
  k.sigma=matrix(0,(Mon)*(d),N*(d))
  L=matrix(0,Mon,n) # Cluster assignment
  p.probs=matrix(0,Mon,N) # marginal prob of each cluster
  alpha=rep(0,Mon) # Update alpha
  # for NIW parameter update
  M=matrix(0,Mon,d) # m
  V=matrix(0,(Mon)*(d),d) # V
  S=matrix(0,(Mon)*(d),d) # S
  
  components=rep(0,Mon)
  
  # group z together
  sampled_z = c()
  sampled_y = c()
  for (z_index in 1:R) {
    sampled_z = abind(sampled_z, matrix(0,Mon,n), along = 3)
    sampled_y = abind(sampled_y, matrix(0,Mon,n), along = 3)
  }
  
  ####Initial Values####
  
  # initialize mean for each cluster 0 for muY
  k.mu.init=matrix(0,nrow=d,ncol=N)
  
  # initialize covariance matrix for each cluster
  k.sigma.init=matrix(0,nrow=d,ncol=N*(d))
  num=seq(from=1,to=ncol(k.sigma.init)-(d-1),by=(d))
  for(i in 1:(d))
    k.sigma.init[i,num+(i-1)]=1
  
  # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
  # sd = range/6
  for (var_ind in 1:R) {
    k.sigma.init[var_ind,]=(R.z[var_ind]/6)^2*k.sigma.init[var_ind,]/2
  }
  
  
  #L.init=rep(1,n) # everyone is in the same cluster
  L.init <- sample(1:(N/3),n,replace=T) #randomly assign people to one-third of the clusters
  #L.init <- sample(1:N,n,replace=T) #randomly assign people to all the clusters
  
  # equal probability for each cluster
  p.probs.init=rep(1/N,N)
  
  # alpha in DP is 1
  alpha.init=1
  
  # mean of mean in the NIW
  M.init=rep(0, R)
  
  # covariance for mean in the NIW
  V.init=diag(rep(1,d))
  diag(V.init)=c((R.z/6)^2/5)
  
  # initial guess for covariance matrix in NIW
  S.init=k.sigma.init[1:d,1:d]*(nu-1)

  components.init=1
  z.init=matrix(0,n,R)
  for (z_index in 1:R) {
    Ki = length(gamma.list[[z_index]])-1
    # left end
    z.init[which(y[, z_index]==1), z_index]=gamma.list[[z_index]][2]-1
    # right end
    z.init[which(y[, z_index]==Ki), z_index]=gamma.list[[z_index]][Ki]+1
    # middle zi = (gamma[i] + gamma[i+1])/2
    if (Ki>2) {
      for(i in 2:(Ki-1)){
        z.init[which(y[, z_index]==i),z_index]=(gamma.list[[z_index]][i]+
                                                  gamma.list[[z_index]][i+1])/2
      }
    }
  }

  # mc_save current value
  k.mu.cur=k.mu.init
  k.sigma.cur=k.sigma.init
  L.cur=L.init
  p.probs.cur=p.probs.init
  alpha.cur=alpha.init
  M.cur=M.init
  V.cur=V.init
  S.cur=S.init
  components.cur=components.init
  z.cur = z.init
  y.cur = y
  row.cur=0
  N.j <- as.data.frame(table(factor(L.cur,levels=1:N)))$Freq #moved from inside the loop
  
  ####MCMC####
  
  for(m in 2:(Mon.true+B+1)){
    
    cat(paste("Iteration: ", m,"\n", sep = ""))
    
    # If pass burn-in and not thinned -> mc_save
    if(m>(B+1)&&(m-B-1)%%thin.int==0) {row.cur=row.cur+1; mc_save=1} else {mc_save=0}
    
    
    # Calculate number of occupied clusters
    nstar=sum(N.j!=0)
    
    # occupied cluster and number of members in those occupied
    L.j=sort(unique(L.cur))
    #N.j.star=N.j[N.j!=0]
    
    # indices is a matrix that store the index of samples that fall into each clusters
    # the size of indices will be (No. used clusters x max number of members)
    #Indices=matrix(0,nstar,max(N.j))
    #for(i in 1:nstar)
    #  Indices[i,1:N.j.star[i]]=which(L.cur==L.j[i])
    
    ###alternative to for loop
    Indices <- lapply(as.matrix(1:nstar),function(x) which(L.cur==L.j[x]))
    
    
    ## step1: sample mu and sigma for each cluster
    # r is the index for used cluster
    # i is the index for all cluster
    # j is the index for members in that cluster
    start = Sys.time()
    r=1
    for(i in 1:N){
      if(N.j[i]==0){
        
        # No member in that cluster
        # draw mu, sigma from the base distribution
        k.mu.cur[,i]=rmvnorm(1, M.cur, V.cur)
        k.sigma.cur[,((i-1)*(d)+1):(i*(d))]=riwish(nu, S.cur)
        
        if(mc_save==1){
          # mc_save mu and sigma for empty clusters
          k.mu[((row.cur-1)*(d)+1):(row.cur*(d)),i]=k.mu.cur[,i]
          k.sigma[((row.cur-1)*(d)+1):(row.cur*(d)),((i-1)*(d)+1):(i*(d))]=k.sigma.cur[,((i-1)*(d)+1):(i*(d))] 
        }
        
      }else{
        # that cluster is occupied
        #sum.zx=rep(0,d)
        #sum.sigma.zx=matrix(0,d,d)
        #for(j in 1:N.j[i]){
          # sum up the response vector of all members in that cluster
        #  sum.zx=sum.zx+z.cur[Indices[r,j],]
        #}
        
        ###alternative to for loop
        sum.zx <- colSums(matrix(z.cur[Indices[[r]],],ncol=d))
        
        
        # update prior parameter for mu - N(m, V)
        # updated covariance matrix = prior precision + sample precision
        b.star.mu=solve(solve(V.cur)+N.j[i]*solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))]))
        # updated mean vector
        a.star.mu=solve(V.cur)%*%matrix(data=M.cur,nrow=d,ncol=1)+solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))])%*%sum.zx
        # draw mu from updated distribution
        k.mu.cur[,i]=rmvnorm(1,b.star.mu%*%a.star.mu,b.star.mu)
        
        # calculate Stheta = sum[(zj - mu)(zj-mu)^T]
        #for(j in 1:N.j[i]){
        #  diff=matrix(data=z.cur[(Indices[[r]])[j],]-k.mu.cur[,i],nrow=d,ncol=1) # dx1 vector
        #  sum.sigma.zx=sum.sigma.zx+diff%*%t(diff) 
        #}
        
        ###alternative to for loop
        sum.sigma.zx <- t(as.matrix(z.cur[Indices[[r]],] - matrix(k.mu.cur[,i],byrow=T,ncol=d,nrow=N.j[i])))%*%
          as.matrix(z.cur[Indices[[r]],] - matrix(k.mu.cur[,i],byrow=T,ncol=d,nrow=N.j[i]))
        
        # draw sigma for that cluster from IW distribution
        k.sigma.cur[,((i-1)*(d)+1):(i*(d))]=riwish(nu+N.j[i],S.cur+sum.sigma.zx)
        
        r=r+1 # complete this used cluster -> move to the next
        
        if(mc_save==1){
          # mc_save sampled mu and sigma for that cluster
          k.mu[((row.cur-1)*(d)+1):(row.cur*(d)),i]=k.mu.cur[,i]
          k.sigma[((row.cur-1)*(d)+1):(row.cur*(d)),((i-1)*(d)+1):(i*(d))]=k.sigma.cur[,((i-1)*(d)+1):(i*(d))]
        }
      }
    }
    mu_time = c(mu_time, Sys.time() - start)
    #########################################################################
    ## step2: sample L
    
    #L.cur=numeric(n)
    
    # Collect cluster mean and sigma sampled from step1 into meancov.list
    #meancov.list=vector("list",N)
    #for(j in 1:N){
    #  meancov.list[[j]]=list(k.mu.cur[,j],k.sigma.cur[,((j-1)*(d)+1):(j*(d))])
    #}
    # Calculate p(Z|mu_l, Sigma_l) for each sample for each cluster = p(Z|L)
    #norm.dens.list=lapply(meancov.list, function(item) dmvnorm (z.cur, item[[1]], item[[2]]))
    # Turn that into a matrix of size 198x40: ij entry is p(Zi|L = j)
    #norm.dens.mtrx=NULL
    #for(j in 1:N){
    #  norm.dens.mtrx=cbind(norm.dens.mtrx,as.numeric(norm.dens.list[[j]]))
    #}
    
    # Sample new cluster assignment from P(L|z) = P(z|L)P(L)
    #for(i in 1:n){
    #  L.cur[i]=sample.int(N,size=1,prob=norm.dens.mtrx[i,]*p.probs.cur)
    #}
    #if(mc_save==1){
    #  L[row.cur,]=L.cur
    #}
    
    ###rewrite everything into a single for loop
    start = Sys.time()
    norm.dens.mtrx=NULL
    for(j in 1:N){
      # Calculate p(Z|mu_l, Sigma_l) for each sample for each cluster = p(Z|L)
      k.mu.cur_j <- k.mu.cur[,j]
      k.sigma.cur_j <- k.sigma.cur[,((j-1)*(d)+1):(j*(d))]
      norm.dens.mtrx_j <- dmvnorm(z.cur, k.mu.cur_j, k.sigma.cur_j)
      # Turn that into a matrix of size 198x40: ij entry is p(Zi|L = j)
      norm.dens.mtrx <- cbind(norm.dens.mtrx,norm.dens.mtrx_j)
    }
    ###alternative to for loop
    full_prob_matrix <- norm.dens.mtrx*matrix(p.probs.cur,byrow=T,ncol=N,nrow=n)
    full_prob_matrix <- full_prob_matrix/matrix(rowSums(full_prob_matrix),nrow=n,ncol=N)
    Ran_unif <- runif(nrow(full_prob_matrix))
    cumul <- full_prob_matrix%*%upper.tri(diag(ncol(full_prob_matrix)),diag=TRUE)
    L.cur <- rowSums(Ran_unif>cumul) + 1L
    
    #update components: Number of occupied clusters
    components.cur=length(unique(L.cur))
    #if(mc_save==1)
    #  components[row.cur]=components.cur
    #moved to the end of mcmc
    L_time = c(L_time, Sys.time() - start)
    #########################################################################
    ## step3: Sample p.probs
    
    # Calculate Nj (Don't know why they don't use Nj) -- I've changed it!
    #M.config=numeric(N)
    #for(i in 1:N)
    #  M.config[i]=sum(L.cur==i)
    
    # Update stick breaking process: Vi
    #vstar=numeric(N-1)
    #for(i in 1:(N-1)){
    #  vstar[i]=rbeta(1,1+N.j[i],alpha.cur+sum(N.j[(i+1):N]))
    #  if(vstar[i]==0) vstar[i]=.00001
    #  if(vstar[i]==1) vstar[i]=.9999
    #}
    
    # Update pi for each cluster (mixing proportion)
    # Using updated Vi and stick breaking process
    #p.probs.cur[1]=vstar[1]
    #prod=1
    #for(i in 2:(N-1)){
    #  prod=prod*(1-vstar[i-1])
    #  p.probs.cur[i]=vstar[i]*prod
    #}
    #if((1-sum(p.probs.cur[1:(N-1)]))>=0)
    #  p.probs.cur[N]=1-sum(p.probs.cur[1:(N-1)]) else p.probs.cur[N]=0
    
    
    ###rewrite step3 without any loops
    start = Sys.time()
    N.j <- as.data.frame(table(factor(L.cur,levels=1:N)))$Freq
    vstar <- rep(1,length(p.probs.cur))
    vstar[1:(N-1)] <- rbeta((N-1),(1L+N.j[1:(N-1)]),(alpha.cur+(sum(N.j)-cumsum(N.j[-N]))))
    if(length(which(vstar[-N]==1))>0){
      vstar[which(vstar[-N]==1)] <- 0.99999
    }
    if(length(which(vstar[-N]==0))>0){
      vstar[which(vstar[-N]==0)] <- .00001
    }
    one_min_vstar <- 1L-vstar
    one_min_vstar_prod <- c(1,cumprod(one_min_vstar[1:(N-1)]))
    p.probs.cur <- vstar*one_min_vstar_prod
    if((1-sum(p.probs.cur[1:(N-1)]))>=0)
      p.probs.cur[N]=1-sum(p.probs.cur[1:(N-1)]) else p.probs.cur[N]=0
    
    pprobs_time = c(pprobs_time, Sys.time() - start)
    #if(mc_save==1)
    #  p.probs[row.cur,]=p.probs.cur
    #moved to the end of mcmc
    
    #########################################################################
    # step4: sample alpha
    
    # sample new alpha (for stick breaking) from the posterior gamma dist
    start = Sys.time()
    alpha.cur=rgamma(1,a.alpha+N-1,rate=b.alpha-sum(log(1-vstar[-N])))
    #if(mc_save==1)
    #  alpha[row.cur]=alpha.cur
    #moved to the end of mcmc
    alpha_time = c(alpha_time, Sys.time() - start)
    #########################################################################
    # step5: sample M and V (parameter for DP NIW)
    
    # sample M from normal dist
    # Update covariance matrix
    start = Sys.time()
    B.m.star=solve(solve(B.m)+N*solve(V.cur))
    # Update mean vector
    #sum=0
    #for(i in 1:N){
    #  sum=sum+solve(V.cur)%*%k.mu.cur[,i]
    #}
    
    ###alternative to for loop
    sum <- as.matrix(rowSums(solve(V.cur)%*%k.mu.cur))
    
    a.m.star=B.m.star%*%(solve(B.m)%*%a.m+sum)
    
    # Sample M from full conditionally
    M.cur=rmvnorm(1,a.m.star,B.m.star)
    #if(mc_save==1)
    #  M[row.cur,]=M.cur
    #moved to the end of mcmc
    
    # sample V from IW
    #sum=matrix(0,nrow=d,ncol=d)
    #for(i in 1:N){
      # diff = mu-M
    #  mu.diff=matrix(data=k.mu.cur[,i]-M.cur,nrow=d,ncol=1)
      # calculate sum(mu-M)(mu-M)^T
    #  sum=sum+mu.diff%*%t(mu.diff)
    #}
    
    ###alternative to for loop
    sum <- as.matrix(k.mu.cur - matrix(M.cur,nrow=d,ncol=N))%*%
      t(as.matrix(k.mu.cur - matrix(M.cur,nrow=d,ncol=N)))
    
    V.cur=riwish(a.V+N,B.V+sum)
    
    #if(mc_save==1)
    #  V[((row.cur-1)*(d)+1):(row.cur*(d)),]=V.cur
    #moved to the end of mcmc
    MV_time = c(MV_time, Sys.time() - start)
    #########################################################################
    # step6: sample S (parameter for DP NIW)
    # The posterior of S will be Wishart
    start = Sys.time()
    sum.S=matrix(0,d,d)
    
    for(i in 1:N){
      sum.S=sum.S+solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))])
    }
    
    S.cur=rwish(a.S+nu*N,solve(solve(B.S)+sum.S))
    #if(mc_save==1) S[((row.cur-1)*(d)+1):(row.cur*(d)),]=S.cur
    #moved to the end of mcmc
    
    S_time = c(S_time, Sys.time() - start)
    #########################################################################
    # step7: sample z
    # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
    start = Sys.time()
    # Loop over active cluster
    for (cluster_id in unique(L.cur)) {
      # get covariance matrix for that cluster
      sig <- k.sigma.cur[,((cluster_id-1)*(d)+1):((cluster_id)*(d))]
      
      # Loop over variables z
      for (z_index in 1:R) {
        # mu.zi and mu.xi for that cluster
        mu.zi = k.mu.cur[z_index,cluster_id]
        mu.xi = k.mu.cur[-z_index,cluster_id]
        # variance/covariance of different terms
        sig.zx = matrix(data=sig[z_index,-z_index],nrow=1,ncol=d-1)
        sig.xz = t(sig.zx)
        sig.xx = matrix(data=sig[-z_index,-z_index],nrow=d-1,ncol=d-1)
        sig.zz = sig[z_index,z_index]
        # calculate conditional variance of z|x: s.z
        sig.zx_inv_sig.xx = sig.zx%*%solve(sig.xx)
        s.z=sig.zz-sig.zx_inv_sig.xx%*%sig.xz
        
        
        # get boundary for variable z: gamma_i
        #gamma_i = gamma.list[[z_index]]
        
        # Loop over observations in that cluster
        #for (i in (1:n)[L.cur == cluster_id]) {
          # xi - mu.xi: diff
        #  diff=matrix(data=z.cur[i,-z_index]-mu.xi,nrow=d-1,ncol=1)
          # calculate conditional mean: m.z
        #  m.z=mu.zi+sig.zx_inv_sig.xx%*%diff
          # if missing, sample from untruncated normal. Else, sample from truncated normal
        #  if(IndicatorMat[i,z_index]) {
            # The Y is missing
            # Sample zi from non truncated normal distribution and impute y value
        #    z.cur[i,z_index] = rnorm(1,mean=m.z,sd=sqrt(s.z))
        #    y.cur[i,z_index] = sum(z.cur[i,z_index]>gamma_i)
        #    # Set y according to gamma_i
        #  }else{
            # Sample zi from truncated normal considering yi
        #    z.cur[i,z_index]=rtruncnorm(1,a=gamma_i[y[i,z_index]],
        #                                b=gamma_i[y[i,z_index]+1],
        #                                mean=m.z,sd=sqrt(s.z))
        #  }
        #}
        
        # Block sampling
        all_index_cluster_id <- which(L.cur == cluster_id)
        z.cur_cluster_id_min_z_index <- matrix(z.cur[all_index_cluster_id,-z_index],ncol=length(mu.xi))
        diff_cluster_id <- z.cur_cluster_id_min_z_index - 
          matrix(mu.xi,nrow=nrow(z.cur_cluster_id_min_z_index),ncol=length(mu.xi),byrow=T)
        m.z <- mu.zi + diff_cluster_id%*%t(sig.zx_inv_sig.xx)
        
        # Sample all z_i and y for observations with missing entries
        gamma_z_index <- gamma.list[[z_index]]
        miss_index_cluster_id <- all_index_cluster_id[IndicatorMat[all_index_cluster_id,z_index]]
        if(length(miss_index_cluster_id) > 0){
          z.cur[miss_index_cluster_id,z_index] <- 
            rnorm(length(miss_index_cluster_id),mean=m.z[IndicatorMat[all_index_cluster_id,z_index]],sd=sqrt(s.z))
          y.cur[miss_index_cluster_id,z_index] <- 
            apply(as.matrix(z.cur[miss_index_cluster_id,z_index]),1,function(x) sum(x > gamma_z_index))
        }
        
        # Sample all z_i for observations with no missing entries
        obs_index_cluster_id <- all_index_cluster_id[!IndicatorMat[all_index_cluster_id,z_index]]
        if(length(obs_index_cluster_id) > 0){
          z.cur[obs_index_cluster_id,z_index] <- 
            rtruncnorm(length(obs_index_cluster_id),
                       a=gamma_z_index[y.cur[obs_index_cluster_id,z_index]],
                       b=gamma_z_index[y.cur[obs_index_cluster_id,z_index]+1],
                       mean=m.z[!IndicatorMat[all_index_cluster_id,z_index]],
                       sd=sqrt(s.z))
        }
        
        
      }
    }
    z_time = c(z_time, Sys.time() - start)
    # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
    start = Sys.time()
    if(mc_save==1){
      L[row.cur,] <- L.cur
      components[row.cur] <- components.cur
      p.probs[row.cur,]=p.probs.cur
      alpha[row.cur]=alpha.cur
      M[row.cur,]=M.cur
      V[((row.cur-1)*(d)+1):(row.cur*(d)),]=V.cur
      S[((row.cur-1)*(d)+1):(row.cur*(d)),]=S.cur
      
      #print(c(row.cur,m,components.cur))
      for (z_index in 1:R) {
        sampled_z[row.cur,,z_index] = z.cur[,z_index]
        sampled_y[row.cur,,z_index] = y.cur[,z_index]
      }
    }
    saving_time = c(saving_time, Sys.time() - start)
    cat(paste("Post Burn-in and thinned sample index: ", row.cur, "\n", sep = ''))
    cat(paste("Number of Occupied Components: ", components.cur, "\n", sep = ''))
    cat(paste("Alpha: ", alpha.cur, "\n", sep = ''))
    cat(paste("Observations in all Components: ",
              paste(N.j,collapse = "_|||_"), "\n\n", sep = ''))
    
    
  }
  total_time = mean(mu_time)+mean(L_time)+mean(pprobs_time)+mean(alpha_time)+mean(MV_time)+
    mean(S_time)+mean(z_time)+mean(saving_time)
  cat(paste("average mu sample time ", round(mean(mu_time),3), "\n"))
  cat(paste("average L sample time ", round(mean(L_time),3), "\n"))
  cat(paste("average pprobs sample time ", round(mean(pprobs_time),3), "\n"))
  cat(paste("average alpha sample time ", round(mean(alpha_time),3), "\n"))
  cat(paste("average MV sample time ", round(mean(MV_time),3), "\n"))
  cat(paste("average S sample time ", round(mean(S_time),3), "\n"))
  cat(paste("average z sample time ", round(mean(z_time),3), "\n"))
  cat(paste("average saving sample time ", round(mean(saving_time),3), "\n"))
  cat(paste("total time per iteration ", round(total_time,3), "\n"))
  output_list <- list(sampled_y, sampled_z,p.probs,alpha,M)
  names(output_list) <- c('sampled_y', 'sampled_z','p.probs','alpha','M')
  return(output_list)
}
