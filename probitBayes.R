probitBayesImputation <- function(y, N = 40, Mon = 2000, B = 300, thin.int = 5, seed = 0){
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

  # initially impute y with 2
  y[is.na(y)] <- 2

  ####prior specification####
  
  # The cutoffs according to this paper can be arbitrarily set
  gamma.list = list()
  for (z_index in 1:R) {
    numlevels <- length(unique(y[,z_index]))
    gamma.list[[z_index]] <- c(-10000,0:(numlevels-2),10000)
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
  
  # everyone is in the same cluster
  L.init=rep(1,n) 
  
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
    for(i in 2:(Ki-1)){
      z.init[which(y[, z_index]==i),z_index]=(gamma.list[[z_index]][i]+
                                                gamma.list[[z_index]][i+1])/2
    }
  }
  
  # save current value
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
  
  ####MCMC####
  
  set.seed(seed)
  for(m in 2:(Mon.true+B+1)){
    
    # If pass burn-in and not thinned -> save
    if(m>(B+1)&&(m-B-1)%%thin.int==0) {row.cur=row.cur+1; save=1} else {save=0}
    
    # N.j: how many members are in each cluster
    N.j=numeric(N)
    for(i in 1:N)
      N.j[i]=sum(L.cur==i)
    
    # Calculate number of occupied clusters
    nstar=sum(N.j!=0)
    
    # occupied cluster and number of members in those occupied
    L.j=sort(unique(L.cur))
    N.j.star=N.j[which(N.j!=0)]
    
    # indices is a matrix that store the index of samples that fall into each clusters
    # the size of indices will be (No. used clusters x max number of members)
    Indices=matrix(0,nstar,max(N.j))
    for(i in 1:nstar)
      Indices[i,1:N.j.star[i]]=which(L.cur==L.j[i])
    
    ## step1: sample mu and sigma for each cluster
    # r is the index for used cluster
    # i is the index for all cluster
    # j is the index for members in that cluster
    r=1
    for(i in 1:N){
      if(N.j[i]==0){
        
        # No member in that cluster
        # draw mu, sigma from the base distribution
        k.mu.cur[,i]=rmvnorm(1, M.cur, V.cur)
        k.sigma.cur[,((i-1)*(d)+1):(i*(d))]=riwish(nu, S.cur)
        
        if(save==1){
          # save mu and sigma for empty clusters
          k.mu[((row.cur-1)*(d)+1):(row.cur*(d)),i]=k.mu.cur[,i]
          k.sigma[((row.cur-1)*(d)+1):(row.cur*(d)),((i-1)*(d)+1):(i*(d))]=k.sigma.cur[,((i-1)*(d)+1):(i*(d))] 
        }
        
      }else{
        # that cluster is occupied
        sum.zx=rep(0,d)
        sum.sigma.zx=matrix(0,d,d)
        
        for(j in 1:N.j[i]){
          # sum up the response vector of all members in that cluster
          sum.zx=sum.zx+z.cur[Indices[r,j],]
        }
        
        # update prior parameter for mu - N(m, V)
        # updated covariance matrix = prior precision + sample precision
        b.star.mu=solve(solve(V.cur)+N.j[i]*solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))]))
        # updated mean vector
        a.star.mu=solve(V.cur)%*%matrix(data=M.cur,nrow=d,ncol=1)+solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))])%*%sum.zx
        # draw mu from updated distribution
        k.mu.cur[,i]=rmvnorm(1,b.star.mu%*%a.star.mu,b.star.mu)
        
        # calculate Stheta = sum[(zj - mu)(zj-mu)^T]
        for(j in 1:N.j[i]){
          diff=matrix(data=z.cur[Indices[r,j],]-k.mu.cur[,i],nrow=d,ncol=1) # dx1 vector
          sum.sigma.zx=sum.sigma.zx+diff%*%t(diff) 
        }
        # draw sigma for that cluster from IW distribution
        k.sigma.cur[,((i-1)*(d)+1):(i*(d))]=riwish(nu+N.j[i],S.cur+sum.sigma.zx)
        
        r=r+1 # complete this used cluster -> move to the next
        
        if(save==1){
          # save sampled mu and sigma for that cluster
          k.mu[((row.cur-1)*(d)+1):(row.cur*(d)),i]=k.mu.cur[,i]
          k.sigma[((row.cur-1)*(d)+1):(row.cur*(d)),((i-1)*(d)+1):(i*(d))]=k.sigma.cur[,((i-1)*(d)+1):(i*(d))]
        }
      }
    }
    
    #########################################################################
    ## step2: sample L
    
    L.cur=numeric(n)
    
    # Collect cluster mean and sigma sampled from step1 into meancov.list
    meancov.list=vector("list",N)
    for(j in 1:N){
      meancov.list[[j]]=list(k.mu.cur[,j],k.sigma.cur[,((j-1)*(d)+1):(j*(d))])
    }
    
    # Calculate p(Z|mu_l, Sigma_l) for each sample for each cluster = p(Z|L)
    norm.dens.list=lapply(meancov.list, function( item ) dmvnorm (z.cur, item[[1]], item[[2]]))
    # Turn that into a matrix of size 198x40: ij entry is p(Zi|L = j)
    norm.dens.mtrx=NULL
    for(j in 1:N){
      norm.dens.mtrx=cbind(norm.dens.mtrx,as.numeric(norm.dens.list[[j]]))
    }
    
    # Sample new cluster assignment from P(L|z) = P(z|L)P(L)
    for(i in 1:n){
      L.cur[i]=sample.int(N,size=1,prob=norm.dens.mtrx[i,]*p.probs.cur)
    }
    if(save==1){
      L[row.cur,]=L.cur
    }
    
    #update components: Number of occupied clusters
    components.cur=length(unique(L.cur))
    if(save==1)
      components[row.cur]=components.cur
    
    #########################################################################
    ## step3: Sample p.probs
    
    # Calculate Nj (Don't know why they don't use Nj)
    M.config=numeric(N)
    for(i in 1:N)
      M.config[i]=sum(L.cur==i)
    
    # Update stick breaking process: Vi
    vstar=numeric(N-1)
    for(i in 1:(N-1)){
      vstar[i]=rbeta(1,1+M.config[i],alpha.cur+sum(M.config[(i+1):N]))
      if(vstar[i]==0) vstar[i]=.00001
      if(vstar[i]==1) vstar[i]=.9999
    }
    
    # Update pi for each cluster (mixing proportion)
    # Using updated Vi and stick breaking process
    p.probs.cur[1]=vstar[1]
    prod=1
    for(i in 2:(N-1)){
      prod=prod*(1-vstar[i-1])
      p.probs.cur[i]=vstar[i]*prod
    }
    
    if((1-sum(p.probs.cur[1:(N-1)]))>=0)
      p.probs.cur[N]=1-sum(p.probs.cur[1:(N-1)]) else p.probs.cur[N]=0
    if(save==1)
      p.probs[row.cur,]=p.probs.cur
    
    #########################################################################
    # step4: sample alpha
    
    # sample new alpha (for stick breaking) from the posterior gamma dist
    alpha.cur=rgamma(1,a.alpha+N-1,rate= -sum(log(1-vstar))+b.alpha)
    if(save==1)
      alpha[row.cur]=alpha.cur
    
    #########################################################################
    # step5: sample M and V (parameter for DP NIW)
    
    # sample M from normal dist
    # Update covariance matrix
    B.m.star=solve(solve(B.m)+N*solve(V.cur))
    # Update mean vector
    sum=0
    for(i in 1:N)
      sum=sum+solve(V.cur)%*%k.mu.cur[,i]
    
    a.m.star=B.m.star%*%(solve(B.m)%*%a.m+sum)
    
    # Sample M from full conditionally
    M.cur=rmvnorm(1,a.m.star,B.m.star)
    if(save==1)
      M[row.cur,]=M.cur
    
    # sample V from IW
    sum=matrix(0,nrow=d,ncol=d)
    for(i in 1:N){
      # diff = mu-M
      mu.diff=matrix(data=k.mu.cur[,i]-M.cur,nrow=d,ncol=1)
      # calculate sum(mu-M)(mu-M)^T
      sum=sum+mu.diff%*%t(mu.diff)
    }
    V.cur=riwish(a.V+N,B.V+sum)
    
    if(save==1)
      V[((row.cur-1)*(d)+1):(row.cur*(d)),]=V.cur
    
    #########################################################################
    # step6: sample S (parameter for DP NIW)
    # The posterior of S will be Wishart
    sum.S=matrix(0,d,d)
    
    for(i in 1:N)
      sum.S=sum.S+solve(k.sigma.cur[,((i-1)*(d)+1):(i*(d))])
    
    S.cur=rwish(a.S+nu*N,solve(solve(B.S)+sum.S))
    if(save==1) S[((row.cur-1)*(d)+1):(row.cur*(d)),]=S.cur
    
    
    #########################################################################
    # step7: sample z
    # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
    for(i in 1:n){
      # extract current covariance matrix of that cluster
      sig=k.sigma.cur[,((L.cur[i]-1)*(d)+1):((L.cur[i])*(d))]
      # impute z1 given z2, z3 and X and other z's
      # iterate through zi
      for (z_index in 1:R) {
        # mu of zi in that cluster
        mu.zi = k.mu.cur[z_index,L.cur[i]]
        # variance/covariance of different terms
        sig.zx=matrix(data=sig[z_index,-z_index],nrow=1,ncol=d-1)
        sig.xz=t(sig.zx)
        sig.xx=matrix(data=sig[-z_index,-z_index],nrow=d-1,ncol=d-1)
        sig.zz=sig[z_index,z_index]
        # z_others - mu_other
        diff=matrix(data=z.cur[i,-z_index]-
                      k.mu.cur[-z_index,L.cur[i]],nrow=d-1,ncol=1)
        # calculate conditional mean and variance for zi
        m.z=mu.zi+sig.zx%*%solve(sig.xx)%*%diff
        s.z=sig.zz-sig.zx%*%solve(sig.xx)%*%sig.xz
        gamma_i = gamma.list[[z_index]]
        if (IndicatorMat[i,z_index]) {
          # The Y is missing
          # Sample zi from non truncated normal distribution and impute y value
          z.cur[i,z_index] = rnorm(1,mean=m.z,sd=sqrt(s.z))
          y.cur[i,z_index] = sum(z.cur[i,z_index]>gamma_i)
          # Set y according to gamma_i
        }else{
          # Sample zi from truncated normal considering yi
          z.cur[i,z_index]=rtruncnorm(1,a=gamma_i[y[i,z_index]],
                                      b=gamma_i[y[i,z_index]+1],
                                      mean=m.z,sd=sqrt(s.z))
        }
        
      }
    }
    # >>>>>>>>>>>>>>>> operationalized  <<<<<<<<<
    if(save==1){
      print(c(row.cur,m,components.cur))
      for (z_index in 1:R) {
        sampled_z[row.cur,,z_index] = z.cur[,z_index]
        sampled_y[row.cur,,z_index] = y.cur[,z_index]
      }
    }
  }
  return(sampled_y)
}