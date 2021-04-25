source("../../utils/load_result.R")
source("../../utils/get_true_pmf.R")
pmf_comparison_plot <- function(data_name,n_way){
  # Plot marginal pmf of imputed dataset against the population dataset
  # data_name: the name of dataset to be plotted
  # n_way: the number of way in the joint probability to be considered
  
  # return: NULL
  models = c('MICE_NOM', 'MICE','CART', 'MICE_RF', 'FOREST', 'GAIN_CAT', 'DP', 'PROBIT')
  report_name = c('MI-Multireg', 'MI-Polr', 'MI-Cart', 'MI-Forest', 'missForest', 'GAIN', 
                  'MI-DPMPM', 'MI-DPMMVN')
  # get true pmf
  true_pmf = get_true_pmf(n_way=n_way)
  indicator = (true_pmf !=0) & (true_pmf*10000>10) & ((1-true_pmf)*10000>10)
  true_pmf = true_pmf[indicator]
  par(mfrow=c(2,4), oma = c(5.5,4.5,4,0.2) + 0.1, mar = c(0.25,0.25,1.25,1.25) + 0.1, 
      cex.axis = 1.75, cex.lab=1.75, cex.main = 1.5)
  for (i in 1:length(models)) {
    # for each model, get marginal pmf
    model_name = models[i]
    output_list = load_result(model_name, data_name, n_way= n_way)
    q_bar = output_list[['q_bar']] # mean estimate
    
    # calculate imputed pmf
    imputed_pmf = apply(q_bar, MARGIN = 2, mean)
    imputed_pmf = imputed_pmf[indicator]
    print(length(imputed_pmf))
    print(length(true_pmf))
    title = report_name[i]
    if (i==1) {
      # top left
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7), xaxt="n")
      axis(1, labels=FALSE)
    }else if(i %in% c(2,3,4)){
      # top row
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7), xaxt="n",
           yaxt="n")
      axis(1, labels=FALSE)
      axis(2, labels=FALSE)
    }else if(i == 5){
      # bottom left
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7))
    }else{
      # bottom rows
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7),
           yaxt="n")
      axis(2, labels=FALSE)
    }
    abline(0,1, col = 'gray')
    #abline(0,1.1, col = 'red')
    #abline(0, 0.9, col = 'red')
  }
  
  if (n_way==1) {
    #title_name = 'Comparison of imputed pmf and population pmf: Marginal distribution'
  }else if (n_way==2) {
    #title_name = 'Comparison of imputed pmf and population pmf: Bivariate distribution'
  }else if (n_way == 3){
    #title_name = 'Comparison of imputed pmf and population pmf: Trivariate distribution'
  }
  title(xlab = "Average imputed pmf",
        ylab = "Population pmf",
        outer = TRUE, line = 3)
  #mtext(title_name, side = 3, line = 1.5, outer = TRUE, cex = 1.2)
}

pmf_comparison_plot_noGAIN <- function(data_name,n_way){
  # Plot marginal pmf of imputed dataset against the population dataset
  # data_name: the name of dataset to be plotted
  # n_way: the number of way in the joint probability to be considered
  
  # return: NULL
  models = c('MICE_NOM', 'MICE','CART', 'MICE_RF', 'DP', 'PROBIT')
  report_name = c('MI-Multireg', 'MI-Polr', 'MI-Cart', 'MI-Forest', 'MI-DPMPM', 'MI-DPMMVN')
  # get true pmf
  true_pmf = get_true_pmf(n_way=n_way)
  indicator = (true_pmf !=0) & (true_pmf*10000>10) & ((1-true_pmf)*10000>10)
  true_pmf = true_pmf[indicator]
  par(mfrow=c(2,3), oma = c(5.5,4.5,4,0.2) + 0.1, mar = c(0.25,0.25,1.25,1.25) + 0.1, 
      cex.axis = 1.75, cex.lab=1.75, cex.main = 1.5)
  for (i in 1:length(models)) {
    # for each model, get marginal pmf
    model_name = models[i]
    output_list = load_result(model_name, data_name, n_way= n_way)
    q_bar = output_list[['q_bar']] # mean estimate
    
    # calculate imputed pmf
    imputed_pmf = apply(q_bar, MARGIN = 2, mean)
    imputed_pmf = imputed_pmf[indicator]
    title = report_name[i]
    if (i==1) {
      # top left
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7), xaxt="n")
      axis(1, labels=FALSE)
    }else if(i %in% c(2,3)){
      # top row
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7), xaxt="n",
           yaxt="n")
      axis(1, labels=FALSE)
      axis(2, labels=FALSE)
    }else if(i == 4){
      # bottom left
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7))
    }else{
      # bottom rows
      plot(imputed_pmf, true_pmf, main = title, xlim =c(0,0.7), ylim = c(0,0.7),
           yaxt="n")
      axis(2, labels=FALSE)
    }
    abline(0,1, col = 'gray')
    #abline(0,1.1, col = 'red')
    #abline(0, 0.9, col = 'red')
  }
  
  if (n_way==1) {
    #title_name = 'Comparison of imputed pmf and population pmf: Marginal distribution'
  }else if (n_way==2) {
    #title_name = 'Comparison of imputed pmf and population pmf: Bivariate distribution'
  }else if (n_way == 3){
    #title_name = 'Comparison of imputed pmf and population pmf: Trivariate distribution'
  }
  title(xlab = "Average imputed pmf",
        ylab = "Population pmf",
        outer = TRUE, line = 3)
  #mtext(title_name, side = 3, line = 1.5, outer = TRUE, cex = 1.2)
}