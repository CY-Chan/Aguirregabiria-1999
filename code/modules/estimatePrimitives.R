# estimateDemandParameters
#
# INPUT: data, the data from which parameters are to be estimated
# OUTPUT: theta1, estimates of mu, sigma (demand parameters)
estimateDemandParameters <- function(data){
  MLE<- function(par, data){
#   print(par)
    log((data$i > data$s) * dlnorm(data$s, meanlog = par[1], sdlog = par[2])  + 
          (data$i <= data$s) * (1 - plnorm(data$s, meanlog = par[1], sdlog = par[2]))) %>%
      sum
  }
  
  estimateLogNormal <- function(){
    results <- optim(runif(2), MLE, 
                     data = data, 
                     method = 'L-BFGS-B', lower = c(0.01,0.01), 
                     control = list(fnscale = -1) # maximization option
    )
    results$par
  }
  theta1 <- rep(0,2)
  
  # Repeat until the optimization doesn't throw an error because of bad initial values
  repeat{
    temp <- try(estimateLogNormal(), silent = TRUE)
    # Check if bad initial values, if not break
    if (!('try-error' %in% class(temp))){
      break
    } 
  }
  theta1 <- c(temp[1],temp[2])
}

# estimateCCPs
#
# INPUT: data, Dataframe of inventory and sales
# OUTPUT: Dataframe of CCPs corresponding to different (i-s) levels
estimateCCPs<- function(data){
  regData <- data.frame(replace = 1 * (data$q > 0),
                        key = data$i - data$s)
  results <- npreg(replace ~ key, data = regData)
  newdata <- data.frame(key = first(chebknots(dims = dims,intervals = c(0,Q))))
  output <- data.frame(key = newdata$key, 
                       prob_order = predict(results,newdata = newdata))
}

#estimateCostParameters
#
# INPUT: theta1, estimate of demand parameters
#        CCP, initial CCP estimate 
#        data, dataframe of observations
#        conv.err, convergence criterion for CCP estimates
# OUTPUT: theta2, cost parameter estimates

estimateCostParameters <- function(theta1,CCP,data,conv.err = 10^-5){
  g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q), CCP = CCP) %>%
    Vectorize  
  # Estimate of f(i) using current CCP estimates, for use in optimization
  f_hat <- function(theta1, theta2, g.old){
    chebappxf(f,dims = dims,intervals = c(0,Q), theta1 = theta1, theta2 = theta2, g.old =  g.old) %>% 
    Vectorize
  }  
  # Pseudo maximum likelihood
  PMLE <- function(theta2,data){
    log((data$q > 0) * g(data$i - data$s, theta2 = theta2, 
                         f.old = f_hat(theta1 = theta1, theta2 = theta2, g.old =  g.old))  + 
          (data$q == 0) * (1 - g(data$i - data$s, theta2 = theta2, 
                                 f.old = f_hat(theta1 = theta1, theta2 = theta2, g.old =  g.old)))) %>%
      sum
  }
  
  # Iterate getting cost parameter estimates and updating CCP estimates until CCP estimates converge
  delta <- 1 + conv.err
  while(delta > conv.err) {
    results <- try(optim(runif(3), PMLE, 
                     data = data, 
                     method = 'L-BFGS-B', lower = c(0.1,0.1,0.1),
                     control = list(fnscale = -1) # maximization option
    ),silent = TRUE)
    # Check if bad initial values. End optimization and try again if true.
    if ('try-error' %in% class(results)){
      break
    } else {
    theta2 <- results$par
    f.old <- chebappxf(f,dims,c(0,Q),theta1 = theta1, theta2 = theta2, g.old = g.old) %>%
      Vectorize
    g.new <- chebappxf(g,dims,c(0,Q),theta2 = theta2, f.old = f.old) %>%
      Vectorize
    test.vals <- runif(100,min = 0,max = Q)
    delta <- max(abs(g.new(test.vals) - g.old(test.vals)))
    g.old <- g.new
    }
}
theta2
}


# estimateInvModel
#
# INPUT: data, data frame of observations
#        conv.err, convergence criterion for pseudo MLE in estimateCostParameters() (default = 10^-5)   
# OUTPUT: a list containing
#         (1) estimate of mu (demand parameter)
#         (2) estimate of sigma (demand parameter)
#         (3) estimate of pr (cost parameter)
#         (4) estimate of alpha (cost parameter)
#         (5) estimate of eta (cost parameter)

estimateInvModel <- function(data,conv.err = 10^-5){
  theta1 <- estimateDemandParameters(data = data)
  CCP <- estimateCCPs(data = data)
  theta2 <- estimateCostParameters(theta1 = theta1,CCP = CCP,data,conv.err = conv.err)
  list(mu = theta1[1],
       sigma = theta1[2],
       pr = theta2[1],
       alpha = theta2[2],
       eta = theta2[3])
}






