MLE<- function(par, data){
  print(par)
  log((data$i > data$s) * dlnorm(data$s, meanlog = par[1], sdlog = par[2])  + 
        (data$i <= data$s) * (1 - plnorm(data$s, meanlog = par[1], sdlog = par[2]))) %>%
    sum
}

estimateLogNormal<- function(){
  results <- optim(runif(2), MLE, 
                   data = data, 
                   method = 'L-BFGS-B', lower = c(0.01,0.01), 
                   control = list(fnscale = -1) # maximization option
  )
  results$par
}

# Input: Dataframe of inventory and sales
# Output: Dataframe of CCPs corresponding to different (i-s) levels
initialCCPs<- function(data){
  regData <- data.frame(replace = 1 * (data$q > 0),
                        key = data$i - data$s)
  results <- npreg(replace ~ key, data = regData)
  newdata <- data.frame(key = first(chebknots(dims = dims,intervals = c(0,Q))))
  output <- data.frame(key = newdata$key, 
                       prob_order = predict(results,newdata = newdata))
}

#This will mostly be taken out of the function and sourced in a separate file.
estimatePrimitives <- function(){
  
  
#   par <- data.frame(mu = 0,
#                    sigma = 0,
#                    pr = 3,
#                    alpha = .3,
#                    eta = 4)
  theta1 <- rep(0,2)
  
  # Repeat until the optimization doesn't throw an error because of bad initial values
  repeat{
    temp <- try(estimateLogNormal(), silent = TRUE)
    # Check if bad initial values, if not break
    if (!('try-error' %in% class(temp))){
      break
    } 
  }
  theta1[1] <- temp[1]
  theta1[2] <- temp[2]
  CCP <- initialCCPs(data)
  
  g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q), CCP = CCP) %>%
    Vectorize
  
  f_hat <- function(theta1, theta2, g.old){
    chebappxf(f,dims = dims,intervals = c(0,Q), theta1 = theta1, theta2 = theta2, g.old =  g.old) %>% 
    Vectorize
  }
  
  # Pseudo-Maximum Likelihood, doesn't yet work
  PMLE <- function(theta2,data){
    #theta2 =c(theta2[1],theta2[2],4)
    print(theta2)
    log((data$q > 0) * g(data$i - data$s, theta2 = theta2, 
                         f.old = f_hat(theta1 = theta1, theta2 = theta2, g.old =  g.old))  + 
          (data$q == 0) * (1 - g(data$i - data$s, theta2 = theta2, 
                                 f.old = f_hat(theta1 = theta1, theta2 = theta2, g.old =  g.old)))) %>%
      sum
  }
  
  results <- optim(c(3,.3,4), PMLE, 
                   data = data, 
                   method = 'L-BFGS-B', lower = c(0.01,0.01,0.01),
                   control = list(fnscale = -1) # maximization option
  )
  
}









