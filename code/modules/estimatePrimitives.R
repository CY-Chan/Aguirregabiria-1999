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
  data.frame(mu = results$par[1], sigma = results$par[2])
  
}

# Input: Dataframe of inventory and sales
# Output: Dataframe of CCPs corresponding to different (i-s) levels
initialCCPs<- function(data){
  regData <- data.frame(replace = 1 * (data$q > 0),
                       key = data$i - data$s)
  results <- npreg(replace ~ key, data = regData)
  output <- data.frame(key = results$eval, CCP = results$mean) %>% 
    unique %>%
    arrange(key) %>%
    select(key,CCP)
}

#This will mostly be taken out of the function and sourced in a separate file.
estimatePrimitives<- function(){
  
  
  par<- data.frame(mu = 0,
                   sigma = 0,
                   pr = 0,
                   alpha = 0,
                   eta = 0)
  
  # This is an extremely ugly loop. Need to fix it.
  while(1 ==1){
    temp <- try(estimateLogNormal(), silent = TRUE)
    
    # Check if bad initial values. End EM operator and return -Inf if true.
    if (!('try-error' %in% class(temp))){
      break
    } 
  }
  par$mu<- temp$mu
  par$sigma<- temp$sigma
  CCP<- initialCCPs(data)
  
  g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q), CCP = CCP) %>%
    Vectorize
  
  f.old <- chebappxf(f,dims = dims,intervals = c(0,Q), par = par, g.old =  g.old) %>% 
    Vectorize
  
  g.old <- chebappxf(f,dims = dims,intervals = c(0,Q), par = par, f.old = f.old) %>% 
    Vectorize
}
