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
}

# Input: Dataframe of inventory and sales
# Output: Dataframe of CCPs corresponding to different (i-s) levels
initialCCPs<- function(data){
  regData <- data.frame(replace = 1 * (data$q > 0),
                       key = data$i - data$s)
  results <- npreg(replace ~ key, data = regData)
  output <- data.frame(key = results$eval, CCPs = results$mean) %>% 
    unique %>%
    arrange(key) %>%
    select(key,CCPs)
}

