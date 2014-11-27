MLE<- function(par, data){
  print(par)
  log((data$i > data$s) * plnorm(data$s, meanlog = par[1], sdlog = par[2])  + 
        (data$i <= data$s) * (1 - plnorm(data$s, meanlog = par[1], sdlog = par[2]))) %>%
    sum
}

estimateLogNormal<- function(){
  results <- optim(c(1,1), MLE, 
                   data = data, 
                   method = 'L-BFGS-B', lower = c(0.01,0.01), 
                   control = list(fnscale = -1) # maximization option
  )
}