# simData()
#
# INPUT: S, number of observations
#        rel.err, convergence criterion for getDoubleIntegratedValueFunction()
# OUTPUT: simulated sample of N observations of {i,s,q} (data frame)

simdata <- function(S,rel.err = 10^-3){
#   par<- list(mu = 1,
#              sigma = 1,
#              pr = 3,
#              alpha = .3,
#              eta = 4)
  theta1 <- c(1,1)
  theta2 <- c(3,.3,4)
  
  V <- getDoubleIntegratedValueFunction(theta1 = theta1, theta2 = theta2, rel.err)
  
  data <- data.frame(
    i = c(runif(1,min = 0,max = Q),rep(0,S-1)),
    d = rlnorm(S,meanlog = theta1[1],sdlog = theta1[2])
  ) %>%
    mutate(s = pmin(i,d),
           q = c(rbinom(1,1,V$g(first(i)-first(s))) * (Q - first(i) + first(s)),rep(0,S-1)))
  
  for (j in 2:S){
    data$i[j] <- data$i[j-1] - data$s[j-1] + data$q[j-1]
    data$s[j] <- pmin(data$i[j],data$d[j])
    data$q[j] <- rbinom(1,1,V$g(data$i[j] - data$s[j])) * (Q - data$i[j] + data$s[j])
  }
  
  data %>%
    select(-d)
}