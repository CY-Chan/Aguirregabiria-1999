# simData()
#
# INPUT: S, number of observations
#        rel.err, convergence criterion for getDoubleIntegratedValueFunction()
# OUTPUT: simulated sample of N observations of {i,s,q} (data frame)

simdata <- function(S,rel.err = 10^-3){
  
  par<- data.frame(mu = 1,
  sigma = 1,
  pr = 3,
  alpha = .3,
  eta = 4)
  
  # Initialize CCP, g, and f
  # Made prob_order 0.5 for all to avoid negative CCPs
  CCP<- data.frame(
    key = chebknots(dims = dims, intervals = c(0,Q))[[1]],
    prob_order = rep(0.5,dims)
  )
  g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q)) %>%
    Vectorize
  
  f.old <- chebappxf(f,dims = dims,intervals = c(0,Q))
  
  V <- getDoubleIntegratedValueFunction(rel.err)
  
  data <- data.frame(
    i = c(runif(1,min = 0,max = Q),rep(0,N-1)),
    d = rlnorm(N,meanlog = par$mu,sdlog = par$sigma)
  ) %>%
    mutate(s = pmin(i,d),
           q = c(rbinom(1,1,V$g(first(i)-first(s))) * (Q - first(i) + first(s)),rep(0,N-1)))
  
  for (j in 2:N){
    data$i[j] <- data$i[j-1] - data$s[j-1] + data$q[j-1]
    data$s[j] <- pmin(data$i[j],data$d[j])
    data$q[j] <- rbinom(1,1,V$g(data$i[j] - data$s[j])) * (Q - data$i[j] + data$s[j])
  }
  
  data %>%
    select(-d)
}


