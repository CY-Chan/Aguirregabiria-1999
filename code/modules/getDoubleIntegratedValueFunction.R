# getDoubleIntegratedValueFunction()
# 
# INPUT: rel.err, the closeness between iterations required for convergence of f, g, default = 10^-3
# OUTPUT: a list containing
#         (1) f(i) for all i in [0,Q]
#         (2) g(i-s) for all (i-s) in [0,Q]
#         (3) V_bar_bar(i) for all i in [0,Q]

getDoubleIntegratedValueFunction <- function(theta1,theta2,rel.err = 10^-3){
  
  # Initialize CCP, g, and f
  # Made prob_order 0.5 for all to avoid negative CCPs
  CCP<- data.frame(
    key = chebknots(dims = dims, intervals = c(0,Q))[[1]],
    prob_order = rep(0.5,dims)
  )
  
  g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q), CCP = CCP) %>%
    Vectorize
  
  f.old <- chebappxf(f,dims = dims,intervals = c(0,Q), theta1 = theta1, theta2 = theta2, g.old = g.old) %>% 
    Vectorize
  
  delta <- 1 + rel.err
  # Run until both f and CCPs converge, testing by comparing 100 different random points each time.
  while (delta > rel.err){
    # When calling chebappxf here, it is essentially updating g taking into account the updated f, 
    # and updating f taking into account the updated g.
    g.new <- chebappxf(g,dims,c(0,Q), theta2 = theta2, f.old = f.old) %>%
      Vectorize
    f.new <- chebappxf(f,dims,c(0,Q), theta1 = theta1, theta2 = theta2, g.old = g.old) %>%
      Vectorize
    
    test.vals <- runif(100,min = 0,max = Q)
    delta.f <- max(abs(f.new(test.vals) - f.old(test.vals)))
    delta.g <- max(abs(g.new(test.vals) - g.old(test.vals)))
    delta <- max(delta.f,delta.g)
    
    g.old <- g.new
    f.old <- f.new
  }
  
  VbarBar <- function(i){
    f.old(i) + (f.old(Q) + gamma)/(1 - beta) + gamma
  }
  VdoubleBar <- chebappxf(VbarBar,dims = dims,intervals = c(0,Q))
  
  list(f = f.old,g = g.old,VdoubleBar = VdoubleBar)
}