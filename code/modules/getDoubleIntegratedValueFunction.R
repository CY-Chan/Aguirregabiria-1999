# getDoubleIntegratedValueFunction()
# 
# INPUT: rel.err, the closeness between iterations required for convergence of f, g, default = 10^-3
# OUTPUT: a list containing
#         (1) f(i) for all i in [0,Q]
#         (2) g(i-s) for all (i-s) in [0,Q]
#         (3) V_bar_bar(i) for all i in [0,Q]

getDoubleIntegratedValueFunction <- function(rel.err = 10^-3){
    delta <- 1 + rel.err
    # Run until both f and CCPs converge, testing by comparing 100 different random points each time.
    while (delta > rel.err){
      # When calling chebappxf here, it is essentially updating g taking into account the updated f, 
      # and updating f taking into account the updated g.
      g.new <- chebappxf(g,dims,c(0,Q)) %>%
        Vectorize
      f.new <- chebappxf(f,dims,c(0,Q))
      
      test.vals <- runif(100,min = 0,max = Q)
      delta.f <- max(abs(sapply(test.vals,f.new) - sapply(test.vals,f.old)))
      delta.g <- max(abs(sapply(test.vals,g.new) - sapply(test.vals,g.old)))
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
