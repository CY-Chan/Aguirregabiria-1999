# getDoubleIntegratedValueFunction()
# 
# INPUT: rel.err, the closeness between iterations required for convergence of f, g, default = 10^-3
# OUTPUT: a list containing
#         (1) f(i) for all i in [0,Q]
#         (2) g(i-s) for all (i-s) in [0,Q]
#         (3) V_bar_bar(i) for all i in [0,Q]

getDoubleIntegratedValueFunction <- function(Q, mu, sigma, pr, alpha, eta, rel.err = 10^-3){
    pi_<- function(i, s, q){
      pr*s - pw*q - alpha*(i - s) - eta*(q>0)
    }

    # Initialize CCP
    # Made prob_order 0.5 for all to avoid negative CCPs
    CCP<- data.frame(
      key = chebknots(dims = dims, intervals = c(0,Q))[[1]],
      prob_order = rep(0.5,dims)
    )

    g.init <- function(key){
      CCP$prob_order[CCP$key == key]
    }
    g.old <- chebappxf(g.init,dims = dims,intervals = c(0,Q)) %>%
      Vectorize

    # Relies on g.old, and therefore each time g.old/CCPs are updated, 
    # this is updated when a new chebappxf is called.
    f <- function(i){
      form<- function(i,s){
        (pi_(i,s,Q-i+s) - log(g.old(i-s)))
      }
      # Integrate in two parts. When demand <= i, and when demand >i
      first(integrate(function(s) form(i,s) * dlnorm(s,meanlog = mu,sdlog = sigma),0,i)) +
        first(integrate(function(s) form(i,i) * dlnorm(s,meanlog = mu,sdlog = sigma),i,Inf))
    }
    f.old <- chebappxf(f,dims = dims,intervals = c(0,Q)) 

    g <- function(x){
      1/(1 + exp(pw*(Q- x) + eta + beta*(f.old(x) - f.old(Q))))
    }

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
      f.old(i) + (f(Q) + gamma)/(1 - beta) + gamma
    }
    VdoubleBar <- chebappxf(VbarBar,dims = dims,intervals = c(0,Q))
    
    list(f = f.old,g = g.old,VdoubleBar = VdoubleBar)
}
