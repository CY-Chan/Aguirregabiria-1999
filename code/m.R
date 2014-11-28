#################################################################################################
# Aguirregabiria (1999) coding assignment Part 3
# Matt Beamer & Cheng-Yu Chan
# 11/21/2014
#################################################################################################
source('header.R')
Q = 10
pw = 1
beta = .95
N = 1000
dims = 12
gamma <- -digamma(1)


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



# Simulate data
data <- simdata(S = N)
