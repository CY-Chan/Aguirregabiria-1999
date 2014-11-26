Q = 10
mu = 1
sigma = 1
pr = 3
pw = 1
alpha = .3
eta = 4
beta = .95
N = 1000
dims = 12

# rob had bug with iterated chebappx
# appf<- chebappxf(f) %>% vectorize

pi_<- function(i, s, q){
    pr*s - pw*q - alpha*(i - s) - eta*(q>0)
}

CCP<- data.frame(
  key = chebknots(dims, c(0,Q))[[1]],
  prob_order = runif(dims)
)

# Testing chebappxf. Update key to i later and figure out where to get s.
# The problem now is how to pull the correct CCP value out of the table.
f<- function(key){
  CCP$prob_order[CCP$key==key]
}

f_tilda<- chebappxf(f, dims, c(0,Q))
f_tilda(5.65263096)  #When this returns 0.690786510 we know we're using chebappxf correctly.
evalongrid(f_tilda,dims)

