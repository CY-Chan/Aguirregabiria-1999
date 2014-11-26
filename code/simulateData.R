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

# Made prob_order 0.5 for all to avoid negative CCPs in g_tilda (line 28)
CCP<- data.frame(
  key = chebknots(dims, c(0,Q))[[1]],
  prob_order = rep(0.5,dims)
)

g <- function(key){
  CCP$prob_order[CCP$key == key]
}
g_tilda <- chebappxf(g,dims,c(0,Q)) 

# Unsure of the integration here, since s = min(i,d)
f <- function(i){
  integrate(function(s) (pr*s - pw*(Q-i+s) - alpha*(i-s) - eta - g_tilda(i-s)) * 
              (1 - plnorm(s,meanlog = mu,sdlog = sigma)),0,i)  
}
# 
# # Testing chebappxf. Update key to i later and figure out where to get s.
# f<- function(key){
#   CCP$prob_order[CCP$key==key]
# }

# This doesn't work correctly, but I can calculate f at different values. The error says 
# 'fun must return a real value'
f_tilda<- chebappxf(f, dims, c(0,Q))

# f_tilda(5.65263096)  #When this returns the corresponding CCP we know we're using chebappxf correctly.
# CCP$prob_order[6] #Corresponding CCP
# f_tilda(5.7) #Especially when this is also giving a close result!
# evalongrid(f_tilda,grid = expand.grid(chebknots(dims,c(0,Q)))) #And this returns the exact corresponding values.


