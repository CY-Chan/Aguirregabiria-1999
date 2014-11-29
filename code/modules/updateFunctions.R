g.init <- function(key, CCP){
  CCP$prob_order[CCP$key == key]
}

# Relies on g.old, and therefore each time g.old/CCPs are updated, 
# this is updated when a new chebappxf is called.
f <- function(i, theta1, theta2, g.old){
  pi_<- function(i, s, q){
    theta2[1]*s - pw*q - theta2[2]*(i - s) - theta2[3]*(q>0)
  }
  
  form<- function(i,s){
    (pi_(i,s,Q-i+s) - log(g.old(i-s)))
  }
  # Integrate in two parts. When demand <= i, and when demand >i
  first(integrate(function(s) form(i,s) * dlnorm(s,meanlog = theta1[1],sdlog = theta1[2]),0,i)) + 
    first(integrate(function(s) form(i,i) * dlnorm(s,meanlog = theta1[1],sdlog = theta1[2]),i,Inf))
}

g <- function(x, theta2, f.old){
  1/(1 + exp(pw*(Q- x) + theta2[3] + beta*(f.old(x) - f.old(Q))))
}