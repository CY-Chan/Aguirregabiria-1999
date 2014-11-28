g.init <- function(key, CCP){
  CCP$prob_order[CCP$key == key]
}

# Relies on g.old, and therefore each time g.old/CCPs are updated, 
# this is updated when a new chebappxf is called.
f <- function(i, par, g.old){
  pi_<- function(i, s, q){
    par$pr*s - pw*q - par$alpha*(i - s) - par$eta*(q>0)
  }
  
  form<- function(i,s){
    (pi_(i,s,Q-i+s) - log(g.old(i-s)))
  }
  # Integrate in two parts. When demand <= i, and when demand >i
  first(integrate(function(s) form(i,s) * dlnorm(s,meanlog = par$mu,sdlog = par$sigma),0,i)) + 
    first(integrate(function(s) form(i,i) * dlnorm(s,meanlog = par$mu,sdlog = par$sigma),i,Inf))
}

g <- function(x, par, f.old){
  1/(1 + exp(pw*(Q- x) + par$eta + beta*(f.old(x) - f.old(Q))))
}