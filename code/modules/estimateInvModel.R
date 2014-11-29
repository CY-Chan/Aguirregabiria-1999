# estimateInvModel is a function which estimates parameters mu, sigma, pr, alpha, and eta from data
#
# INPUT: data, data frame of obsevations
# OUTPUT: 

estimateInvModel <- function(data){
  Q <- max(data$i)
  theta <- estimateDemandParameters()$par
  CCP_hat <- estimateCCPs(data)
  
}