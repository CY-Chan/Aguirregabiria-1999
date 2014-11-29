# estimateInvModel
#
# INPUT: data, data frame of observations
#        conv.err, convergence criterion for pseudo MLE in estimateCostParameters() (default = 10^-3)   
# OUTPUT: a list containing
#         (1) theta1, estimates of mu, sigma (demand parameters)
#         (2) theta2, estimates of pr, alpha, eta (cost parameters)

estimateInvModel <- function(data,conv.err = 10^-5){
  theta1 <- estimateDemandParameters(data = data)
  CCP <- estimateCCPs(data = data)
  theta2 <- estimateCostParameters(theta1 = theta1,CCP = CCP,data,conv.err = conv.err)
  list(theta1 = theta1, theta2 = theta2)
}