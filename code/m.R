#################################################################################################
# Aguirregabiria (1999) coding assignment Part 3
# Matt Beamer & Cheng-Yu Chan
# 11/21/2014
#################################################################################################
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
gamma <- -digamma(1)

source('header.R')

# Simulate data
data <- simdata(S = N)
