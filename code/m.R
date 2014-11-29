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

# Simulate data
data <- simdata(S = N)