varSave<-'../variables/'
ex.mods<-'modules/'
library('fExtremes')
library('plyr')
library('reshape2')
library('dplyr')
library('doParallel')
library('chebpol')
library('np')
registerDoParallel(cores=20)
l_ply(dir(ex.mods), function(l) source(paste(ex.mods, l, sep="")))