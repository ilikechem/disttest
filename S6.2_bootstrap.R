########################################
#
# Distribution Test Bootstrap
#
########################################
rm(list  = ls())
library(nloptr)
library(foreign)
library(dplyr)
library(foreach)
library(doSNOW)
#library(doRNG)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("/home/138/kl6140/disttest")
source('Rfunctions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("est.Rdata")
n    <- 1000
nsim <- n*6
nc   <- 12
ftol <- 1e-6

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Standard Errors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl   <- makeCluster(nc)
ex            <- Filter(function(x) is.function(get(x, .GlobalEnv)), ls(.GlobalEnv))
clusterEvalQ(cl, library("nloptr"))
clusterExport(cl, ex)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nsim, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

ptm <- proc.time()
bootest <- foreach(i = 1:nsim, .combine = rbind, .options.snow = opts) %dopar%
  { 
    s <- bootwrapper(i,data,est,ftol)
    return(s)
  }

close(pb)
stopCluster(cl)
proc.time() - ptm

#rm(list=ls()[! ls() %in% c("ini","est","bootest","data")])
#write.dta(as.data.frame(est),"est.dta")
#write.dta(as.data.frame(bootest),"bootest.dta")
save.image("estboot.RData")
