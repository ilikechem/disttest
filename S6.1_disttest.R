########################################
#
# Distribution Test
#
########################################
rm(list  = ls())
library(nloptr)
library(foreign)
library(doSNOW)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/kl/Desktop/disttest")
source('Rfunctions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data       <- read.dta("disttest.dta")
ini        <- c(0,1,0)
ini        <- matrix(ini,nrow=6*6,ncol=length(ini),byrow=TRUE)
ini        <- cbind(ceiling((1:36)/6),1:6,ini)
ftol       <- 1e-6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nsim <- 6 
nc   <- 6
pb <- txtProgressBar(max = nsim, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
cl   <- makeCluster(nc)
ex            <- Filter(function(x) is.function(get(x, .GlobalEnv)), ls(.GlobalEnv))
clusterEvalQ(cl, library("nloptr"))
clusterExport(cl, ex)
registerDoSNOW(cl)
ptm <- proc.time()
est <- foreach(i = 1:nsim, .combine=rbind, .options.snow = opts) %dopar%
  {
    s <- estwrapper(i,data,ini,ftol)
    return(s)
  }
proc.time() - ptm
close(pb)
stopCluster(cl)

rm(list=ls()[! ls() %in% c("ini","est","data")])
save.image("est.RData")
