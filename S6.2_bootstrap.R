########################################
#
# Distribution Test Bootstrap
#
########################################
rm(list  = ls())
library(nloptr)
library(foreign)
library(foreach)
library(doSNOW)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
os       = Sys.info()['sysname']
release  = Sys.info()['release']
user     = Sys.getenv("USERNAME")

if (os == "Darwin") {
  setwd("~/Dropbox/Working/entry/data/created/disttest")
  source('../../../Code/Rfunctions.R')
} else if (os == "Linux") {
  setwd("/home/138/kl6140/disttest")
  source('Rfunctions.R')
} else {
  setwd("D:/Dropbox/Working/entry/data/created/disttest")
  source('../../../Code/Rfunctions.R')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("est.Rdata")
n    <- 20
nc   <- 16
ftol <- 1e-6

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read Input
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
args    <- commandArgs()
args    <- as.numeric(args[4])
minn    <- (args - 1)*n + 1
maxn    <- (args - 1)*n + n
minnsim <- (minn-1)*6 + 1
maxnsim <- maxn*6

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
bootest <- foreach(i = minnsim:maxnsim, .combine = rbind, .options.snow = opts) %dopar%
  { 
    s <- bootwrapper(i,data,est,ftol)
    return(s)
  }

close(pb)
stopCluster(cl)
proc.time() - ptm

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Output File
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- paste("estboot",args,".RData",sep="")
save.image(outfile)
