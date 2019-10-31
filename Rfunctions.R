########################################
#
# Relevant Functions
#
########################################
rs <- function (s, u) {
  # r(s)
  max(0,(-s/(1-s)))+(1-max(0,(-s/(1-s))))*u
}

rst <- function (s, u) {
  # r(s)_tilde
  max(0,s)+ (1-max(0,s))*u
}

lambda <- function (data, u) {
  # data is a vector of log-productivities of large/small cities (sorted in from small to large)
  result <- quantile(data, probs = u,na.rm = T, type=8)
  return(result)
}

#alternatively, one can change the lambda from default quantile to an approximation by linear interpolation
lambda1 <- function (data, u){
  result = apply(matrix(u, ncol = 1), 1, function(x) lambda1(data = data, u = x))
  result
}

lambda2 <- function (data,  u) {
  # data is a vector of log-productivities of large/small cities (sorted in from small to large)
  # u is the rank, u is between [0,1]
  E <- length(data)
  # E is total number of log-productivities
  k <- u*(E-1)
  kstar <- floor(k)
  #below is to use linear interpolation to get lambda(u)_hat
  if (u == 0){
    result <- data[1]
  } else if (u < 1){
    result <- (kstar + 1 - k) * data[kstar+1] + (k-kstar) * data[(kstar+2)]
  } else{
    result <- data[length(data)]
  }
  # result <- quantile(data, probs = u,na.rm = T, type=3)
  return(result)
}

m_hat <- function (theta, u, datai, dataj) {
  # this function compute the empirical counterparts of m(theta)
  # theta is a vector contains A, D and S
  lambda(datai, rs(theta[3], u)) - theta[2] * lambda(dataj, (theta[3]+(1-theta[3])*rs(theta[3], u))) - theta[1]
}

mtilde_hat <- function (theta, u, datai, dataj) {
  # this function compute the empirical counterparts of m_tilde(theta)
  # theta is a vector contains A, D and S
  lambda(dataj, rst(theta[3], u)) - 1/theta[2] * lambda(datai, ((rst(theta[3], u)-theta[3])/(1-theta[3]))) + theta[1]/theta[2]
}

object <- function (theta=theta, datai=datai, dataj=dataj) {
  #object <- function (theta=theta, datai=datai, dataj=dataj, x.digits = 8) {
  # this function compute the approximation of the objective function to minimize
  # theta is a vector contains A, D and S
  # Check if any of opt is not NA
  # if (any(!sapply( fixed, is.na))) {
  #   i = !sapply( fixed, is.na)
  #   # Fix non-NA values
  #   theta[i] <-  fixed[i]
  # }
  #m_hat_sq <- rep(0,1001)
  #for (i in 1:1001){
  
  #  u <- 0.001*(i-1)
  # m_hat_sq[i] <- m_hat(theta, u, datai, dataj)^2
  
  #}
  theta = round(theta, digits = 8)
  uu <- seq(0,1,by=0.001)
  
  
  
  m_hat_sq = m_hat(theta, uu, datai, dataj)^2
  #print(length(m_hat_sq[-1]))
  # if(length(m_hat_sq) == 1000){
  #   print(theta)
  #   
  # }
  # mtilde_hat_sq <- NULL
  # for(i in 1:1001){
  #   u <- 0.001*(i-1)
  #   mtilde_hat_sq <- c(mtilde_hat_sq, mtilde_hat(theta, u, datai, dataj)^2)
  # }
  mtilde_hat_sq = mtilde_hat(theta, uu, datai, dataj)^2
  
  
  integral_m_hat <- 0.5*sum((m_hat_sq[-1]+m_hat_sq[-1001])*0.001)
  
  integral_mtilde_hat <- 0.5*sum((mtilde_hat_sq[-1]+mtilde_hat_sq[-1001])*0.001)
  
  integral_m_hat+integral_mtilde_hat
}

dataprocess <- function(i,data,s) {
  s = s[s[,1]==i,]
  if (i==1) {
    # Promotion+All
    data    <- data[!is.na(data$promotion),]
    datai   <- data[data$promotion==2,1]
    dataj   <- data[data$promotion==1,1]
  } else if (i==2) {
    # Promotion+Entrant
    data    <- data[!is.na(data$promotion),]
    data    <- data[data$entrant==1,]
    datai   <- data[data$promotion==2,1]
    dataj   <- data[data$promotion==1,1]
  } else if (i==3) {
    # Promotion+Incumbent
    data    <- data[!is.na(data$promotion),]
    data    <- data[data$entrant==0,]
    datai   <- data[data$promotion==2,1]
    dataj   <- data[data$promotion==1,1]
  } else if (i==4) {
    # ETR+All
    data    <- data[!is.na(data$egroup),]
    datai   <- data[data$egroup==2,1]
    dataj   <- data[data$egroup==1,1]
  } else if (i==5) {
    # ETR+Entrant
    data    <- data[!is.na(data$egroup),]
    data    <- data[data$entrant==1,]
    datai   <- data[data$egroup==2,1]
    dataj   <- data[data$egroup==1,1]
  } else {
    # ETR+Incumbent
    data    <- data[!is.na(data$egroup),]
    data    <- data[data$entrant==0,]
    datai   <- data[data$egroup==2,1]
    dataj   <- data[data$egroup==1,1]
  }
  datai     <- datai[(datai<quantile(datai,0.99,type=8) & datai>quantile(datai,0.01,type=8))]
  dataj     <- dataj[(dataj<quantile(dataj,0.99,type=8) & dataj>quantile(dataj,0.01,type=8))]
  datai     <- as.data.frame(sort(datai))
  dataj     <- as.data.frame(sort(dataj))
  out       <- list()
  out$datai <- datai
  out$dataj <- dataj
  out$s     <- s
  return(out)
}

resfun <- function(opt,datai,dataj,benchmark) {
  #r2
  r2 <- 1-object(opt$solution,datai=datai, dataj=dataj)/benchmark
  #size j
  #s = opt$solution[3]
  #if (s> 0) {
  #  uu <- seq(0,1,by=0.001)
  #  obsj <- max(dim(dataj[dataj >= quantile(dataj, probs = rst(s,uu)[1],na.rm = T, type=8)]))
  #} else {
  #  obsj <- max(dim(dataj))
  #}
  #size i
  #s = opt$solution[3]
  #if (s< 0) {
  #  uu <- seq(0,1,by=0.001)
  #  obsi <- max(dim(datai[datai >= quantile(datai, probs = rs(s,uu)[1],na.rm = T, type=8)]))
  #} else {
  #  obsi <- max(dim(datai))
  #}
  #obs <-  max(dim(datai)) + max(dim(dataj))
  obs <- 1
  out <- cbind(opt$solution[1],opt$solution[2],opt$solution[3],r2,opt$objective,obs)
  return(out)
}

estfun <- function(data,ftol) {
  datai   <- data$datai
  dataj   <- data$dataj
  s       <- data$s
  opts    <- list("algorithm"="NLOPT_GN_DIRECT_L_RAND_NOSCAL", "ftol_rel"=ftol, "maxeval"=10000)
  # Benchmark
  benchmark <- object(c(0,1,0),datai=datai, dataj=dataj)
  #case 1 all fixed
  opt1    <- nloptr(x0 = s[1,3:5], eval_f=object ,lb=c(0,1,0), ub=c(0, 1, 0), opts = opts, datai=datai, dataj=dataj)
  #case 2 all all changable
  opt2    <- nloptr(x0 = s[2,3:5], eval_f=object ,lb=c(0,0,-2), ub=c(2, 2, 0.99), opts = opts, datai=datai, dataj=dataj)
  #case 3 D=1
  opt3    <- nloptr(x0 = s[3,3:5], eval_f=object ,lb=c(0,1,-2), ub=c(2, 1, 0.99), opts = opts, datai=datai, dataj=dataj)
  #case 4 D=1 S=0
  opt4    <- nloptr(x0 = s[4,3:5], eval_f=object ,lb=c(0,1,0), ub=c(2, 1, 0), opts = opts, datai=datai, dataj=dataj)
  #case 5 A=0 D=1 
  opt5    <- nloptr(x0 = s[5,3:5], eval_f=object ,lb=c(0,1,-2), ub=c(0, 1, 0.99), opts = opts, datai=datai, dataj=dataj)
  #case 6 S=0
  opt6    <- nloptr(x0 = s[6,3:5], eval_f=object ,lb=c(0,0,0), ub=c(2, 2, 0), opts = opts, datai=datai, dataj=dataj)
  out     <- rbind(cbind(1,resfun(opt1,datai,dataj,benchmark)),
                   cbind(2,resfun(opt2,datai,dataj,benchmark)),
                   cbind(3,resfun(opt3,datai,dataj,benchmark)),
                   cbind(4,resfun(opt4,datai,dataj,benchmark)),
                   cbind(5,resfun(opt5,datai,dataj,benchmark)),
                   cbind(6,resfun(opt6,datai,dataj,benchmark)))
  return(out)
}

estfun2 <- function(data,ftol) {
  datai   <- data$datai
  dataj   <- data$dataj
  s       <- data$s
  opts    <- list("algorithm"="NLOPT_GN_DIRECT_L_RAND_NOSCAL", "ftol_rel"=ftol, "maxeval"=10000)
  # Benchmark
  benchmark <- object(c(0,1,0),datai=datai, dataj=dataj)
  #case 2 all all changable
  opt     <- nloptr(x0 = s[2,3:5], eval_f=object ,lb=c(0,0,-2), ub=c(2, 2, 0.99), opts = opts, datai=datai, dataj=dataj)
  out     <- resfun(opt,datai,dataj,benchmark)
  return(out)
}

estwrapper <- function (i,data,s,ftol) {
  data    <- dataprocess(i,data,s)
  out     <- estfun(data,ftol)
  out     <- cbind(i,out)
  return(out)
}

bootwrapper <- function (i,data,s,ftol) {
  ii           <- ceiling(i/6) 
  ii           <- 6 + i - ii*6
  set.seed(i)
  data         <- dataprocess(ii,data,s)
  i.boot <- sample(1:dim(data$datai)[1],dim(data$datai)[1], replace = TRUE)
  j.boot <- sample(1:dim(data$dataj)[1],dim(data$dataj)[1], replace = TRUE)
  data$datai <- data$datai[i.boot,]
  data$dataj <- data$dataj[j.boot,]
  #data$datai   <- sample_n(data$datai,max(dim(data$dataj)), replace = TRUE)
  #data$dataj   <- sample_n(data$dataj,max(dim(data$dataj)), replace = TRUE)
  out          <- estfun(data,ftol)
  out          <- cbind(i,out)
  return(out)
}
