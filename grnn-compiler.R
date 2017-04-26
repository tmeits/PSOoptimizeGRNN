# Renjin is a JVM-based interpreter for the R language for statistical computing. http://www.renjin.org/
# http://www.inp.nsk.su/~baldin/DataAnalysis/R/R-10-hpc.pdf
# http://www.tandfonline.com/doi/pdf/10.1623/hysj.51.6.1092
# https://arxiv.org/pdf/1503.00855.pdf
# http://dirk.eddelbuettel.com/papers/ismNov2009introHPCwithR.pdf
# https://www.r-bloggers.com/deploying-desktop-apps-with-r/
# http://r.psylab.info/blog/2015/05/09/code-profiling/
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
#https://minds.wisconsin.edu/handle/1793/7779
# http://easyneuralnetwork.blogspot.ru/2013/07/grnn-generalized-regression-neural.html
# https://www.researchgate.net/publication/282640453_A_fast_progressive_Local_Learning_regression_ensemble_of_Generalized_Regression_Neural_Networks
# http://www.flyfoa.com/2014/10/matlab-code-for-foagrnn-in-past.html
# file:///C:/Users/IVA/Downloads/B1003.pdf
#http://www.mathworks.com/matlabcentral/fileexchange/11559-particle-swarm-optimization-simulation
#rm(list=ls())
#http://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2292&context=oa_dissertations
require(compiler)
enableJIT(3)

ds <- function(Xa, X) {
  value <- (X - Xa) %*% t(X - Xa)
  return(as.numeric(value))
}

pattern <- function(Xa, X, sigma) {
  res <- exp( - ds(Xa, X) / (2 * sigma ^ 2) )
  return(as.numeric(res))
}

patterns <- function(Xa, X, sigma)
  apply(Xa, 1, pattern, X, sigma)

K <- function(Xa, Ya, X, sigma) {
  patterns1 <- patterns(Xa, X, sigma)
  f <- sum(Ya * patterns1) / sum(patterns1)
  return(f)
}

sim <- function(Xa, Ya, Ga, sigma){
  len <- length(Ga[,1])
  res <- as.matrix(1:len)
  for(i in 1:len) {
    res[i] <- K(Xa, Ya, Ga[i,], sigma)
  }
  return(res)
}

# 
activator <- function(data, train_x, sigma) {
  distance <- 0.
  if (!is.vector(data)) stop("data not vector")
  if (!is.vector(train_x)) stop("train_x not vector")
  for (i in 1:length(data)){
    distance <- distance + (data[i] - train_x[i]) ^ 2
  }
  return(exp( - distance / (sigma ^ 2)))
}

grnn <- function(data, train_x, train_y, sigma) {
  out_dim <- ncol(train_y)
  result <- 1:out_dim
  for (dim in 1:out_dim) {
    factor <- 0.; divide <- 0;
    for (i in 1:nrow(train_x)) {
      cache <- activator(data, train_x[i, ], sigma)
      factor <- factor + train_y[i, dim] * cache
      divide <- divide + cache
    }
    result[dim] <- (factor/divide)
  }
  return(result)
}
  
grnn.sim <- function(Ga, Xa, Ya, sigma) {
  len <- nrow(Ga)
  cat("grnn.sim", len, "\n")
  res <- as.matrix(1:len)
  for(i in 1:len) {
    res[i] <- grnn(Ga[i, ], Xa, Ya, sigma)
  }
  return(res)
}

grrn.pso <-function(Xa, Ya) {
  
}

if (FALSE) {
  train_x <-   matrix(c(3,5,  3,11,  8,6,  0,34,  13,3,  2,17,  23,2,  37,1,  1,40,  21,30,  30,24,  24,64,  43,46,  31,51), ncol=2, byrow = TRUE)
  train_y <-   as.matrix(c(-10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  0,  0,  10,  10,  10))
  
  y.sim <- grnn.sim(train_x[4:9,], train_x, train_y, 14.7)
  plot(train_y, col="blue")
  points(y.sim, col="red")
}

if (FALSE) {
  n <- 900; set.seed(123456); sigma <- 0.5
  x <- as.matrix(runif(n, -2, 2))
  y <- as.matrix(x^3 + rnorm(n, 0, .1))
  plot(x,y, col="blue")
  #x.sample <- as.matrix(sample(x, 35))
  x.sample <- x
  y.sim <- sim(x, y, x.sample, sigma)
  points(x.sample, y.sim, col="red")
  y.sim <- grnn.sim(x, x.sample, y, sigma)
  points(x.sample, y.sim, col="green")
  
  system.time(sim(x, y, x.sample, sigma)) 
  system.time(grnn.sim(x, x.sample,y, sigma))
  #пользователь      система       прошло 
  #18.98             0.00          18.99
}
if (FALSE) {
  train_x <-   matrix(c(3,5,  3,11,  8,6,  0,34,  13,3,  2,17,  23,2,  37,1,  1,40,  21,30,  30,24,  24,64,  43,46,  31,51), ncol=2, byrow = TRUE)
  train_y <-   as.matrix(c(-10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  0,  0,  10,  10,  10))
  train_y <-   as.vector(c(-10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  0,  0,  10,  10,  10))
  y.sim <- sim(train_x/20., train_y/20., train_x/20., 0.1)
  #y.sim <- grnn.sim(train_x/20.,  train_x/20., train_y/20., 0.0001)
  plot(train_y, col="blue")
  points(y.sim*20, col="red")
}



