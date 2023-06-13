#' Median rank method to estimate parameters of the two-parameter exponential dist.
#'
#' Median rank method to estimate parameters of the two-parameter exponential dist.
#'
#' @param x        vector of quantile (or a data set).
#' @param methods  there are some of median rank methods as follows;
#'                 "B"  stand for Benard median rank method (default),
#'                 "BL" stand for Blom method,
#'                 "MKM" stand for Hazen (Modified Kaplan Meier) method,
#'                 "OT" stand for The one-third method, and
#'                 "C"  stand for Cunane method
#'
#' @return the estimate three values for the two-parameter exponential dist. as follows:
#' \code{theta.hat}  gives the estimate location parameter,
#' \code{beta.hat}   gives the estimate scale parameter,
#' and \code{lamda.hat} gives the estimate the rate.
#'
#' @source Reid, M. (2022). Reliability – a Python library for reliability engineering (Version 0.8.2) [Computer software].
#' Zenodo. \doi{https://doi.org/10.5281/ZENODO.3938000}.
#'
#' @export
#' @examples
#' x1 <- c(25,43,53,65,76,86,95,115,132,150) # test a data set
#' medrank(x1,"B")    # Benard method (default) or medrank(x1)
#'
medrank <- function(x,methods=c("B")){
  n <- length(x)
  t <- sort(x)
  i<- rank(t)
  # mdrank = (i-a)/(n+1-2*(a)) , where a = the heuristics constants, b=1-2*(a)
  if(methods=="B")  # a=0.3, Benard (median)  equal to (i-0.3)/(n+0.4)
    mdrank <- (i-0.3)/(n+1-2*(0.3))
  else if(methods=="BL" )  # a= 0.375  Blom method
          mdrank <- (i-0.375)/(n+1-2*(0.375))
       else if(methods=="MKM")       # a=0.5 Hazen (Modified Kaplan Meier)
               mdrank <- (i-0.5)/(n+1-2*(0.5))
             else if(methods=="OT")   # a=1/3 The one-third method
                     mdrank <- (i-1/3)/(n+1-2*(1/3))
                  else if(methods=="C")   # a=0.4 Cunane method
                         mdrank <- (i-0.4)/(n+1-2*(0.4))
                       else{ warning("Error in the specified methods") }
  yt <- log(1-mdrank)
  tsq <- t^2
  ytsq <- yt^2
  tyt <- t*yt
  dataf111 <- data.frame(rank=i,t=t,Ft.est=mdrank,y=yt,tsq=tsq,ysq=ytsq,ty=tyt)
  f1 <- sum(dataf111$ty)-(sum(dataf111$t)*sum(dataf111$y)/n)
  f2 <- sum(dataf111$tsq)-(sum(dataf111$t)^2/n)
  b <- f1/f2
  a <- mean(dataf111$y)-b*mean(dataf111$t)
  lamda.hat <- -b
  theta.hat <- a/lamda.hat
  if(theta.hat<0.0) theta.hat <- 0.0
  beta.hat <- 1/lamda.hat
  #fx.est = lamda.hat*exp{-lamda.hat(x-theta.hat)}
  #or fx.est=(1/beta.hat)*exp{-(x-theta.hat)/beta.hat}
  cat("Theta.est=",theta.hat,"Beta.est=",beta.hat,"Rate.est=",lamda.hat, "\n ")
  output<- list( dataf=dataf111,para.est=c(theta.hat,beta.hat,lamda.hat) )
  return(output[2])
}


