#' The two-parameter exponential distribution(tpexp)
#'
#' @description Density, distribution function, quantile function, and random generation function
#' for the two-parameter exponential distribution with \code{theta} and \code{beta}
#'
#' @param x,q vector of quantile.
#' @param p   vector of probabilities
#' @param n   number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param theta location parameter, where \eqn{\theta > 0}.
#' @param beta  scale parameter, where \eqn{\beta > 0} and \eqn{rate=1/\beta}.
#' @param log,log.p   logical; (default = \code{FALSE}), if \code{TRUE}, then probabilities are given as \code{log(p)}.
#' @param lower.tail  logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
#'
#' @import graphics
#' @import stats
#'
#' @return
#' \code{dtpexp} gives the density,
#' \code{ptpexp} gives the distribution function,
#' \code{qtpexp} gives the quantile function,
#' and \code{rtpexp} generates random samples.
#'
#' @name tpexp
#' @examples
#'
NULL

#' @export
#' @rdname tpexp
#' @examples
#' x <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
#' dtpexp(x,theta=0,beta=1)
#' dtpexp(x,theta=0,beta=1,log=TRUE)
#'
dtpexp <- function(x, theta = 0.0, beta = 1.0, log = FALSE){
  xs <- x[x>=theta]
  fx <- (1/beta)*exp(-(xs-theta)/beta)
  if (log==TRUE)
    return (log(fx))
  else
    return(fx)

}

#' @export
#' @rdname tpexp
#' @examples
#' q <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
#' ptpexp(q,theta = 0, beta = 1)
#' ptpexp(q,theta=0, beta = 1, lower.tail = FALSE)
#'
ptpexp <- function(q, theta = 0.0, beta = 1.0, lower.tail = TRUE, log.p = FALSE){
  xs <- q[q>=theta]
  cdf <- 1-exp(-(xs-theta)/beta)
  if(lower.tail==TRUE)
    p <- cdf
  else
    p <- 1-cdf
  if (log.p==TRUE)
    return (log(p))
  else
    return(p)
}

#' @export
#' @rdname tpexp
#' @examples
#' q <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
#' p<- ptpexp(q,theta = 0, beta = 1); p
#' qtpexp(p,theta=0, beta = 1)
#'
qtpexp <- function(p, theta = 0.0, beta = 1.0, lower.tail = TRUE, log.p = FALSE){
  if (log.p==TRUE)
    p <- exp(p)
  if (lower.tail == FALSE)
    p <- 1 - p
  x <- theta - beta * log(1 - p)
  return(x)
}

#' @export
#' @rdname tpexp
#' @examples
#' rtpexp(5, theta=0, beta=1)
#' rtpexp(10, theta=1, beta=1.5)
#'
rtpexp <- function (n, theta = 0, beta = 1)
{
  if (beta <= 0) {
    stop(paste("beta must be larger than 0!", "\n"))
  }
  #qtpexp(runif(n), theta = theta, beta = beta )
  #x based on the inverse transform method : F(x)=u => x=(F(u))
  # where u=runif(1)
  x <- theta-beta*log(1-runif(n))
  return(x)
}
