#' Survival function plot of the two-parameter exponential distribution
#'
#' Survival function plot of the two-parameter exponential distribution with \code{theta} and \code{beta}
#'
#' @param x     vector of quantile.
#' @param theta location parameter, where \eqn{\theta > 0}.
#' @param beta  scale parameter, where \eqn{\beta > 0}.
#'
#' @return  a survival function plot of the two-parameter exponential distribution
#' @export
#'
#' @examples
#' x <- seq(0,20,by=0.01)
#' theta <- 8
#' beta <- 1
#' surplot(x,theta,beta)
surplot <- function(x,theta,beta){
  tta <- theta; bta <- beta
  xx <- x
  xs <- xx[xx>=tta]
  xss <- sort(xs)
  surxs <- 1-ptpexp(xss,theta = tta ,beta = bta)
  plot(xss,surxs,type = "l",xlab="",ylab="")
  legend("top",legend =  expression(S(x) == paste(e^{frac(-(x - theta), beta)})),cex = 1.2, bty="n")
  title(main ="A survival function plot of the two-parameter exponential dist.",xlab="x",ylab="S(x)")
}
