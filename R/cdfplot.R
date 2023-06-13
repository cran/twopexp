#' Distribution function plot of the two-parameter exponential distribution
#'
#' Distribution function plot of the two-parameter exponential distribution with \code{theta} and \code{beta}
#'
#' @param x     vector of quantile.
#' @param theta location parameter, where \eqn{\theta > 0}.
#' @param beta  scale parameter, where \eqn{\beta > 0}.
#'
#' @return  a distribution function plot of the two-parameter exponential distribution
#' @export
#'
#' @examples
#' x <- seq(0,20,by=0.01)
#' theta <- 6
#' beta <- 2
#' cdfplot(x,theta,beta)
cdfplot <- function(x,theta,beta){
  tta <- theta; bta <- beta
  xx <- x
  xs <- xx[xx>=tta]
  xss <- sort(xs)
  cdfxs <- ptpexp(xss,theta = tta ,beta = bta)
  plot(xss,cdfxs,type = "l",xlab="",ylab="")
  legend("top", legend =  expression(F(x) == paste(1-e^{frac(-(x - theta), beta)})),cex = 1.2, bty="n")
  title(main ="The CDF of the two-parameter exponential dist.",xlab="x",ylab="F(x)")
  abline(h=1,col="red",lty=2)
}
