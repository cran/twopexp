#' Density plot of the two-parameter exponential distribution
#'
#' Density plot of the two-parameter exponential distribution with \code{theta} and \code{beta}
#'
#' @param x     vector of quantile.
#' @param theta location parameter, where \eqn{\theta > 0}.
#' @param beta  scale parameter, where \eqn{\beta > 0}.
#'
#' @return  a density plot of the two-parameter exponential distribution
#' @export
#'
#' @examples
#' x <- seq(0,20,by=0.01)
#' theta <- 6
#' beta <- 2
#' pdfplot(x,theta,beta)
pdfplot <- function(x,theta,beta){
  tta <- theta; bta <- beta
  xx <- x
  xs <- xx[xx>=tta]
  xss <- sort(xs)
  fxs <- dtpexp(xss,theta =tta ,beta = bta)
  plot(xss,fxs,type = "l",xlab="",ylab="")
  legend("top", legend =  expression(f(x) == paste(frac(1, beta)," ",
        e^{frac(-(x - theta), beta)})),cex = 1.2, bty = "n")
  title(main ="The PDF of the two-parameter exponential dist.",xlab="x",ylab="f(x)")
  abline(v=tta,col="red",lty=2)
}

