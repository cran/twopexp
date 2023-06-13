#' Quartile method estimation of the two-parameter exponential distribution
#'
#' To estimate the location (or shift) and scale parameters for the two-parameter exponential distribution
#' based on quartile method. See detail in \code{source}
#'
#' @param x         vector of quantile (or a data set).
#' @param methods   there are two quartile methods as follows;
#'                  "Q13" stand for the first and the third quartile method (default), and
#'                  "Q12" stand for the first and the second quartile (median) method.
#'
#' @return the estimate three values for the two-parameter exponential dist. as follows:
#' \code{qmtheta.hat}  gives the estimate location parameter,
#' \code{qmbeta.hat}   gives the estimate scale parameter,
#' and \code{qmlamda.hat} gives the estimate the rate.
#'
#' @source  Elgmati, E., Gregni, N. (2016). Quartile Method Estimation of Two-Parameter Exponential Distribution Data with Outliers.
#' \emph{International Journal of Statistics and Probability}, \emph{5(5)}, 12-15.
#' \doi{http://dx.doi.org/10.5539/ijsp.v5n5p12}
#'
#' @import stats
#' @export
#'
#' @examples
#' x1 <- c(25,43,53,65,76,86,95,115,132,150) # test a data set
#' qm_tpexp(x1,"Q13")  # or qm_tpexp(x1)
#' qm_tpexp(x1,"Q12")
#'
qm_tpexp<- function(x,methods=c("Q13")){
  xs <- sort(x)
  n <- length(xs)
  Q1 <-quantile(xs)[[2]]   #Q1
  Q2 <-quantile(xs)[[3]]   #Q2=Median
  Q3 <- quantile(xs)[[4]]  #Q3
  if(methods=="Q13"){
    qmbeta.hat <- (Q3-Q1)/log(3)
    qmtheta.hat <- Q3-qmbeta.hat*log(4)
  }else if(methods=="Q12"){
    qmbeta.hat <- (Q3-Q2)/log(2)
    qmtheta.hat <- 2*Q2-Q3
  }else{
    warning("Error in the specified methods")
  }
  qmlamda.hat <- 1/qmbeta.hat
  cat("Theta.est=",qmtheta.hat,"Beta.est=",qmbeta.hat,"Rate.est=",qmlamda.hat, "\n ")
  output<- list( datax=xs,para.est=c(qmtheta.hat,qmbeta.hat,qmlamda.hat) )
  return(output[2])
}


