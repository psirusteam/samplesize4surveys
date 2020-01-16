#' @import TeachingSampling
#' @export
#' 
#' @title
#' The required sample size for estimating a single proportion
#' @description 
#' This function returns the minimum sample size required for estimating a single proportion subjecto to predefined errors.
#' @details
#' Note that the minimun sample size to achieve a particular margin of error \eqn{\varepsilon} is defined by: 
#' \deqn{n = \frac{n_0}{1+\frac{n_0}{N}}}
#' Where \deqn{n_0=\frac{z^2_{1-\frac{\alpha}{2}}S^2}{\varepsilon^2}}
#' and
#' \deqn{S^2=P(1-P)DEFF}
#' Also note that the minimun sample size to achieve a particular coefficient of variation \eqn{cve} is defined by:
#' \deqn{n = \frac{S^2}{P^2cve^2+\frac{S^2}{N}}} 
#'   
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N The population size.
#' @param P The value of the estimated proportion.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default conf = 0.95. By default \code{conf = 0.95}.
#' @param error The type of error you want to minimize.
#' @param delta The magnitude of the error you want to minimize.
#' @param plot Optionally plot the errors (cve and margin of error) against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{e4p}}
#' @examples 
#' ss4p(N=10000, P=0.05, error = "cve", delta=0.05, DEFF = 1, conf = 0.95, plot=TRUE)
#' ss4p(N=10000, P=0.05, error = "me", delta=0.05, DEFF = 1, conf = 0.95, plot=TRUE)
#' ss4p(N=10000, P=0.5, error = "rme", delta=0.05, DEFF = 1, conf = 0.95, plot=TRUE)
#' 
#' ##########################
#' # Example with Lucy data #
#' ##########################
#' 
#' data(Lucy)
#' attach(Lucy)
#' N <- nrow(Lucy)
#' P <- prop.table(table(SPAM))[1]
#' # The minimum sample size for simple random sampling
#' ss4p(N, P, DEFF=3.45, conf=0.95, error = "cve", delta = 0.03, plot=TRUE)
#' # The minimum sample size for a complex sampling design
#' ss4p(N, P, DEFF=3.45, conf=0.95, error = "rme", delta = 0.03, plot=TRUE)
#' # The minimum sample size for a complex sampling design
#' ss4p(N, P, DEFF=3.45, conf=0.95, error = "me", delta = 0.03, plot=TRUE)


ss4p <- function(N, P, DEFF = 1, conf = 0.95, error = "cve", delta = 0.03, 
                 plot = FALSE) 
{
  S2 = P * (1 - P) * DEFF
  Z = 1 - ((1 - conf)/2)
  
  if (error == "cve"){
    n <- S2/(P^2 * delta^2 + (S2/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      cveseq = 100 * sqrt(varseq)/P
      
      plot(nseq, cveseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Coefficient of variation %", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  if (error == "me"){
    n0 <- (qnorm(Z)^2/delta^2) * S2
    n <- n0/(1 + (n0/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      meseq = 100 * qnorm(Z) * sqrt(varseq)
      
      plot(nseq, meseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Margin of error %", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  if (error == "rme"){
    n0 <- (qnorm(Z)^2/(delta * P)^2) * S2
    n <- n0/(1 + (n0/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      rmeseq = 100 * qnorm(Z) * sqrt(varseq) / P
      
      plot(nseq, rmeseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Relative margin of error %", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  return(ceiling(n))
}
