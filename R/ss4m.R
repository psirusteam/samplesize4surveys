#' @import TeachingSampling
#' @export
#' 
#' @title
#' The required sample size for estimating a single mean
#' @description 
#' This function returns the minimum sample size required for estimating a single mean subjec to predefined errors.
#' @details
#' Note that the minimun sample size to achieve a relative margin of error \eqn{\varepsilon} is defined by: 
#' \deqn{n = \frac{n_0}{1+\frac{n_0}{N}}}
#' Where \deqn{n_0=\frac{z^2_{1-\frac{alpha}{2}}S^2}{\varepsilon^2 \mu^2}}
#' and
#' \deqn{S^2=\sigma^2 DEFF}
#' Also note that the minimun sample size to achieve a coefficient of variation \eqn{cve} is defined by:
#' \deqn{n = \frac{S^2}{\bar{y}_U^2 cve^2 + \frac{S^2}{N}}} 
#'   
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at gmail.com>
#' @param N The population size.
#' @param mu The value of the estimated mean of a variable of interest.
#' @param sigma The value of the estimated standard deviation of a variable of interest.
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
#' ss4m(N=10000, mu=10, sigma=2, DEFF = 2, error = "cve", delta = 0.03, plot=TRUE)
#' ss4m(N=10000, mu=10, sigma=2, DEFF = 2, error = "me", delta = 1, plot=TRUE)
#' ss4m(N=10000, mu=10, sigma=2, DEFF = 2, error = "rme", delta = 0.03, plot=TRUE)
#' 
#' ##########################
#' # Example with Lucy data #
#' ##########################
#' 
#' data(Lucy)
#' attach(Lucy)
#' N <- nrow(Lucy)
#' mu <- mean(Income)
#' sigma <- sd(Income)
#' # The minimum sample size for simple random sampling
#' ss4m(N, mu, sigma, DEFF=1, conf=0.95, error = "rme", delta = 0.03, plot=TRUE)
#' # The minimum sample size for a complex sampling design
#' ss4m(N, mu, sigma, DEFF=1, conf=0.95, error = "me", delta = 5, plot=TRUE)
#' # The minimum sample size for a complex sampling design
#' ss4m(N, mu, sigma, DEFF=3.45, conf=0.95, error = "rme", delta = 0.03, plot=TRUE)


ss4m = function(N, mu, sigma, DEFF=1, conf=0.95, error = "cve", delta = 0.03, 
                plot=FALSE){
  
  S2 <- sigma^2 * DEFF
  Z <- 1 - ((1 - conf) / 2)
  
  if (error == "cve"){
    n <- S2/(mu^2 * delta^2 + (S2/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      cveseq = 100 * sqrt(varseq)/mu
      
      plot(nseq, cveseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Coefficient of variation %", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  if (error == "me") {
    n0 <- (qnorm(Z)^2/delta^2) * S2
    n <- n0/(1 + (n0/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      meseq = 100 * qnorm(Z) * sqrt(varseq)
      
      plot(nseq, meseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Margin of error", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  if (error == "rme"){
    n0 <- (qnorm(Z)^2/(delta * mu)^2) * S2
    n <- n0/(1 + (n0/N))
    
    if (plot == TRUE) {
      nseq = seq(100, N, 10)
      fseq = nseq/N
      varseq = (1/nseq) * (1 - fseq) * S2
      rmeseq = 100 * qnorm(Z) * sqrt(varseq) / mu
      
      plot(nseq, rmeseq, type = "l", lty = 2, pch = 1, col = 3, 
           ylab = "Relative margin of error %", xlab = "Sample size")
      points(n, 100 * delta, pch = 8, col = "red")
      abline(h = 100 * delta, lty = 3)
      abline(v = n, lty = 3)
    }
  }
  return(ceiling(n))
}

