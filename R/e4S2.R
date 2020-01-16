#' @import TeachingSampling
#' @import timeDate
#' @export
#' 
#' @title
#' Statistical errors for the estimation of a single variance 
#' @description 
#' This function computes the cofficient of variation and the margin of error when estimating a single variance under a sample design.
#' @return 
#' The coefficient of variation and the margin of error for a predefined sample size.
#' @details
#' We note that the coefficient of variation is defined as: \deqn{cve = \frac{\sqrt{Var(\hat{S^2})}}{\hat{S^2}}} 
#' Also, note that the magin of error is defined as: \deqn{\varepsilon = z_{1-\frac{\alpha}{2}}\sqrt{Var(\hat{S^2})}}
#' 
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N The population size.
#' @param n The sample size.
#' @param K The excess kurtosis of the variable in the population.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param plot Optionally plot the errors (cve and margin of error) against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ss4p}}
#' @examples 
#' e4S2(N=10000, n=400, K = 0)
#' e4S2(N=10000, n=400, K = 1, DEFF = 2, conf = 0.99)
#' e4S2(N=10000, n=400, K = 2, DEFF = 2, conf = 0.99, plot=TRUE)

e4S2 <- function (N, n, K=0, DEFF = 1, conf = 0.95, plot = FALSE) {
  Z = 1 - ((1 - conf)/2)
  f <- n/N
  CVE <- 100* sqrt(DEFF * N^2 * (K*N + 2*N + 2) * (1 - f)  / (n * (N - 1)^3))
  ME <- qnorm(Z) * CVE
  if (plot == TRUE) {
    nseq <- seq(1, N, 10)
    cveseq <- rep(NA, length(nseq))
    meseq <- rep(NA, length(nseq))
    for (k in 1:length(nseq)) {
      fseq <- nseq[k]/N
      cveseq[k] <- 100* sqrt(DEFF * N^2 * (K*N + 2*N + 2) * (1 - fseq)  / (nseq[k] * (N - 1)^3))
      meseq[k] <- qnorm(Z) * cveseq[k]
    }
    par(mfrow = c(1, 2))
    plot(nseq, cveseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Coefficient of variation %", xlab = "Sample Size")
    points(n, CVE, pch = 8, bg = "blue")
    abline(h = CVE, lty = 3)
    abline(v = n, lty = 3)
    plot(nseq, meseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Margin of error", xlab = "Sample Size")
    points(n, ME, pch = 8, bg = "blue")
    abline(h = ME, lty = 3)
    abline(v = n, lty = 3)
  }
  msg <- cat("With the parameters of this function: N =", N, 
             "n = ", n, "Kurtosis = ", K, "DEFF = ", DEFF, "conf =", conf, 
             ". \nThe estimated coefficient of variation is ", CVE, 
             ". \nThe margin of error is", ME, ". \n \n")
  result <- list(cve = CVE, Margin_of_error = ME)
  result
}
