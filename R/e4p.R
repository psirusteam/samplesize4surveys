#' @import TeachingSampling
#' @export
#' 
#' @title
#' Statistical errors for the estimation of a single proportion 
#' @description 
#' This function computes the cofficient of variation and the standard error when estimating a single proportion under a sample design.
#' @return 
#' The coefficient of variation, the margin of error and the relative margin of error for a predefined sample size.
#' @details
#' We note that the coefficent of variation is defined as: \deqn{cve = \frac{\sqrt{Var(\hat{p})}}{\hat{p}}} 
#' Also, note that the magin of error is defined as: \deqn{\varepsilon = z_{1-\frac{\alpha}{2}}\sqrt{Var(\hat{p})}}
#' 
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at usantotomas.edu.co>
#' @param N The population size.
#' @param n The sample size.
#' @param P The value of the estimated proportion.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param plot Optionally plot the errors (cve and margin of error) against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ss4p}}
#' @examples 
#' e4p(N=10000, n=400, P=0.5)
#' e4p(N=10000, n=400, P=0.5, plot=TRUE)
#' e4p(N=10000, n=400, P=0.01, DEFF=3.45, conf=0.99, plot=TRUE)


e4p <- function (N, n, P, DEFF = 1, conf = 0.95, plot = FALSE) 
{
  S2 <- P * (1 - P) * DEFF
  Z <- 1 - ((1 - conf)/2)
  f <- n/N
  VAR <- (1/n) * (1 - f) * S2
  CVE <- 100 * sqrt(VAR)/P
  ME <- 100 * qnorm(Z) * sqrt(VAR)
  RME <- ME / P
  if (plot == TRUE) {
    pseq <- seq(0, 1, 0.01)
    S2seq <- pseq * (1 - pseq) * DEFF
    varseq <- (1/n) * (1 - f) * S2seq
    cveseq <- 100 * sqrt(varseq)/pseq
    meseq <- 100 * qnorm(Z) * sqrt(varseq)
    rmeseq <- meseq / P
    LLseq = pseq - qnorm(Z) * sqrt(varseq)
    ULseq = pseq + qnorm(Z) * sqrt(varseq)
    
    par(mfrow = c(2, 2))
    
    plot(pseq, cveseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Coefficient of variation %", xlab = "Estimated proportion")
    points(P, CVE, pch = 8, col = "blue")
    abline(h = CVE, lty = 3)
    abline(v = P, lty = 3)
    
    plot(pseq, meseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Margin of error %", xlab = "Estimated proportion")
    points(P, ME, pch = 8, col = "blue")
    abline(h = ME, lty = 3)
    abline(v = P, lty = 3)
    
    plot(pseq, rmeseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Relative margin of error %", xlab = "Estimated proportion")
    points(P, RME, pch = 8, col = "blue")
    abline(h = RME, lty = 3)
    abline(v = P, lty = 3)
    
    plot(pseq, pseq, type = "l", lty = 2, pch = 1, col = 3, 
         ylab = "Widht of confidence interval", xlab = "Proportion")
    points(P, P, pch = 8)
    lines(pseq, LLseq, lty = 3, col = 2)
    lines(pseq, ULseq, lty = 3, col = 2)
    abline(h = P, lty = 3, col = 4)
    abline(v = P, lty = 3, col = 4)
    
  }
  result <- list(cve = CVE, 
                 Margin_of_error = ME,
                 Relative_Margin_of_error = RME)
  result
}
