#' @import TeachingSampling
#' @import timeDate
#' @export
#' 
#' @title
#' The required sample size for testing a null hyphotesis for a single variance
#' @description 
#' This function returns the minimum sample size required for testing a null hyphotesis regarding a single variance
#' @details
#' We assume that it is of interest to test the following set of hyphotesis:
#' \deqn{H_0: P - P_0 = 0 \ \ \ \ vs. \ \ \ \ H_a: P - P_0 = D > 0 }
#' Note that the minimun sample size, restricted to the predefined power \eqn{\beta} and confidence \eqn{1-\alpha}, is defined by: 
#' \deqn{n = \frac{S2^2}{\frac{D^2}{(z_{1-\alpha} + z_{\beta})^2}\frac{(N-1)^3}{N^2(N*K+2N+2)}+\frac{S2^2}{N}}}
#'  
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N The population size.
#' @param S2 The value of the estimated variance
#' @param S20 The value to test for the single variance
#' @param K The excess kurtosis of the variable in the population.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param power The statistical power. By default \code{power = 0.80}.
#' @param plot Optionally plot the effect against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{e4p}}
#' @examples 
#' 
#' ss4S2H(N = 10000, S2 = 120, S20 = 110, K = 0)
#' ss4S2H(N = 10000, S2 = 120, S20 = 110, K = 2, DEFF = 2, power = 0.9)
#' ss4S2H(N = 10000, S2 = 120, S20 = 110, K = 2, DEFF = 2, power = 0.8, plot = TRUE)
#' 
#' #############################
#' # Example with BigLucy data #
#' #############################
#' data(BigLucy)
#' attach(BigLucy)
#' N <- nrow(BigLucy)
#' S2 <- var(BigLucy$Income)
#' 
#' # The minimum sample size for testing 
#' # H_0: S2 - S2_0 = 0   vs.   H_a: S2 - S2_0 = D = 8000
#' D = 8000 
#' S20 = S2 - D 
#' K <- kurtosis(BigLucy$Income)
#' ss4S2H(N, S2, S20, K, DEFF=1, conf = 0.99, power = 0.8, plot=TRUE)

ss4S2H <- function(N, S2, S20, K=0, DEFF=1, conf=0.95, power=0.8, plot=FALSE){
  
  Za = conf
  Zb = power
  Z = qnorm(Za) + qnorm(Zb)
  D = abs(S2 - S20)
  n.hyp = S2^2 * DEFF / (D^2 * (N - 1)^3/(Z^2 * N^2 * (K*N + 2*N + 2)) + S2^2 * DEFF / N)
  n.hyp = ceiling(n.hyp)
  if (plot == TRUE) {
    nseq = seq(100, N, 10)
    Dseq = rep(NA, length(nseq))
    for (k in 1:length(nseq)) {
      fseq = nseq[k]/N
      varseq = DEFF * (1/fseq) * (1 - fseq) * N * (K*N + 2*N + 2) * (S2^2) * (qnorm(Za) + qnorm(Zb))^2 / ((N - 1)^3)
      Dseq[k] = sqrt(varseq)
    }
    plot(nseq, Dseq, type = "l", lty = 2, pch = 1, col = 3, 
         ylab = "Null effect (D) ", xlab = "Sample size")
    points(n.hyp, D, pch = 8, bg = "blue")
    abline(h = D, lty = 3)
    abline(v = n.hyp, lty = 3)
  }
  result = n.hyp
  result
}

