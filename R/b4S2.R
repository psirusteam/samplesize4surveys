#' @import TeachingSampling
#' @import timeDate
#' @export
#' 
#' @title
#' Statistical power for a hyphotesis testing on a single variance
#' @description 
#' This function computes the power for a (right tail) test of variance 
#' @return 
#' The power of the test.
#' @details
#' We note that the power is defined as: \deqn{1-\Phi(Z_{1-\alpha} - \frac{(D-P)}{\sqrt{\frac{DEFF}{n}(1-\frac{n}{N})(P (1-P))}})}
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at gmail.com>
#' @param N The population size.
#' @param n The sample size.
#' @param S2 The value of the first estimated proportion.
#' @param S20 The value of the null effect. Note that \code{S2} must be strictly smaller than \code{S2}.
#' @param K The excess kurtosis of the variable in the population.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param power The statistical power. By default power = 0.80.
#' @param plot Optionally plot the power achieved for an specific sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ss4p}}
#' @examples 
#' b4S2(N = 100000, n = 400, S2 = 120, S20 = 100, K = 0, DEFF = 1)
#' b4S2(N = 100000, n = 400, S2 = 120, S20 = 100, K = 2, DEFF = 1)
#' b4S2(N = 100000, n = 400, S2 = 120, S20 = 100, K = 2, DEFF = 2.5, plot = TRUE)

b4S2 <- function (N, n, S2, S20, K=0, DEFF = 1, conf = 0.95, power = 0.8, plot = FALSE){
  Za = conf
  Zb = power
  Z = qnorm(Za) + qnorm(Zb)
  D = abs(S2 - S20)
  f = n/N
  VAR = DEFF * (1/n) * (1 - f) * N^2 * (K*N + 2*N +2) * S2^2 / ((N - 1)^3)
  beta = 100 * (1 - pnorm(Za - (D / sqrt(VAR))))
  if (plot == TRUE) {
    nseq = seq(1, N, 10)
    betaseq = rep(NA, length(nseq))
    for (k in 1:length(nseq)) {
      fseq = nseq[k]/N
      varseq = DEFF * (1/nseq[k]) * (1 - fseq) * N^2 * (K*N + 2*N +2) * S2^2 / ((N - 1)^3)
      betaseq[k] = 100 * (1 - pnorm(Za - (D /sqrt(varseq))))
    }
    plot(nseq, betaseq, type = "l", lty = 1, pch = 1, col = 3, 
         ylab = "Power of the test (%)", xlab = "Sample Size")
    points(n, beta, pch = 8, bg = "blue")
    abline(h = beta, lty = 3)
    abline(v = n, lty = 3)
  }
  msg <- cat("With the parameters of this function: N =", N, 
             "n = ", n, "S2 =", S2, "S20 =", S20, "Kurtosis =", K, "D =", D, "DEFF = ", DEFF, "conf =", 
             conf, ". \nThe estimated power of the test is ", beta, 
             ". \n \n")
  result <- list(Power = beta)
  result
}
