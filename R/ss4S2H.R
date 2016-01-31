#' @import TeachingSampling
#' @export
#' 
#' @title
#' The required sample size for testing a null hyphotesis regarding a single variance
#' @description 
#' This function returns the minimum sample size required for testing a null hyphotesis regarding a single variance
#' @details
#' We assume that it is of interest to test the following set of hyphotesis:
#' \deqn{H_0: S^2_U - S^2_0 = 0 \ \ \ \ vs. \ \ \ \ H_a: S^2_U - S^2_0 = D > \neq 0 }
#' Note that the minimun sample size, restricted to the predefined power \eqn{\beta} and confidence \eqn{1-\alpha}, is defined by: 
#' \deqn{n = \frac{S^4_U*DEFF}{\frac{D^2}{(z_{1-\alpha} + z_{\beta})^2}\frac{(N-3)(N-2)}{N(2N-7)}+\frac{S^4_U*DEFF}{N}}}
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at usantotomas.edu.co> and Hanwen Zhang <hanwenzhang at usantotomas.edu.co>
#' @param N The maximun population size between the groups (strata) that we want to compare.
#' @param S2 The value of the estimated variance.
#' @param S20 The value to test for the variance.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param power The statistical power. By default \code{power = 0.80}.
#' @param plot Optionally plot the effect against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ss4pH}}
#' @examples 
#' ss4S2H(N = 100000, S2 = 100, S20 = 110)
#' ss4S2H(N = 100000, S2 = 100, S20 = 110, plot=TRUE)
#' 
#' #############################
#' # Example with BigLucy data #
#' #############################
#' data(BigLucy)
#' attach(BigLucy)
#' 
#' N1 <- table(SPAM)[1]
#' N2 <- table(SPAM)[2]
#' N <- max(N1,N2)
#' P1 <- prop.table(table(SPAM))[1]
#' P2 <- prop.table(table(SPAM))[2]
#' 
#' # The minimum sample size for testing 
#' # H_0: P_1 - P_2 = 0   vs.   H_a: P_1 - P_2 = D = 0.05
#' D = 0.05  
#' ss4dpH(N, P1, P2, D, DEFF = 2, plot=TRUE)
#' 
#' # The minimum sample size for testing 
#' # H_0: P - P_0 = 0   vs.   H_a: P - P_0 = D = 0.02
#' D = 0.01
#' ss4dpH(N, P1, P2, D, conf = 0.99, power = 0.9, DEFF = 3.45, plot=TRUE)

ss4S2H = function(N, S2, S20, DEFF=1, conf=0.95, power=0.8, plot=FALSE){
  
  Za = conf
  Zb = power
  Z = qnorm(Za) + qnorm(Zb)
  D = abs(S2 - S20)
  n.hyp = S2^2*DEFF/(D^2*(N-3)*(N-2)/(Z^2*N*(2*N-7)) + S2^2*DEFF/N)
  n.hyp = ceiling(n.hyp)
  if (plot == TRUE) {
    nseq = seq(100, N, 10)
    Dseq = rep(NA, length(nseq))
    for (k in 1:length(nseq)) {
      fseq = nseq[k]/N
      varseq = (1/fseq) * (1 - fseq) * (2*N-7) * (S2^2) * (qnorm(Za) + 
                                                  qnorm(Zb))^2/((N-3)*(N-2))
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