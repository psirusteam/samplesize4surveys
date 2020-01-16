#' @import TeachingSampling
#' @export
#' 
#' @title
#' Sample Sizes in Two-Stage sampling Designs for Estimating Signle Proportions
#' @description 
#' This function computes a grid of possible sample sizes for estimating single proportions under two-stage sampling designs.
#' @return 
#' This function returns a grid of possible sample sizes. 
#' The first column represent the design effect,
#' the second column is the number of clusters to be selected, 
#' the third column is the number of units to be selected inside the clusters, 
#' and finally, the last column indicates the full sample size induced by this particular strategy.
#' @details
#' In two-stage (2S) sampling, the design effect is defined by
#' \deqn{DEFF = 1 + (\bar{m}-1)\rho} 
#' Where \eqn{\rho} is defined as the intraclass correlation coefficient,  
#' \eqn{\bar{m}} is the average sample size of units selected inside each cluster. 
#' The relationship of the full sample size of the two stage design (2S) with the 
#' simple random sample (SI) design is given by
#' \deqn{ n_{2S} =  n_{SI}*DEFF} 
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N The population size.
#' @param P The value of the estimated proportion.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param delta The maximun margin of error that can be allowed for the estimation.
#' @param M Number of clusters in the population.
#' @param to (integer) maximum number of final units to be selected per cluster. By default \code{to = 20}.
#' @param rho The Intraclass Correlation Coefficient.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ICC}}
#' 
#' @examples
#' 
#' ss2s4p(N=100000, P=0.5, delta=0.05, M=50, rho=0.01)
#' ss2s4p(N=100000, P=0.5, delta=0.05, M=500, to=40, rho=0.1)
#' ss2s4p(N=100000, P=0.5, delta=0.03, M=1000, to=100, rho=0.2) 
#' 
#' ############################
#' # Example 2 with Lucy data #
#' ############################
#' 
#' data(BigLucy)
#' attach(BigLucy)
#' N <- nrow(BigLucy)
#' P <- prop.table(table(SPAM))[1]
#' y <- Domains(SPAM)[, 1]
#' cl <- Segments
#' 
#' rho <- ICC(y,cl)$ICC
#' M <- length(levels(Segments))
#' ss2s4p(N, P, conf=0.95, delta = 0.03, M=M, to=30, rho=rho)

ss2s4p <- function(N, P, conf = 0.95, delta = 0.03, M, to = 20, rho) {
  
  mseq <- seq(from = 1, to = to)
  nIseq <- Deffseq <- n2seq <- rep(NA, times = length(mseq))
  
  for (i in 1:length(mseq)) {
    Deffseq[i] = 1 + (mseq[i] - 1) * rho
    n2seq[i] = ss4p(N, P, DEFF = Deffseq[i], conf = conf, 
      error = "rme", delta = delta)
    nIseq[i] <- ifelse(n2seq[i]/mseq[i] > M,
                       M, ceiling(n2seq[i]/mseq[i]))
  }
  
  result <- data.frame(Deff = Deffseq, nI = nIseq, m = mseq, 
    n2s = n2seq)
  result.adj <- result[(result$nI <= M), ]
  result.adj
}


