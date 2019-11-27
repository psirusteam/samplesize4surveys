#' @import TeachingSampling
#' @export
#' 
#' @title
#' Sample Sizes for Household Surveys in Two-Stages for Estimating Single Proportions
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
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at gmail.com>
#' @param N The population size.
#' @param M Number of clusters in the population.
#' @param r Percentage of people within the subpopulation of interest.
#' @param b Average household size (number of members).
#' @param P The value of the estimated proportion.
#' @param conf The statistical confidence. By default \code{conf = 0.95}.
#' @param delta The maximun margin of error that can be allowed for the estimation.
#' @param m (vector) Number of households selected within PSU.
#' @param rho The Intraclass Correlation Coefficient.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ICC}}
#' 
#' @examples
#' 
#' ss4HHSp(N = 50000000, M = 3000, r = 1, b = 3.5, 
#' rho = 0.034, P = 0.05, delta = 0.05, conf = 0.95,
#' m = c(5:15))
#' 
#' ##################################
#' # Example with BigCity data      #
#' # Sample size for the estimation #
#' # of the unemployment rate       #
#' ##################################
#' 
#' library(TeachingSampling)
#' data(BigCity)
#' 
#' BigCity1 <- BigCity[!is.na(BigCity$Employment), ]
#' summary(BigCity1$Employment)
#' BigCity1$Unemp <- Domains(BigCity1$Employment)[, 1]
#' BigCity1$Active <- Domains(BigCity1$Employment)[, 1] +
#' Domains(BigCity1$Employment)[, 3]
#' 
#' N <- nrow(BigCity)
#' M <- length(unique(BigCity$PSU))
#' r <- sum(BigCity1$Active)/N
#' b <- N/length(unique(BigCity$HHID))
#' rho <- ICC(BigCity1$Unemp, BigCity1$PSU)$ICC
#' P <- sum(BigCity1$Unemp)/sum(BigCity1$Active)
#' delta <- 0.05
#' conf <- 0.95
#' m <- c(5:15)
#' ss4HHSp(N, M, r, b, rho, P, delta, conf, m)

ss4HHSp <- function(N, M, r, b, rho, P, delta, conf, m){
  
  bar.n <- Deff <- n <- Mi <- M <- 
    rep(NA, times = length(m))
  
  for (k in 1:length(m)) {
    bar.n[k] <- m[k] * r * b
    Deff[k] <- 1 + (bar.n[k] - 1) * rho
    n[k] <- ss4p(N, P, DEFF = Deff[k], conf = conf, 
            error = "rme", delta = delta)
    Mi[k] <- n[k] / (r * b)
    M[k] <- n[k] / bar.n[k]
    }
  
  result <- data.frame(HouseholdsPerPSU = round(m), 
                   PersonsPerPSU = round(bar.n),
                   DEFF = round(Deff, 2), 
                   PSUinSample = round(M),
                   HouseholdsInSample = round(Mi), 
                   PersonsInSample = round(n))
  
  return(result)
}
