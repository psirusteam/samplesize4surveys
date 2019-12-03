#' @import TeachingSampling
#' @export
#' 
#' @title
#' Sample Sizes for Household Surveys in Two-Stages for Estimating Single Means
#' @description 
#' This function computes a grid of possible sample sizes for estimating single means under two-stage sampling designs.
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
#' @param mu The value of the estimated mean of a variable of interest.
#' @param sigma The value of the estimated standard deviation of a variable of interest.
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
#' ss4HHSm(N = 50000000, M = 3000, rho = 0.034, 
#'         mu = 10, sigma = 2, delta = 0.03, conf = 0.95,
#'         m = c(5:15))
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
#' BigCity1 <- BigCity %>% 
#'             group_by(HHID) %>%
#'             summarise(IncomeHH = sum(Income),
#'                       PSU = unique(PSU))
#'                       
#' summary(BigCity1$IncomeHH)
#' mean(BigCity1$IncomeHH)
#' sd(BigCity1$IncomeHH)
#' 
#' N <- nrow(BigCity)
#' M <- length(unique(BigCity$PSU))
#' rho <- ICC(BigCity1$IncomeHH, BigCity1$PSU)$ICC
#' mu <- mean(BigCity1$IncomeHH)
#' sigma <- sd(BigCity1$IncomeHH)
#' delta <- 0.05
#' conf <- 0.95
#' m <- c(5:15)
#' ss4HHSm(N, M, rho, mu, sigma, delta, conf, m)

ss4HHSm <- function(N, M, rho, mu, sigma, delta, conf, m){
  
  Deff <- n <- M <- rep(NA, times = length(m))
  
  for (k in 1:length(m)) {
    Deff[k] <- 1 + (m[k] - 1) * rho
    n[k] <- ss4m(N, mu, sigma, DEFF = Deff[k], conf=0.95, 
         error = "rme", delta = delta)
    M[k] <- n[k] / m[k]
  }
  
  result <- data.frame(HouseholdsPerPSU = round(m), 
                       DEFF = round(Deff, 2), 
                       PSUinSample = round(M),
                       HouseholdsInSample = round(n))
  
  return(result)
}
