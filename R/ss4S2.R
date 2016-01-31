#' @import TeachingSampling
#' @export
#' 
#' @title
#' The required sample size for estimating a single variance
#' @description 
#' This function returns the minimum sample size required for estimating variance in a finite population subjecto to predefined errors.
#' @details
#' Note that the minimun sample size to achieve a particular relative margin of error \eqn{\varepsilon} is defined by: 
#' \deqn{n = \frac{n_0}{\frac{(N-3)(N-2)}{N(2N-7)}+\frac{n_0}{N}}}
#' Where \deqn{n_0=\frac{z^2_{1-\frac{\alpha}{2}}DEFF}{\varepsilon^2}}
#' Also note that the minimun sample size to achieve a particular coefficient of variation \eqn{cve} is defined by:
#' \deqn{n = \frac{S^2}{p^2cve^2+\frac{S^2}{N}}} 
#'   
#' @author Hugo Andres Gutierrez Rojas <hugogutierrez at usantotomas.edu.co> and Hanwen Zhang <hanwenzhang at usantotomas.edu.co>
#' @param N The population size.
#' @param DEFF The design effect of the sample design. By default \code{DEFF = 1}, which corresponds to a simple random sampling design.
#' @param conf The statistical confidence. By default conf = 0.95. By default \code{conf = 0.95}.
#' @param cve The maximun coeficient of variation that can be allowed for the estimation.
#' @param rme The maximun relative margin of error that can be allowed for the estimation.
#' @param plot Optionally plot the errors (cve and margin of error) against the sample size.
#' 
#' @references 
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas
#' @seealso \code{\link{ss4S2H}}
#' @examples 
#' ss4S2(N=100000, DEFF=1, conf=0.95, cve=0.05, rme=0.03)
#' ss4S2(N=100000, DEFF=1, conf=0.95, cve=0.05, rme=0.03, plot=TRUE)
#' ss4S2(N=100000, DEFF=1, conf=0.95, cve=0.05, rme=0.05, plot=TRUE)
#' 
#' ##########################
#' # Example with Lucy data #
#' ##########################
#' 
#' data(Lucy)
#' attach(Lucy)
#' N <- nrow(Lucy)
#' # The population variance of the variable Income
#' S2 <- var(Lucy$Income)
#' # The minimum sample size for simple random sampling
#' ss4p(N, p, DEFF=1, conf=0.99, cve=0.03, me=0.03, plot=TRUE)
#' # The minimum sample size for a complex sampling design
#' ss4p(N, p, DEFF=3.45, conf=0.99, cve=0.03, me=0.03, plot=TRUE)

ss4S2 <- function(N, DEFF=1, conf=0.95, cve=0.05, rme=0.03, plot=FALSE){
  Z = 1-((1-conf)/2)
  n0.rme <- qnorm(Z)^2*DEFF/(rme^2)
  n.rme <- n0.rme/((N-3)*(N-2)/(N*(2*N-7))+n0.rme/N)
  n.cve <- N*(2*N-7)*DEFF/(cve^2*(N-3)*(N-2)+(2*N-7)*DEFF)
  
  if(plot == TRUE){
   
    nseq=seq(100,N,10) 
    cveseq=rep(NA,length(nseq))
    meseq=rep(NA,length(nseq))
      
      for(k in 1:length(nseq)){
        fseq=nseq[k]/N
        cveseq[k]=100*sqrt(N/nseq[k]*(1-fseq)*DEFF*(2*N-7)/((N-3)*(N-2)))
        meseq[k]=qnorm(Z)*cveseq[k]
      }
    par(mfrow=c(1,2))
    plot(nseq,cveseq, type="l", lty=2, pch=1, col=3,ylab="Coefficient of variation %",xlab="Sample size")
    points(n.cve, 100*cve, pch=8,bg = "blue")
    abline(h=100*cve,lty=3)
    abline(v=n.cve,lty=3)
    
    plot(nseq,meseq, type="l", lty=2, pch=1, col=3,ylab="Relative margin of error %",xlab="Sample size")
    points(n.rme, 100*rme, pch=8,bg = "red")
    abline(h=100*rme,lty=3)
    abline(v=n.rme,lty=3)
  }
  msg <- cat('With the parameters of this function: N =', N, 'DEFF = ',
             DEFF, 'conf =', conf, '.\n
             The estimated sample size to obatin a maximun coefficient of variation of', 100*cve, '% is n=', ceiling(n.cve), '.
             The estimated sample size to obatin a maximun relative margin of error of', 100*rme, '% is n=', ceiling(n.rme), '. \n \n')
  
  result <- list(n.cve = ceiling(n.cve), n.rme = ceiling(n.rme))
  result 
}