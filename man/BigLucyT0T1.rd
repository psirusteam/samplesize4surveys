\name{BigLucyT0T1}
\docType{data}
\alias{BigLucyT0T1}
\title{Some Business Population Database for two periods of time}
\description{
This data set corresponds to a random sample of BigLucy. It contains some financial variables of 85296 industrial companies of a city in a particular fiscal year.
}

\usage{BigLucyT0T1}
\format{
  \describe{
\item{ID}{The identifier of the company. It correspond to an alphanumeric sequence (two letters and three digits)}
\item{Ubication}{The address of the principal office of the company in the city}
\item{Level}{The industrial companies are discrimitnated according to the Taxes declared. 
There are small, medium and big companies}
\item{Zone}{The city is divided by geoghrafical zones. A company is classified in a particular zone according to its address}
\item{Income}{The total ammount of a company's earnings (or profit) in the previuos fiscal year. It is calculated by taking 
revenues and adjusting for the cost of doing business}
\item{Employees}{The total number of persons working for the company in the previuos fiscal year}
\item{Taxes}{The total ammount of a company's income Tax}
\item{SPAM}{Indicates if the company uses the Internet and WEBmail options in order to make self-propaganda.}
\item{Segments}{The cartographic divisions.}
\item{Outgoing}{Expenses per year.}
\item{Years}{Age of the company.}
\item{ISO}{Indicates whether the company is quality-certified.}
\item{ISOYears}{Indicates the time company has been certified.}
\item{CountyP}{Indicates wheter the county is participating in the intervention. That is if the county contains companies that have been certified by ISO}
\item{Time}{Refers to the time of observation.}
}
}
\author{Hugo Andres Gutierrez Rojas \email{hugogutierrez@usantotomas.edu.co}}
\references{
Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}.
Editorial Universidad Santo Tomas.
}

\examples{
data(Lucy)
attach(Lucy)
# The variables of interest are: Income, Employees and Taxes
# This information is stored in a data frame called estima
estima <- data.frame(Income, Employees, Taxes)
# The population totals
colSums(estima)
# Some parameters of interest
table(SPAM,Level)
xtabs(Income ~ Level+SPAM)
# Correlations among characteristics of interest
cor(estima)
# Some useful histograms
hist(Income)
hist(Taxes)
hist(Employees)
# Some useful plots
boxplot(Income ~ Level)
barplot(table(Level))
pie(table(SPAM))
}
\keyword{datasets}
