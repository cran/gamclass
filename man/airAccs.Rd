\name{airAccs}
\alias{airAccs}
\docType{data}
\title{
Aircraft Crash data
}
\description{
Aircraft Crash Data
}
\usage{data(airAccs)
}
\format{
  A data frame with 5666 observations on the following 7 variables.
  \describe{
    \item{\code{Date}}{Date of Accident}
    \item{\code{location}}{Location of accident}
    \item{\code{operator}}{Aircraft operator}
    \item{\code{planeType}}{Aircraft type}
    \item{\code{Dead}}{Number of deaths}
    \item{\code{Aboard}}{Number aboard}
    \item{\code{Ground}}{Deaths on ground}    
  }
}
\details{
For details of inclusion criteria, see
  \url{http://www.planecrashinfo.com/database.htm}
}
\source{
  \url{http://www.planecrashinfo.com/database.htm}
}
\references{
\url{http://www.planecrashinfo.com/reference.htm}
}
\examples{
data(airAccs)
str(airAccs)
}
\keyword{datasets}
