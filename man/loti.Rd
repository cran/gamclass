\name{loti}
\alias{loti}
\docType{data}
\title{
Global temperature anomalies
}
\description{
  Anomalies, for the years 1880 to 2010, from the 1951 - 1980 average.
  These are the GISS (Goddard Institute for Space Studies) Land-Ocean Temperature Index (LOTI) data
}
\usage{
loti
}
\format{
  A data frame with 131 observations on the following 19 variables.
  \describe{
    \item{\code{Jan}}{a numeric vector}
    \item{\code{Feb}}{a numeric vector}
    \item{\code{Mar}}{a numeric vector}
    \item{\code{Apr}}{a numeric vector}
    \item{\code{May}}{a numeric vector}
    \item{\code{Jun}}{a numeric vector}
    \item{\code{Jul}}{a numeric vector}
    \item{\code{Aug}}{a numeric vector}
    \item{\code{Sep}}{a numeric vector}
    \item{\code{Oct}}{a numeric vector}
    \item{\code{Nov}}{a numeric vector}
    \item{\code{Dec}}{a numeric vector}
    \item{\code{J.D}}{Jan-Dec averages}
    \item{\code{D.N}}{Dec-Nov averages}
    \item{\code{DJF}}{Dec-Jan-Feb averages}
    \item{\code{MAM}}{Mar-Apr-May}
    \item{\code{JJA}}{Jun-Jul-Aug}
    \item{\code{SON}}{Sept-Oct-Nov}
    \item{\code{Year}}{a numeric vector}
  }
}
% \details{
% %%  ~~ If necessary, more details than the __description__ above ~~
% }
\source{
\url{http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt}
}
% \references{
% %%  ~~ possibly secondary sources and usages ~~
% }
\examples{
data(loti)
}
\keyword{datasets}
