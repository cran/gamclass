\name{loti}
\alias{loti}
\docType{data}
\title{
Global temperature anomalies
}
\description{
  GISS (Goddard Institute for Space Studies) Land-Ocean Temperature 
  Index (LOTI) data for the years 1880 to 2019, giving anomalies in 
  0.01 degrees Celsius, from the 1951 - 1980 average.
}
\usage{
loti
}
\format{
  A data frame with 140 observations on the following 19 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
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
    \item{\code{JtoD}}{Jan-Dec averages}
    \item{\code{D.N}}{Dec-Nov averages}
    \item{\code{DJF}}{Dec-Jan-Feb averages}
    \item{\code{MAM}}{Mar-Apr-May}
    \item{\code{JJA}}{Jun-Jul-Aug}
    \item{\code{SON}}{Sept-Oct-Nov}
    \item{\code{JtoD2011}}{January to December average, from 
    data accessed in 2011}
  }
}
% \details{
% %%  ~~ If necessary, more details than the __description__ above ~~
% }
\source{
Data are the Combined Land-Surface Air and Sea-Surface Water Temperature 
Anomalies (Land-Ocean Temperature Index, LOTI), in 0.01 degrees Celsius, from
\url{https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt}
Data in the column \code{JtoD2011} was accessed 2011-09-06.

Also available is a CSV file, with anomalies in degrees Celsius.
}
\references{
GISTEMP Team, 2020: GISS Surface Temperature Analysis (GISTEMP), version 4. 
NASA Goddard Institute for Space Studies. Dataset accessed 2020-11-13 at 
\url{https://data.giss.nasa.gov/gistemp/}.
}
\examples{
data(loti)
plot(JtoD ~ Year, data=loti)
## Add 11 point moving average
ma11 <- filter(loti$JtoD, rep(1,11)/11, sides=2)
lines(loti$Year, ma11)
}
\keyword{datasets}
