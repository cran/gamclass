\name{fars2007}
\alias{fars2007}
\alias{fars2008}
\docType{data}
\title{
US fatal road accident data, 2007 and 2008
}
\description{
  Data are included on variables that may be relevant to assessing
  airbag and seatbelt effectiveness in preventing fatal injury.
}
\usage{
fars2007
fars2008
}
\format{
  A data frame with 72548 observations on the following 24 variables.
  \describe{
    \item{\code{Obs.}}{a numeric vector}
    \item{\code{state}}{a numeric vector}
    \item{\code{casenum}}{a numeric vector}
    \item{\code{vnum}}{a numeric vector}
    \item{\code{pnum}}{a numeric vector}
    \item{\code{lightcond}}{a numeric vector}
    \item{\code{numfatal}}{a numeric vector}
    \item{\code{vforms}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{airbag}}{a numeric vector}
    \item{\code{injury}}{a numeric vector}
    \item{\code{ptype}}{a numeric vector}
    \item{\code{restraint}}{a numeric vector}
    \item{\code{seatpos}}{a numeric vector}
    \item{\code{sex}}{a numeric vector}
    \item{\code{body}}{a numeric vector}
    \item{\code{inimpact}}{a numeric vector}
    \item{\code{mhevent}}{a numeric vector}
    \item{\code{vfatcount}}{a numeric vector}
    \item{\code{numoccs}}{a numeric vector}
    \item{\code{travspd}}{a numeric vector}
    \item{\code{make}}{a numeric vector}
    \item{\code{model}}{a numeric vector}
    \item{\code{modelyr}}{a numeric vector}
  }
}
\details{
  Data is for automabiles where a passenger seat was occupied,
  with one observation for each such passenger.
}
\source{
\url{http://www-fars.nhtsa.dot.gov/Main/index.aspx}
}
\references{
  \url{http://maths-people.anu.edu.au/~johnm/nzsr/taws.html}

  Olson CM, Cummings P, Rivara FP. 2006. Association of first- and
  second-generation air bags with front occupant death in car crashes: a
  matched cohort study. Am J Epidemiol 164:161-169

  Cummings, P; McKnight, B, 2010. Accounting for vehicle, crash, and occupant
  characteristics in traffic crash studies. Injury Prevention 16: 363-366

  Braver, ER; Shardell, M; Teoh, ER, 2010. \emph{How have changes in air
  bag designs affected frontal crash mortality?} Ann Epidemiol 20:499-510.
}
\examples{
data(fars2007)
str(fars2007)
}
\keyword{datasets}
