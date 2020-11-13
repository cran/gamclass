\name{plotFars}
\alias{plotFars}
\title{
  Plot Protection Device Effectiveness Measure Against Year 
}
\description{
Devices may be airbags or seatbelts.  For airbags, alternatives are 
to use \dQuote{airbag installed} or \dQuote{airbag deployed} as 
the criterion.  The plot shows, for each of the specified features, 
the ratio of driver death rate (or other outcome, e.g., death or injury)
with feature, to rate without feature, in both cases for passenger 
without feature.
}
\usage{
plotFars(tabDeaths=gamclass::frontDeaths,
          statistics = c("airbagAvail", "airbagDeploy", "restraint"))
}
\arguments{
  \item{tabDeaths}{List, containing (as a minimum) three-dimensional arrays
  with the names specified in the argument \code{statistics}, such as is
  returned by the function \code{\link{tabFarsDead}}}
  \item{statistics}{
Vector of character: names of the sublists, which contain information on
the deathrates
}
}
\details{
The name \code{injury} is used, with \code{frontDeaths} or \code{sideDeaths}
or \code{rearDeaths} or \code{otherDeaths} as the first argument, to refer to
deaths.  The function \code{tabFarsDeaths} allows the option of returning an
object, suitable for using as first argument, that treats \code{injury} as
death or serious injury.
}
\value{
  A graphics object is returned
}
\note{Note that the \dQuote{airbag deployed} statistic is not a useful measure
of airbag effectiveness.  At its most effective, the airbag will deploy
only when the accident is sufficiently serious that deployment will
reduce the risk of serious injury and/or accident. The with/without
deployment comparison compares, in part, serious accidents with less
serious accidents.}
\author{
John Maindonald
}
\keyword{hplot}
