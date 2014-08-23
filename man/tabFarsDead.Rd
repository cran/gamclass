\name{tabFarsDead}
\alias{tabFarsDead}
\title{
  Extract ratio of ratios estimate of safety device effectiveness, from
  the \code{Fars} dataset.
}
\description{
Safety devices may be airbags or seatbelts.  For airbags, alternatives
are to use \sQuote{airbag installed} or \sQuote{airbag deployed} as the
criterion. Ratio of driver deaths to passenger deaths are calculated
for driver with device and for driver without device, in both cases for
passenger without device.
}
\usage{
tabFarsDead(restrict =
"age>=16&age<998&inimpact\%in\%c(11,12,1)", fatal = 4, statistics =
c("airbagAvail", "airbagDeploy", "Restraint"))
}
\arguments{
  \item{restrict}{
text: an expression that restricts observations considered
}
  \item{fatal}{
    numeric: 4 for fatal injury, or \code{c(3,4)} for incapacitating or
    fatal injury
}
  \item{statistics}{
Vector of character: ratio of ratios variables that wil be plotted
}
}
\details{
Note that the \sQuote{airbag deployed} statistic is not a useful measure
of airbag effectiveness.  At its most effective, the airbag will deploy
only when the accident is sufficiently serious that deployment will
reduce the risk of serious injury and/or accident. The with/without
deployment comparison compares, in part, serious accidents with less
serious accidents.
}
\value{
  A list with elements
  \item{airbagAvail}{a multiway table with margins \code{yrs},
    \code{airbagAvail}, and a third margin with levels
    \code{P_injury}, \code{D_injury}, \code{tot}, and \code{prop}}
  \item{airbagDeploy}{a multiway table with margins \code{yrs},
    \code{airbagDeploy}, and a third margin with levels
    \code{P_injury}, \code{D_injury}, \code{tot}, and \code{prop}}
  \item{Restraint}{a multiway table with margins \code{yrs},
    \code{Restraint}, and a third margin \code{injury} with levels
    \code{P_injury}, \code{D_injury}, \code{tot}, and \code{prop}}
}
\author{
John Maindonald
}
\seealso{
\code{\link{matchedPairs}}
}
\examples{
tabDeaths <- tabFarsDead()
}
\keyword{manip}% __ONLY ONE__ keyword per line
