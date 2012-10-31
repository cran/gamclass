\name{plotFars}
\alias{plotFars}
\title{
  Extract ratio of ratios estimate of safety device effectiveness,
  and return trellis graphics object
}
\description{
Safety devices may be airbags or seatbelts.  For airbags, alternatives
are to use \sQuote{airbag installed} or \sQuote{airbag deployed} as the
criterion. Ratio of driver deaths to passenger deaths are calculated
for driver with device and for driver without device, in both cases for
passenger without device.
}
\usage{
plotFars(data = FARS, restrict =
"age>=16&age<998&inimpact\%in\%c(11,12,1)", fatal = 4, statistics =
c("airbagAvail", "airbagDeploy", "Restraint"))
}
\arguments{
  \item{data}{
    Data frame, which must include among its columns variables
    \code{year}, variables noted in
    \code{statistics} plus corresponding variables for drivers,
    variables that appear in \code{restrict}, the variable
    \code{injury} and \code{D_injury}.
}
  \item{restrict}{
text: an expression that restricts observations considered
}
  \item{fatal}{
    numeric: 4 for fatal injury, or \code{c(3,4)} for incxapacitating or
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
  A graphics object is returned
}
\author{
John Maindonald
}
\seealso{
\code{\link{matchedPairs}}
}
\examples{
\dontrun{
gphFars <- plotFars()
}
}
\keyword{manip}% __ONLY ONE__ keyword per line
