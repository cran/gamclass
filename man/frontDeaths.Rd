\name{frontDeaths}
\alias{frontDeaths}
\alias{sideDeaths}
\alias{rearDeaths}
\alias{otherDeaths}
\docType{data}
\title{
Safety Device effectiveness Measures, by Year 
}
\description{
Safety devices may be airbags or seatbelts.  For airbags, alternatives
are to use \sQuote{airbag installed} or \sQuote{airbag deployed} as the
criterion. Ratio of driver deaths to passenger deaths are calculated
for driver with device and for driver without device, in both cases for
passenger without device.
}
\usage{data("frontDeaths")}
\format{
  The format is:
List of 3
 $ airbagAvail : num [1:13, 1:2, 1:4] 1068 1120 1089 1033 940 ...
  ..- attr(*, "dimnames")=List of 3
  .. ..$ years        : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ D_airbagAvail: chr [1:2] "no" "yes"
  .. ..$ injury       : chr [1:4] "P_injury" "D_injury" "tot" "prop"
 $ airbagDeploy: num [1:13, 1:2, 1:4] 1133 1226 1196 1151 1091 ...
  ..- attr(*, "dimnames")=List of 3
  .. ..$ years        : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ D_airbagAvail: chr [1:2] "no" "yes"
  .. ..$ injury       : chr [1:4] "P_injury" "D_injury" "tot" "prop"
 $ restraint   : num [1:13, 1:2, 1:4] 780 783 735 714 741 645 634 561 558 494 ...
  ..- attr(*, "dimnames")=List of 3
  .. ..$ years        : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ D_airbagAvail: chr [1:2] "no" "yes"
  .. ..$ injury       : chr [1:4] "P_injury" "D_injury" "tot" "prop"
}
\source{
See \code{\link{FARS}}
}
\examples{
data(frontDeaths)
## maybe str(frontDeaths) ; plot(frontDeaths) ...
}
\keyword{datasets}
