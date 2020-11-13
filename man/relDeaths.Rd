\name{relDeaths}
\alias{relDeaths}
\docType{data}
\title{
Yearly Driver deaths, as Fraction of Deaths for All Years
}
\description{The four list elements are for four positions of initial impact.
Each list element is a 13 by 3 \verb{years} by \dQuote{safety device} matrix
that gives the proportion, for that device in year, of the total over 
\var{years}
}
\usage{data("relDeaths")}
\format{
  The format is:
List of 4
 $ front: num [1:13, 1:3] 0.559 0.548 0.544 0.577 0.574 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ : chr [1:3] "airbagAvail" "airbagDeploy" "restraint"
 $ side : num [1:13, 1:3] 0.36 0.366 0.367 0.35 0.348 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ : chr [1:3] "airbagAvail" "airbagDeploy" "restraint"
 $ rear : num [1:13, 1:3] 0.0507 0.0558 0.0575 0.0498 0.0522 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ : chr [1:3] "airbagAvail" "airbagDeploy" "restraint"
 $ other: num [1:13, 1:3] 0.0312 0.0304 0.0313 0.0237 0.0254 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:13] "1998" "1999" "2000" "2001" ...
  .. ..$ : chr [1:3] "airbagAvail" "airbagDeploy" "restraint"
}
\examples{
data(relDeaths)
## maybe str(relDeaths) ; plot(relDeaths) ...
}
\keyword{datasets}
