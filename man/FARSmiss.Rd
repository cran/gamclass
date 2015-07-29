\name{FARSmiss}
\alias{FARSmiss}
\docType{data}
\title{
Summary information on records omitted from the \code{FARS} dataset
}
\description{
Data are a 3-way table, indexed by \code{state}, a set of
variable names, and \code{years}
}
\usage{FARSmiss}
\format{
  The format is:
 num [1:51, 1:7, 1:13] 2 0 16 0 75 1 5 0 5 5 ...
 - attr(*, "dimnames")=List of 3
  ..$ : chr [1:51] "1" "2" "3" "4" ...
  ..$ : chr [1:7] "injury" "age" "airbag" "restraint" ...
  ..$ : chr [1:13] "1998" "1999" "2000" "2001" ...
}
\details{
  These data were generated using the function \code{\link{matchedPairs}},
  using as input data downloaded from the URL given as source. Data for
  the years 2007 and 2008 are included with this package, and can be
  used to generate the result of restricting \code{\link{FARS}} and
  \code{FARSmiss} to those years.  The check columns (all values should
  be zero) \code{nomatch} and \code{dups} have been omitted from the
  second dimension of the array
}
\source{
\url{http://www-fars.nhtsa.dot.gov/Main/index.aspx}
}
\references{
\url{http://maths-people.anu.edu.au/~johnm/nzsr/taws.html}
}
\seealso{
\code{\link{matchedPairs}}
}
\examples{
data(FARSmiss)
str(FARSmiss)
}
\keyword{datasets}
