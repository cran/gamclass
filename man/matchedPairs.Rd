\name{matchedPairs}
\alias{matchedPairs}
\title{
  Collect together driver and right seat passenger information, for the
  specified year
}
\description{
  This function collates the information needed for a matched pairs
  analysis. Driver mortalities, with and without airabgs, are matched to
  passenger mortalities for right front seat passengers in cars without
  passenger airbags.  It was used to generate the \code{\link{FARS}} and
  \code{\link{FARSmiss}} datasets
}
\usage{
matchedPairs(years = 2007:2008, prefix = "fars", compareBYvar = c("airbagAvail",
    "airbagDeploy", "Restraint"), bycat = list(airbagAvail = list(yes = c(1:9,
    20, 28:29, 31:32), no = 30, leaveout = c(0, 98, 99)), airbagDeploy = list(yes = c(1:9),
    no = c(20, 28, 30:32), leaveout = c(0, 29, 98, 99)), Restraint = list(yes = c(1:4,
    8, 10:12, 97), no = c(0, 5, 6, 7, 13:17), leaveout = (98:99))),
    restrict = "body\%in\%c(1:19,48,49,61,62)&!(mhevent\%in\%(2:5))",
    restrictvars = c("body", "mhevent", "seatpos", "injury"),
    retain = c("state", "age", "airbag", "injury", "restraint",
        "sex", "inimpact", "modelyr"), progress = TRUE)
}
\arguments{
  \item{years}{
Years for which data is required
}
  \item{prefix}{
Prefix for file name.
}
\item{compareBYvar}{
  Variables to be included in output, selected from \code{airbagAvail},
  \code{airbagDeploy} and \code{Restraint}), for which deaths are to
  be compared between drivers (w/wo
}
  \item{bycat}{
    Maps \code{airbag} and \code{restraint} codes to \code{yes}, \code{no}
    or \code{leaveout}
}
  \item{restrict}{
Allows restriction of data to specified variable subsets.
}
  \item{restrictvars}{
    character vector: names of variables that appear in the \code{restrict}
    argument
}
  \item{retain}{
Retain these columns in the output data
}
\item{progress}{Print year by year details of the progress of calculations.}
}
\details{
  This function is designed for processing data obtained from the FARS url
  noted under references.  This function was used to generate the data
  in the \code{\link{FARS}} data frame. Two of the datasets from which
  the \code{FARS} dataset was generated are included with this package
-- these are \code{fars2007} and \code{fars2008}
}
\value{
  \item{data}{Data frame, with driver information matched against
    passenger information for the same vehicle}
  \item{miss}{3-way table holding missing data information. The table
    is has margins \code{state}, a set of
variable names, and \code{years}}
}
\references{
\url{http://www-fars.nhtsa.dot.gov/Main/index.aspx}
}
\author{
John Maindonald
}
\seealso{
\code{plotFars}, \code{FARS}, \code{fars2007}
}
\examples{
farsMatch0708 <- matchedPairs(years=2007:2008)
}
\keyword{manip}

