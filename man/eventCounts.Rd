\name{eventCounts}
\alias{eventCounts}
\title{
Tabulate vector of dates by specified time event
}
\description{
  For example, dates may be dates of plane crashes.  For purposes
  of analysis, this function tabulates number of crash events
  per event of time, for each successive specified event.
}
\usage{
eventCounts(data, dateCol="Date", from = NULL, to = NULL,
          by = "1 month", categoryCol=NULL, takeOnly=NULL, prefix="n_")
}
\arguments{
  \item{data}{
    Data frame that should include any columns whose names appear in other
    function arguments.
}
  \item{dateCol}{
Name of column that holds vector of dates
}
  \item{from}{
    Starting date. If \code{NULL} set to first date given. If supplied,
    any rows earlier than \code{from} will be omitted.  Similarly, rows
    later than any supplied date \code{to} will be omitted.
}
  \item{to}{
    Final date, for which numbers of events are to be tallied. If
    \code{NULL} set to final date given.
}
  \item{by}{
    Time event to be used; e.g. "1 day", or "1 week", or "4 weeks",
    or "1 month", or "1 quarter", or "1 year", or "10 years".
  }
\item{categoryCol}{
  If not \code{NULL} create one column of counts for each level (or if
  not a factor, unique value).
}
\item{takeOnly}{
  If not \code{NULL}, a charater string that when deparsed and executed
  will return a vector of logicals.
}
\item{prefix}{
  If \code{categoryCol} is not \code{NULL}, a prefix for the names of the
  columns of counts.  Otherwise (\code{categoryCol=NULL}) a name for the
  column of counts.
}
}
\value{
  A data frame, with columns \code{Date} (the first day of the
  event for which events are given), and other column(s) that
  hols counts of events.
}
\author{
John Maindonald
}
\seealso{
\code{\link{cut}}
}
\examples{
crashDate <- as.Date(c("1908-09-17","1912-07-12","1913-08-06",
                       "1913-09-09","1913-10-17"))
df <- data.frame(date=crashDate)
byYears <- eventCounts(data=df, dateCol="date",
                       from=as.Date("1908-01-01"),
                       by="1 year")
}
\keyword{manip}
\keyword{chron}
