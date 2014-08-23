\name{addhlines}
\alias{addhlines}
\title{
Add horizontal lines to plot.
}
\description{
  This is designed for adding horizontal lines that show predicted
  values to a plot of observed values versus x-values,
  in \code{\link{rpart}} regression. Where predicted values change
  between two successive x-values lines are extended to the midway
  point. This reflects the way that \code{\link{predict.rpart}}
  handles predictions for new data.
}
\usage{
addhlines(x, y, ...)
}
\arguments{
  \item{x}{
Vector of predictor variable values.
}
  \item{y}{
Vector of predicted values.
}
  \item{\dots}{
Additional graphics parameters, for passing through to the
\code{lines()}
function.
}
}
\value{
Lines are added to the current graph.
}
\author{
John Maindonald
}
\examples{
x <- c(34, 18, 45, 18, 27, 24, 34, 20, 24, 28, 21, 18)
y <- c(14, 11, 12, 9, 4, 11, 6, 9, 4, 10, 9, 2)
hat <- c(10.5, 7.75, 10.5, 7.75, 7, 7, 10.5, 7.75, 7, 10.5, 7, 7.75)
plot(x, y)
addhlines(x, hat, lwd=2, col="gray")

## The function is currently defined as
function(x,y, ...){
  ordx <- order(x)
  xo <- x[ordx]
  yo <- y[ordx]
  breaks <- diff(yo)!=0
  xh <- c(xo[1],0.5*(xo[c(FALSE,breaks)]+xo[c(breaks, FALSE)]))
  yh <- yo[c(TRUE, breaks)]
  y3 <- x3 <- numeric(3*length(xh)-1)
  loc1 <- seq(from=1, to=length(x3), by=3)
  x3[loc1] <- xh
  x3[loc1+1]<- c(xh[-1], max(x))
  x3[loc1[-length(loc1)]+2] <- NA
  y3[loc1[-length(loc1)]+2] <- NA
  y3[loc1] <- yh
  y3[loc1+1] <- yh
  lines(x3,y3, ...)
}
}
\keyword{graphics}

