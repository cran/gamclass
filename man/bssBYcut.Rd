\name{bssBYcut}
\alias{bssBYcut}
\title{
Between group SS for \code{y}, for all possible splits on values of \code{x}
}
\description{
  Each point of separation between successve values of \code{x} is used
  in turn to create two groups of observations.  The between group sum
  of squares for \code{y} is calculated for each such split.
}
\usage{
bssBYcut(x, y, data)
}
\arguments{
  \item{x}{
    Variable (numeric) used to define splits. Observations with \code{x}
    values less than the cut point go into the first group, while those
    with values \code{>=} the cut point go into the second group.
}
  \item{y}{
Variable for which BSS values are to be calculated.
}
  \item{data}{
Data frame with columns \code{x} and \code{y}.
}
}
\value{
Data frame with columns:
\item{xOrd}{Cut points for splits.}
\item{comp2 }{Between groups sum of squares}
}
\author{
J H Maindonald
}
\examples{
xy <- bssBYcut(weight, height, women)
with(xy, xy[which.max(bss), ])

## The function is currently defined as
function (x, y, data)
{
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    xv <- data[, xnam]
    yv <- data[, ynam]
    sumss <- function(x, y, cut) {
        av <- mean(y)
        left <- x < cut
        sum(left) * (mean(y[left]) - av)^2 + sum(!left) * (mean(y[!left]) -
            av)^2
    }
    xOrd <- unique(sort(xv))[-1]
    bss <- numeric(length(xOrd))
    for (i in 1:length(xOrd)) {
        bss[i] <- sumss(xv, yv, xOrd[i])
    }
    list(xOrd = xOrd, bss = bss)
  }
}
\keyword{manip}

