\name{simreg}
\alias{simreg}
\alias{bootreg}
\title{
Simulate (repeated) regression calculations
}
\description{
  Derive parameter estimates and standard errors by simulation, or
  by bootstrap resampling.
}
\usage{
simreg(formula, data, nsim = 1000)
bootreg(formula, data, nboot = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
Model formula
}
  \item{data}{
Data frame from which names in formula can be taken
}
\item{nsim}{Number of repeats of the simulation (\code{simreg})}
\item{nboot}{Number of bootstrap resamples (\code{bootreg})}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
  Matrix of coefficients from repeated simulations, or from bootstrap
  resamples.  For \code{simreg} there is one row for each repeat
  of the simulation.  For \code{bootreg} there is one row for each
  resample.
}
\references{
\url{https://maths-people.anu.edu.au/~johnm/nzsr/taws.html}
}
\author{
John Maindonald
}
\note{
Note that \code{bootreg} uses the simplest
  possible form of bootstrap.  For any except very large datasets,
  standard errors may be substantial under-estimates
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
xy <- data.frame(x=rnorm(100), y=rnorm(100))
simcoef <- simreg(formula = y~x, data = xy, nsim = 100)
bootcoef <- bootreg(formula = y~x, data = xy, nboot = 100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
