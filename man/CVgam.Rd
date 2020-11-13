\name{CVgam}
\alias{CVgam}
\title{
Cross-validation estimate of accuracy from GAM model fit
}
\description{
  The cross-validation estimate of accuracy is sufficiently independent of
  the available model fitting criteria (including Generalized
  Cross-validation) that it provides a useful check on the extent of
  downward bias in the estimated standard error of residual.
}
\usage{
CVgam(formula, data, nfold = 10, debug.level = 0, method = "GCV.Cp",
              printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Model formula, for passing to the \code{gam()} function
}
\item{data}{
data frame that supplies the data
}
\item{nfold}{
Number of cross-validation folds
}
\item{debug.level}{
See \code{\link{gam}} for details
}
\item{method}{
Fit method for GAM model.  See \code{\link{gam}} for details
}
  \item{printit}{
Should summary information be printed?
}
  \item{cvparts}{
    Use, if required, to specify the precise folds used for the
    cross-validation.  The comparison between different models
    will be more accurate if the same folds are used.
}
\item{gamma}{
See \code{\link{gam}} for details.
}
\item{seed}{
Set seed, if required, so that results are exactly reproducible
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
  \item{fitted}{fitted values}
  \item{resid}{residuals}
  \item{cvscale}{scale parameter from cross-validation}
  \item{scale.gam}{scale parameter from function \code{gam}}
  The scale parameter from cross-validation is the error mean square)
}
\references{
\url{https://maths-people.anu.edu.au/~johnm/nzsr/taws.html}
}
\author{
John Maindonald
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
if(require(sp)){
library(mgcv)
data(meuse)
meuse$ffreq <- factor(meuse$ffreq)
CVgam(formula=log(zinc)~s(elev) + s(dist) + ffreq + soil,
              data = meuse, nfold = 10, debug.level = 0, method = "GCV.Cp",
              printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)
}
}
\keyword{models}
\keyword{regression}
