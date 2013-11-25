\name{CVcluster}
\alias{CVcluster}
\title{
Cross-validation estimate of predictive accuracy for clustered data
}
\description{
  This function adapts cross-validation to work with clustered
  categorical outcome data. For example, there may be multiple
  observations on individuals (clusters). It requires a
  fitting function that accepts a model formula.
}
\usage{
CVcluster(formula, id, data, na.action=na.omit, nfold = 15, FUN = lda,
              predictFUN=function(x, newdata, ...)predict(x, newdata, ...)$class,
              printit = TRUE, cvparts = NULL, seed = 29)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Model formula
}
  \item{id}{
numeric, identifies clusters
}
  \item{data}{
data frame that supplies the data
}
  \item{na.action}{\code{na.fail} (default) or \code{na.omit}
}
  \item{nfold}{
Number of cross-validation folds
}
  \item{FUN}{
\code{function} that fits the model
}
  \item{predictFUN}{
\code{function} that gives predicted values
}
  \item{printit}{
Should summary information be printed?
}
  \item{cvparts}{
    Use, if required, to specify the precise folds used for the
    cross-validation.  The comparison between different models
    will be more accurate if the same folds are used.
}
  \item{seed}{
Set seed, if required, so that results are exactly reproducible
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
\item{class}{Predicted values from cross-validation}
\item{CVaccuracy}{Cross-validation estimate of accuracy}
\item{confusion}{Confusion matrix}
}
\references{
\url{http:/www.maths.anu.edu.au/~johnm/nzsr/taws.html}
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
if(require(mlbench)&require(MASS)){
data(Vowel)
acc <- CVcluster(formula=Class ~., id = V1, data = Vowel, nfold = 15, FUN = lda,
              predictFUN=function(x, newdata, ...)predict(x, newdata, ...)$class,
                     printit = TRUE, cvparts = NULL, seed = 29)
}
}
\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
