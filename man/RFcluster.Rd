\name{RFcluster}
\alias{RFcluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random forests estimate of predictive accuracy for clustered data
}
\description{
  This function adapts random forests to work (albeit clumsily and
  inefficiently) with clustered categorical outcome data. For
  example, there may be multiple observations on individuals (clusters).
  Predictions are made fof the OOB (out of bag) clusters
}
\usage{
RFcluster(formula, id, data, nfold = 15,
              ntree=500, progress=TRUE, printit = TRUE, seed = 29)
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
\item{nfold}{numeric, number of folds
}
\item{ntree}{numeric, number of trees (number of bootstrap samples)
}
  \item{progress}{
Print information on progress of calculations
}
  \item{printit}{
Print summary information on accuracy
}
\item{seed}{Set seed, if required, so that results are exactly reproducible}
}
\details{
  Bootstrap samples are taken of observations in the in-bag clusters.
  Predictions are made for all observations in the OOB clusters.
}
\value{
\item{class}{Predicted values from cross-validation}
\item{OOBaccuracy}{Cross-validation estimate of accuracy}
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
\dontrun{
data(Vowel)
RFcluster(formula=Class ~., id = V1, data = Vowel, nfold = 15,
              ntree=500, progress=TRUE, printit = TRUE, seed = 29)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
