\name{compareModels}
\alias{compareModels}
\title{Compare accuracy of alternative classification methods}
\description{
  Compare, between models, probabilities that the models assign to
  membership in the correct group or class. Probabilites should be
  estimated from cross-validation or from bootstrap out-of-bag data
  or preferably for test data that are completely separate from the
  data used to dervive the model.
}
\usage{
compareModels(groups, estprobs = list(lda = NULL, rf = NULL),
            gpnames = NULL, robust = TRUE, print = TRUE)
}
\arguments{
  \item{groups}{Factor that specifies the groups}
  \item{estprobs}{
    List whose elements (with names that identify the models) are
    matrices that give for each observation (row) estimated
    probabilities of membership for each of the groups (columns).}
    \item{gpnames}{Character: names for groups, if different from
    \code{levels(groups)}}
  \item{robust}{Logical, \code{TRUE} or \code{FALSE}}
  \item{print}{Logical.  Should results be printed?}
}
\details{
The estimated probabilities are compared directly,  under normal
distribution assumptions.  An effect is fitted for each observation,
plus an effect for the method.  Comparison on a logit scale may
sometimes be preferable.  An option to allow this is scheduled for
incorporation in a later version.
}
\value{
\item{modelAVS}{Average accuracies for models}
\item{modelSE}{Approximate average SE for comparing models}
\item{gpAVS}{Average accuracies for groups}
\item{gpSE}{Approximate average SE for comparing groups}
\item{obsEff}{Effects assigned to individual observations}
}
\author{
John Maindonald
}
\note{
  The analysis estimates effects due to model and group (\code{gp}),
  after accounting for differences between observations.
}
\examples{
library(MASS)
library(DAAG)
library(randomForest)
ldahat <- lda(species ~ length+breadth, data=cuckoos, CV=TRUE)$posterior
qdahat <- qda(species ~ length+breadth, data=cuckoos, CV=TRUE)$posterior
rfhat <- predict(randomForest(species ~ length+breadth, data=cuckoos),
                 type="prob")
compareModels(groups=cuckoos$species, estprobs=list(lda=ldahat,
              qda=qdahat, rf=rfhat), robust=FALSE)
}
\keyword{statistics}
\keyword{multivariate}
