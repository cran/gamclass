\name{gamRF}
\alias{gamRF}
\title{
Random forest fit to residuals from GAM model
}
\description{
Fit model using \code{gam()} from mgcv, then use random forest
regression with residuals.  Check perfomance of this hybrid model
for predictions to \code{newdata}, if supplied.
}
\usage{
gamRF(formlist, yvar, data, newdata = NULL, rfVars, method = "GCV.Cp", 
    printit = TRUE, seed = NULL)
}
\arguments{
  \item{formlist}{
List of rght hand sides of formulae for GAM models.
}
  \item{yvar}{
Character string holding y-variable name.
}
  \item{data}{
Data
}
  \item{newdata}{
Optionally, supply test data.
}
  \item{rfVars}{
    Names of explanatory variables for the \code{randomForest} model.
}
  \item{method}{
    Smoothing parameter estimation method for use of \code{gam()}. See
    \code{\link{gam}}.
}
  \item{printit}{
Should a summary of results (error rates) be printed?
}
  \item{seed}{
Set a seed to make result repeatable.
}
}
\value{
  A vector of test data accuracies for the hybrid models (one for each
  element of \code{formlist}), plus test error mean square and OOB error
  mean square for the use of \code{randomForest()}.
}
\references{
  J. Li, A. D. Heap, A. Potter and J. J. Daniell. 2011. Application
  of Machine Learning Methods to Spatial Interpolation of Environmental
  Variables. Environmental Modelling and Software 26: 1647-1656.
  DOI: 10.1016/j.envsoft.2011.07.004.
}
\author{
John Maindonald <john.maindonald@anu.edu.au>
}
\note{
  The best results are typically obtained when a relatively low
  degree of freedom GAM model is used.  It seems advisable to
  use those variables for the GAM fit that seem likely to be
  similar in their effect irrespective of geographic location.
}
\seealso{
\code{\link{CVgam}}
}
\examples{
if(length(find.package("sp", quiet=TRUE))>0){
data("meuse", package="sp")
meuse <- within(meuse, {levels(soil) <- c("1","2","2")
                        ffreq <- as.numeric(ffreq)
                        loglead <- log(lead)}
)
form <- ~ dist + elev + ffreq + soil
rfVars <- c("dist", "elev", "soil", "ffreq", "x", "y")
## Select 90 out of 155 rows
sub <- sample(1:nrow(meuse), 90)
meuseOut <- meuse[-sub,]
meuseIn <- meuse[sub,]
gamRF(formlist=list("lm"=form), yvar="loglead", rfVars=rfVars,
                    data=meuseIn, newdata=meuseOut)
}

## The function is currently defined as
function (formlist, yvar, data, newdata = NULL, rfVars, method = "GCV.Cp", 
    printit = TRUE, seed = NULL) 
{   if(!is.null(seed))set.seed(seed)
    errRate <- numeric(length(formlist)+2)
    names(errRate) <- c(names(formlist), "rfTest", "rfOOB")
    ytrain <- data[, yvar]
    xtrain <- data[, rfVars]
    xtest <- newdata[, rfVars]
    ytest = newdata[, yvar]
    res.rf <- randomForest(x = xtrain, y = ytrain, 
                           xtest=xtest, 
                           ytest=ytest)
    errRate["rfOOB"] <- mean(res.rf$mse)
    errRate["rfTest"] <- mean(res.rf$test$mse)    
    GAMhat <- numeric(nrow(data))
    for(nam in names(formlist)){
      form <- as.formula(paste(c(yvar, paste(formlist[[nam]])), collapse=" "))
      train.gam <- gam(form, data = data, method = method)
      res <- resid(train.gam)
      cvGAMms <- sum(res^2)/length(res)
      if (!all(rfVars \%in\% names(newdata))) {
        missNam <- rfVars[!(rfVars \%in\% names(newdata))]
        stop(paste("The following were not found in 'newdata':", 
                   paste(missNam, collapse = ", ")))
      }
      GAMtesthat <- predict(train.gam, newdata = newdata)
      GAMtestres <- ytest - GAMtesthat
      Gres.rf <- randomForest(x = xtrain, y = res, xtest = xtest, 
                              ytest = GAMtestres)
      errRate[nam] <- mean(Gres.rf$test$mse)
    }
    if (printit) 
        print(round(errRate, 4))
    invisible(errRate)
}
}
\keyword{models}
\keyword{regression}
