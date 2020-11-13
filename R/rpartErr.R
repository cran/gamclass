#' Calculate Error Rates for rpart model
#'
#' Given an rpart model object, calculate training set error,
#' 10-fold cross-validation error, and test set error.
#' 
#' @param train.rp Fitted lda model object.
#' @param train Training set data frame.
#' @param test Test set data frame.
#' @param group Factor that identifies groups
#' @return Vector that holds training set error,
#' 10-fold cross-validation error, and test set error
#' rates.
#' 
#' @examples
#' \dontrun{
#' data(spam, package='kernlab')
#' spam[,-58] <- scale(spam[,-58])
#' nr <- sample(1:nrow(spam))
#' spam01 <- spam[nr[1:3601],]     ## Use for training,
#' ## if holdout not needed
#' spam2 <- spam[nr[3602:4601],]   ## Test
#' spam01.rp <- rpart(type~., data=spam01, cp=0.0001)
#' rpRates <- rpartErr(train.rp=spam01.rp, train=spam01, test=spam2,
#'                     group='type')
#' }
#' 
rpartErr <- function(train.rp, train, test, group='type'){
  cptab <- train.rp$cptable
  nbest <- which.min(cptab[,"xerror"])
  rnprop <- prop.table(table(train.rp$y))
  xcv <- cptab[nbest,"xerror"] * min(rnprop)
  trainerr <- cptab[nbest,"rel error"] * min(rnprop)
  class2 <- predict(train.rp, newdata=test, type="class")
  testerr <- 1-confusion(test[, group], class2, printit=FALSE,
                         prior=rnprop)$overall
  c(cverror=xcv, trainerror=trainerr, testerror=testerr)
}
