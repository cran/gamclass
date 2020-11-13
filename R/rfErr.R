#' Calculate Error Rates for randomForest model
#'
#' Given an randomForest model object, calculate training set error,
#' out-of-bag (OOB) error, and test set error.
#' 
#' @param train.rf Fitted lda model object.
#' @param train Training set data frame.
#' @param test Test set data frame.
#' @param group Factor that identifies groups
#' @return Vector that holds training set error,
#' out-of-bag (OOB) error, and test set error rates.
#' 
#' @examples
#' \dontrun{
#' data(spam, package='kernlab')
#' spam[,-58] <- scale(spam[,-58])
#' nr <- sample(1:nrow(spam))
#' spam01 <- spam[nr[1:3601],]     ## Use for training,
#' spam2 <- spam[nr[3602:4601],]   ## Test
#' spam01.rf <- randomForest(type ~ ., data=spam01)
#' rfRates <- rfErr(train.rf=spam01.rf, train=spam01, test=spam2,
#'                  group='type')
#' }
#' 
rfErr <- function(train.rf, train, test, group='type'){
  trainClass <- predict(train.rf, newdata=train, type="class")
  testClass <- predict(train.rf, newdata=test, type="class")
  rnprop <- prop.table(table(train[, group]))
  OOBerr <- as.vector(train.rf$err.rate[train.rf$ntree, "OOB"])
  rfRates <- c(OOBerr=OOBerr,
               trainerr=1-confusion(train[,group], trainClass,
                                    printit=FALSE)$overall,
               testerr=1-confusion(test$type, testClass, printit=FALSE,
                                   prior=rnprop)$overall)
  rfRates
}

