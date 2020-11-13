#' Calculate Error Rates for Linear Discriminant Model
#'
#' Given an lda model object, calculate training set error,
#' leave-one-out cross-validation error, and test set error.
#' 
#' @param train.lda Fitted lda model object.
#' @param train Training set data frame.
#' @param test Test set data frame.
#' @param group Factor that identifies groups in training data.
#' @return Vector that holds leave-one-out, training, and test error
#' rates
#' 
#' @examples
#' \dontrun{
#' data(spam, package='kernlab')
#' spam[,-58] <- scale(spam[,-58])
#' nr <- sample(1:nrow(spam))
#' spam01 <- spam[nr[1:3601],]     ## Use for training,
#' spam2 <- spam[nr[3602:4601],]   ## Test
#' spam01.lda <- lda(type~., data=spam01)
#' ldaRates <- ldaErr(train.lda=spam01.lda, train=spam01, test=spam2, group="type")
#' }
#' 
ldaErr <- function(train.lda, train, test, group='type'){
  traingp <- train[,group]
  testgp <- test[,group]
  trainCV.lda <- update(train.lda, CV=TRUE)
  prior01 <- train.lda$prior
  ldaRates <- c(loo=1-confusion(traingp,
                                trainCV.lda$class,
                                printit=NULL)$overall,
                trainerr=1-confusion(traingp,
                                     predict(train.lda)$class,
                                     printit=NULL)$overall,
                testerr=1-confusion(testgp,
                                    predict(train.lda,
                                            newdata=test)$class,
                                    prior=prior01, printit=NULL)$overall)
  ldaRates
}
