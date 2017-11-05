#' A function to calculate the accuracy of a given classifier model 
#' 
#' @param fit Classifier model object which should be testet. 
#'            Currently working: rpart and random forest
#' @param testDT Data.frame which contains the predicting variables
#' @param outcomeVar Outcome variable -class- of test data set. This variable is used to test the prediction
#' @return Returns a list containing the predicted outcome, the cross validation table and the accuracy
#' @examples 
#' 
#' 
#' @export

modelAccuracy <- function(fit, testDT, outcomeVar) {
  predict <- predict(fit, testDT, type = "class")
  pred.table <- table(predict,testDT[,outcomeVar])
  accuracy <- mean(predict == testDT[,outcomeVar])
  
  res <- list(predicted = predict,
              pred.table = pred.table,
              accuracy = accuracy)

  return(invisible(res))
}

