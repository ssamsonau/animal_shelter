#' ---
#' title: "train_ROC"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#' ### Script for trees model training
#+
train_ROC <- function(DF, error_est = "cv", model = "rpart", par = FALSE){
  library(caret)
  set.seed(3456)
  
  #   Parallel processing
  if(par){
    require(foreach); require(doParallel)
    cl <- makeCluster(3) #use 3 cores
    registerDoParallel(cl)
  }
 
  #  Set up training
  fitControl <- trainControl(method = error_est,
                             number = 10,
                             classProbs = TRUE, # TRUE for ROC
                             summaryFunction = twoClassSummary, ## twoClassSummary this for ROC metric
                             verbose = TRUE)

  #  Train the model
  lmFit <- train(outcome ~ ., data = DF,
                 method = model,
                 metric= "ROC", # "ROC" for AUC
                 trControl = fitControl)
  if(par){
    stopCluster(cl)
  }
  return(list(model = lmFit))
}
