#' ---
#' title: "train_Kappa"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#' ### Script for linear model training
#+
train_Kappa <- function(DF, error_est = "none", model = "lm", par = FALSE){
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
  #                           classProbs = TRUE, # TRUE for ROC
  #                           summaryFunction = twoClassSummary, ## twoClassSummary this for ROC metric
                             verbose = TRUE)

  #  Train the model
  lmFit <- train(outcome ~ ., data = DF,
                 method = model,
                 metric= "Kappa",
  #              preProc = c("scale"),
                 trControl = fitControl)
  if(par){
    stopCluster(cl)
  }
  return(list(model = lmFit))
}
