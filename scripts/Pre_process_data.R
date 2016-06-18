#' ---
#' title: "pre-process"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#+
#knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

#' * to run use rmarkdown::render("scripts/Pre_process_data.R") 
#' * Or just press "compile notebook" button in Rstudio
#+
library(caret)
y_ind = 1
x_ind = 2:ncol(DF_for_prep)

#' * Read this http://topepo.github.io/caret/preprocess.html. 
#' * Only some of these methods were used here

#+
#' ### Creating dummy variables

#' Prepare a formula for interaction terms if needed
#+
incl_var <- "(AnimalType + PopName + BreedSH + BreedL + BreedD + BreedMix + Breed2)"
library(stringr)
form <- str_c("~ " ,  str_c(names(DF_for_prep)[-1], collapse = " + "), 
              "+ AgeOnOutcome:", incl_var,
              "+ AgeOnOutcome2:", incl_var,
              "+ AgeOnOutcome3:", incl_var,
              "+ AgeOnOutcome12:", incl_var,
              "+ AgeOnOutcome13:", incl_var) 
form.f <- as.formula(form)

#' ### Deal with NA
#' we can do knn imputing, but in this case just remove NAs 
#+
DF_for_prep <- na.omit(DF_for_prep)

#' ### Split data to x and y
#+
names(DF_for_prep)[y_ind] <- "outcome"
y_data <- DF_for_prep[, y_ind]
x_data <- DF_for_prep[, x_ind]


#' ###  Create dummmy variables
#' before splitting the data.
#' Otherwise train function will create dummies on the fly. But, in this case, training set will not
#' have the same number of levels for each factor. This will create different number of predictros.
#' The same is true for cross-validation. 
#+
if(use_formula == TRUE){
  dv <- dummyVars( form.f , data = x_data, fullRank = T) 
  # without fullRank we will have linearly dependent colomns: color will have TRUE and FALSE
}else{
  dv <- dummyVars( ~ . , data = x_data, fullRank = T)  
}

x_data <- data.frame(predict(dv, x_data))


#' ### Check for correlated predictors
#' if correlated or linear dependent will remain glm will give error liek
#' "glm.fit: fitted probabilities numerically 0 or 1 occurred"
#+
descrCor <- cor(x_data)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
x_data <- x_data[,-highlyCorDescr]

#' ### Check for linear dependencies
#+
comboInfo <- findLinearCombos(x_data)

if(length(comboInfo$remove) > 0) 
  x_data <- x_data[, -comboInfo$remove]

if(exists("for_pred")){
  x_data_pred <- x_data[nrow(x_data), ]
  y_data_pred <- y_data[nrow(x_data), ]
  x_data <- x_data[-nrow(x_data), ]
  y_data <- y_data[-nrow(x_data), ]
}
  

#' ###  Split data to train and test set
trainIndex <- createDataPartition(y_data$outcome, p = .8, list = F)
x_data_train <- x_data[ trainIndex,]
x_data_test  <- x_data[-trainIndex,]

y_data_train <- y_data[ trainIndex,]
y_data_test <- y_data[ -trainIndex,]


#' ### If needed Center and scale, or range (0 to 1)
#' dummyVars comand created dummies as num. 
#' If we want to scale data, we either should recode them as factors 
#' or use range comand instead (will map data to interval (0, 1))
#+ 
#preProcValues <- preProcess(x_data_train, method = c("range"))
#x_data_test <- predict(preProcValues, x_data_test)
#x_data_train <- predict(preProcValues, x_data_train)

#' How large is the df now
#+
cat("New DF dimensions \n", dim(x_data_train), "\n")
cat("n/p ratio is \n", dim(x_data_train)[1]/dim(x_data_train)[2], "\n")

#' Data are ready
#+
df_train <- data.frame(x_data_train, y_data_train)
df_test <- data.frame(x_data_test, y_data_test)
