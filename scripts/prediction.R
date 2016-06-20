#' ---
#' title: "prediction"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#+
knitr::opts_chunk$set(echo = TRUE, eval = T)

#` ## Make prediction for a certain animal
#` Here you can include Rshiny app
#+ 
DF_orig <- read_csv("data/animal/train.csv.gz")
#View(DF_orig[1, ])
subDF <- DF_orig[1, ]

#' ### Interactive editing, of not using Shiny
#+
# prepare data frame for interactive editing (only numeric and factors are allowed)
is_str <- !sapply(subDF, is.numeric)
subDF[, is_str] <-  subDF[, is_str] %>%
  mutate_each(funs(as.factor))
# edit an entry interactivelly
subDF <- edit(subDF)

#' Entered record
#+
subDF %>%
  select(Name, AnimalType, SexuponOutcome, AgeuponOutcome, Breed, Color)

#+
#convert back to string
subDF[, is_str] <-  subDF[, is_str] %>%
  mutate_each(funs(as.character))

# can be done better, but a fast way
DF_orig[nrow(DF_orig) +1 , ] <- subDF[1, ]

source("scripts/making_features_4.R")

x_data_predict <- DF_binary[nrow(DF_binary), ]

# Preprocess with the prepared rules
x_data_predict <- data.frame(predict(dv, x_data_predict))
x_data_predict <- x_data_predict[,-highlyCorDescr]
if(length(comboInfo$remove) > 0) 
  x_data_predict <- x_data_predict[, -comboInfo$remove]
#x_data_predict <- predict(preProcValues, x_data_predict)
  
pred_adoption_prob <- predict(res_GLM$model, x_data_predict, type = "prob")
pred_adoption_prob
pred_adoption_prob$yes
