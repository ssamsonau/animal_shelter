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
#subDF$OutcomeType <- ""
#subDF$OutcomeSubtype <- ""
#subDF$AnimalID <- ""
#subDF$DateTime <- ""

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

DF_for_prep <- DF_binary

for_pred = TRUE
source("scripts/Pre_process_data.R")

pred_adoption_prob <- predict(res_GLM$model, x_data_pred, type = "prob")
pred_adoption_prob
