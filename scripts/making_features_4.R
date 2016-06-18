#' ---
#' title: "Code for data cleaning and feature engineering"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
#' to run use rmarkdown::render("data_preparation_4.R") - or just press "compile notebook" button in Rstudio

#+ Clean_data, include=FALSE
library(dplyr)
# Not informative predictors
DF <- DF_orig %>%
  select(-AnimalID, -DateTime)
str(DF)

#' Strings will be converted to factor levels, and to dummy var. 
#' In order to eliminate some errors, just replacing here all symbols by _

#+
library(stringr)
is_str <- !sapply(DF, is.numeric)
DF[, is_str] <-  DF[, is_str] %>%
  mutate_each(funs(str_replace(., " ", "_")))

#' ## Feature Engineering

#' ### Names

#' #### NAs

#+
# How many names are missing. 
# SO much data records will disapear during training a model if we don't do anything with NA.
sum(is.na(DF$Name))/nrow(DF)
DF <- DF %>%
  mutate(Name = ifelse(is.na(Name), "not_known", Name))


#' #### How many unique names. 

#' Each level of a factor will create a new variable. Do we need so many new variables?
#' Let us replace Name by a status of popular or not. Take the popular names from    
#' * http://www.youpet.com/cat-names/   
#' * http://www.youpet.com/dog-names/    
  
#+
length(unique(DF$Name))
pop_names_cat <- read.csv("data/animal/top_100_cat_names.txt") 
pop_names_dog <- read.csv("data/animal/top_100_dog_names.txt")   

library(stringr)
pop_names_cat <- str_trim( as.character(pop_names_cat[, 2]) )
pop_names_dog <- str_trim( as.character(pop_names_dog[, 2]) )

DF <- DF %>%
  mutate(PopNameD = AnimalType == "Dog" & Name %in% pop_names_dog) %>%
  mutate(PopNameC = AnimalType == "Cat" & Name %in% pop_names_cat) %>%
  mutate(PopName = ifelse(PopNameC | PopNameD, "yes", "no")) %>%
  mutate(PopName = ifelse(Name == "not_known", "not_known", PopName)) %>%
  select(-PopNameC, -PopNameD, -Name)

table(DF$PopName)

#' ### Breed
#+
length(unique(DF$Breed))
head(DF$Breed)

DF <- DF %>%
  mutate(BreedSH = factor( str_detect(Breed, "Shorthair")) ) %>%
  mutate(BreedL = factor( str_detect(Breed, "Long"))) %>%
  mutate(BreedD = factor( str_detect(Breed, "Domestic"))) %>%
  mutate(BreedMix = factor( str_detect(Breed, "Mix"))) %>%
  mutate(Breed2 = factor( str_detect(Breed, "/"))) %>%
  select(-Breed)

table(DF$BreedSH)
table(DF$BreedL)
table(DF$BreedD)
table(DF$BreedMix)
table(DF$Breed2)

head(DF)

#' ### Color
#+
length(unique(DF$Color))
#find unique words for color
un_col <- str_replace_all(DF$Color, "[^[:alpha:]]", " ")  %>%
  word() %>%
  table() 

un_col
un_col <- un_col[!un_col < 50]   
un_col_names <- names(un_col)

for(nnn in un_col_names){
  DF[, nnn] <- as.factor(str_detect(DF$Color, nnn))
}

DF$Color <- NULL

#' ### AgeuponOutcome
#+
DF <- DF %>%
  mutate(AgeOnOutcome = 
           as.numeric( str_extract(AgeuponOutcome, "[:digit:]+") ) *
           str_detect(AgeuponOutcome, "year")*360 +
           as.numeric( str_extract(AgeuponOutcome, "[:digit:]+") ) *
           str_detect(AgeuponOutcome, "month")*30 +
           as.numeric( str_extract(AgeuponOutcome, "[:digit:]+") ) *
           str_detect(AgeuponOutcome, "week")*7 + 
           as.numeric( str_extract(AgeuponOutcome, "[:digit:]+") ) *
           str_detect(AgeuponOutcome, "day")*1)

DF$AgeuponOutcome <- NULL
#View(DF)

#' ### non-linear features
# Let us add non-linear features for time.
#+
DF <- DF %>%
  mutate(AgeOnOutcome2 = AgeOnOutcome^2, 
         AgeOnOutcome12 = AgeOnOutcome^(1/2), 
         AgeOnOutcome3 = AgeOnOutcome^3, 
         AgeOnOutcome13 = AgeOnOutcome^(1/3))


#' ### Other predictors
#' THis can't be used in prediction and is not helpfull right now. Remove this for now..
#+
DF$OutcomeSubtype <- NULL

length(unique(DF$SexuponOutcome))

#' ### Convert factors to factors
#+
DF[, 1:4] <- lapply(DF[, 1:4], as.factor )

#' ## Dimension
#+
dim(DF)
str(DF)
sapply(DF, is.factor)

#` Final DF with features
#+
DF_features <- DF
rm(DF)

#' ## Create Binary outcome
#' Create data frame for binary classification
#+
# alive or not
DF_binary <- DF_features %>%
  mutate(OutcomeType = OutcomeType %in% c("Adoption")) %>%
  mutate(OutcomeType = ifelse(OutcomeType, "yes", "no")) 

#' ## Try later
#+
# alive or not. Classes will be unbalanced. Important to observe high accuracy even with a non-predicting model... just becaus of no-information rate
#DF_binary <- DF %>%
#  mutate(OutcomeType = OutcomeType %in% 
#           c("Adoption", "Return_to_owner", "Transfer")) %>%
#  mutate(OutcomeType = ifelse(OutcomeType, "yes", "no")) 

# outcome var should not be logical TRUE or FALSE, and should not be number
#otherwise caret gives error for twoClassSumamry
# like At least one of the class levels are not valid R variables names
# because TRUE and FALSE can't be names of col.

#View(DF_binary)
table(DF_binary$OutcomeType)
