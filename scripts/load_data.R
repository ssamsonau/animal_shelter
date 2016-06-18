#' ---
#' title: "getting data"
#' author: "Sergey Samsonau"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#+
#knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

#+
library(readr)
DF_orig <- read_csv("data/animal/train.csv.gz")

dim(DF_orig)
head(DF_orig)
str(DF_orig)
