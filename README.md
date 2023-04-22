# lm-table-maker
R function to generate publication-ready figures from linear models


# to use the function pass in the data, the name of the dependent variable, and a vector of the names of the independent variables:
library(tidyverse)
library(kableExtra)

source("lm-table-maker-function.R")

generate_lm_table(data = , dependent_var = "", independent_vars = c("", "", ""))
