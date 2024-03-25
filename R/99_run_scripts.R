## ---- run-key-scripts

source("R/00_load_functions.R")
source("R/00_load_package.R")
source("R/01_load_data.R")
source("R/02_create_variable.R")
source("R/03_create_variable_groupings.R")
source("R/04_data_edit.R")

## ---- split-data-set
source("R/05_split_data.R")

## ---- glm
source("R/06_build_glm.R")
source("R/07_evaluate_glm.R")

## ---- bayesian
source("R/06_build_bayesian.R")
source("R/07_evaluate_bayesian.R")

## ---- random-forest
source("R/06_build_random_forest.R")
source("R/07_evaluate_random_forest.R")

## ---- xgboost
source("R/06_build_xgboost.R")
source("R/07_evaluate_xgboost.R")

## ---- visualise
source("R/08_visualise_socio_demographics.R")
