## ---- split-training-test-sets

# conver variables to numeric for modelling
data_complete_cases$group_collapse <- 
  ifelse(data_complete_cases$group_collapse == "control", 0, 1)

data_complete_cases$time_birth <- 
  ifelse(data_complete_cases$time_birth == "core-hours", 0, 1)

data_complete_cases$first_birth <- 
  ifelse(data_complete_cases$first_birth == "no", 0, 1)

data_complete_cases$multiple_birth <- 
  ifelse(data_complete_cases$multiple_birth == "no", 0, 1)

data_complete_cases$missed_due_date <- 
  ifelse(data_complete_cases$missed_due_date == "no", 0, 1)

data_complete_cases$complications_drg <- 
  ifelse(data_complete_cases$complications_drg == "no-complications", 0, 1)

data_complete_cases$complications_delivery <- 
  ifelse(data_complete_cases$complications_delivery == "no-complications", 0, 1)

data_complete_cases$complications_maternal <- 
  ifelse(data_complete_cases$complications_maternal == "no-complications", 0, 1)

data_complete_cases$complications_fetal <- 
  ifelse(data_complete_cases$complications_fetal == "no-complications", 0, 1)

data_complete_cases$pre_existing_risk_fetal <- 
  ifelse(data_complete_cases$pre_existing_risk_fetal == "low", 0, 1)

data_complete_cases$pre_existing_risk_maternal <- 
  ifelse(data_complete_cases$pre_existing_risk_maternal == "low", 0, 1)

# make this example reproducible
set.seed(5)

# use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), 
                 nrow(data_complete_cases), 
                 replace=TRUE, 
                 prob=c(0.7,0.3))

train  <- data_complete_cases[sample, ]
test   <- data_complete_cases[!sample, ]

