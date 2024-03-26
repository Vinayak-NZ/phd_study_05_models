## ---- format-for-modelling-ml

# convert variables to numeric for modelling with machine learning
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