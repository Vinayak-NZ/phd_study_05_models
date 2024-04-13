## ----random-forest-pre-process

# format categorical variables as factors

data_complete_cases$group_collapse <- as.factor(data_complete_cases$group_collapse)

data_complete_cases$time_birth <- as.factor(data_complete_cases$time_birth)

data_complete_cases$first_birth <- as.factor(data_complete_cases$first_birth)

data_complete_cases$multiple_birth <- as.factor(data_complete_cases$multiple_birth)

data_complete_cases$missed_due_date <- as.factor(data_complete_cases$missed_due_date)

data_complete_cases$complications_delivery <- as.factor(data_complete_cases$complications_delivery)

data_complete_cases$complications_maternal <- as.factor(data_complete_cases$complications_maternal)

data_complete_cases$complications_fetal <- as.factor(data_complete_cases$complications_fetal)

data_complete_cases$pre_existing_risk_fetal <- as.factor(data_complete_cases$pre_existing_risk_fetal)

data_complete_cases$pre_existing_risk_maternal <- as.factor(data_complete_cases$pre_existing_risk_maternal)
