## ---- data-subset

# subset to complete cases
raw_data_phase_two <- raw_data_phase_two[complete.cases(raw_data_phase_two),]

# subset to variables used to derive predictors
raw_data_phase_two_subset <- raw_data_phase_two[, c(key_var, 
                                                    complications_delivery, 
                                                    complications_maternal, 
                                                    complications_fetal, 
                                                    pre_existing_risk_fetal, 
                                                    pre_existing_risk_maternal)]

## ---- derive-predictor-variables

# derive indicators of complications
raw_data_phase_two_subset$complications_delivery <- 
  ((rowSums(raw_data_phase_two_subset[, complications_delivery] == 1, na.rm=T) > 0) * 1)

raw_data_phase_two_subset$complications_maternal <- 
  ((rowSums(raw_data_phase_two_subset[, complications_maternal] == 1, na.rm=T) > 0) * 1)

raw_data_phase_two_subset$complications_fetal <- 
  ((rowSums(raw_data_phase_two_subset[, complications_fetal] == 1, na.rm=T) > 0) * 1)

# derive indicators of risk
raw_data_phase_two_subset$pre_existing_risk_fetal <- 
  ((rowSums(raw_data_phase_two_subset[, pre_existing_risk_fetal] == 1, na.rm=T) > 0) * 1)

raw_data_phase_two_subset$pre_existing_risk_maternal <- 
  ((rowSums(raw_data_phase_two_subset[, pre_existing_risk_maternal] == 1, na.rm=T) > 0) * 1)

data_complete_cases <- raw_data_phase_two_subset

## ---- format-variables

data_complete_cases$group_collapse <- ifelse(data_complete_cases$group %in% c("KG", "KG-ET"), 
                                            0, 1)

data_complete_cases$group_collapse <- factor(data_complete_cases$group_collapse, 
                                             levels = c(0, 1), 
                                             labels = c("control", "intervention"))

data_complete_cases$complications_delivery <- factor(data_complete_cases$complications_delivery, 
                                                      levels = c(0, 1), 
                                                      labels = c("no-complications", "complications"))

data_complete_cases$complications_maternal <- factor(data_complete_cases$complications_maternal, 
                                                      levels = c(0, 1), 
                                                      labels = c("no-complications", "complications"))

data_complete_cases$complications_fetal <- factor(data_complete_cases$complications_fetal, 
                                                      levels = c(0, 1), 
                                                      labels = c("no-complications", "complications"))

data_complete_cases$complications_drg <- factor(data_complete_cases$complications_drg, 
                                                  levels = c(0, 1), 
                                                  labels = c("no-complications", "complications"))

data_complete_cases$pre_existing_risk_fetal <- factor(data_complete_cases$pre_existing_risk_fetal, 
                                                      levels = c(0, 1), 
                                                      labels = c("low", "high"))

data_complete_cases$pre_existing_risk_maternal <- factor(data_complete_cases$pre_existing_risk_maternal, 
                                                      levels = c(0, 1), 
                                                      labels = c("low", "high"))

data_complete_cases$complications_delivery <- 
  as.factor(data_complete_cases$complications_delivery)

data_complete_cases$complications_maternal <- 
  as.factor(data_complete_cases$complications_maternal)

data_complete_cases$complications_fetal <- 
  as.factor(data_complete_cases$complications_fetal)

data_complete_cases$complications_drg <- 
  as.factor(data_complete_cases$complications_drg)

data_complete_cases$pre_existing_risk_fetal <- 
  as.factor(data_complete_cases$pre_existing_risk_fetal)

data_complete_cases$pre_existing_risk_maternal <- 
  as.factor(data_complete_cases$pre_existing_risk_maternal)

data_complete_cases$first_birth <- factor(data_complete_cases$first_birth, 
                                          levels = c(0, 1), 
                                          labels = c("no", "yes"))

data_complete_cases$time_birth <- factor(data_complete_cases$time_birth, 
                                         levels = c(0, 1), 
                                         labels = c("core-hours", "after-hours"))

data_complete_cases$multiple_birth <- factor(data_complete_cases$multiple_birth, 
                                         levels = c(0, 1), 
                                         labels = c("no", "yes"))

data_complete_cases$missed_due_date <- factor(data_complete_cases$missed_due_date, 
                                             levels = c(0, 1), 
                                             labels = c("no", "yes"))

## ---- data-subset-final

# subset to predictor variables

data_complete_cases <- data_complete_cases[, c(
  key_var, 
  "group_collapse",
  "complications_delivery", 
  "complications_maternal", 
  "complications_fetal", 
  "pre_existing_risk_fetal", 
  "pre_existing_risk_maternal"
)]

attach(data_complete_cases)
data_complete_cases <- data_complete_cases[order(id), ]
detach(data_complete_cases)
