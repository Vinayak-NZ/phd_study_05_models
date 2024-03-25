data_complete_cases$maternal_risk_other <- 
  ((rowSums(data_complete_cases[, maternal_risk] == 1, na.rm=T) > 0) * 1)

data_complete_cases$maternal_risk_other <- 
  factor(data_complete_cases$maternal_risk, 
         levels = c(0, 1), 
         labels = c("no-risk", "at-risk"))

data_complete_cases$fetal_risk <- 
  ((rowSums(data_complete_cases[, fetal_risk] == 1, na.rm=T) > 0) * 1)

data_complete_cases$fetal_risk <- 
  factor(data_complete_cases$fetal_risk, 
         levels = c(0, 1), 
         labels = c("no-risk", "at-risk"))
