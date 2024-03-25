raw_data_two_group_drg <- merge(raw_data_phase_two, drg_classification, by = "drg")

data_complete_cases <- raw_data_two_group_drg[complete.cases(raw_data_two_group_drg),]

data_complete_cases$group <- factor(data_complete_cases$group, 
                                       levels = c("IG", "KG", "KG-ET"), 
                                    labels = c("IG", "KG", "KG-ET"))

data_complete_cases$group_collapse <- ifelse(data_complete_cases$group %in% c("KG", "KG-ET"), 
                                            0, 1)
data_complete_cases$group_collapse <- factor(data_complete_cases$group_collapse, 
                                    levels = c(0, 1), 
                                    labels = c("control", "intervention"))

data_complete_cases$complications_indicator <- ifelse(rowSums(data_complete_cases[, complications] == 1, na.rm=T) > 0, 1, 
                                                      data_complete_cases$complications_indicator)

data_complete_cases$complications_indicator <- factor(data_complete_cases$complications_indicator, 
                                             levels = c(0, 1), 
                                             labels = c("no-complications", "complications"))

data_complete_cases$complications_indicator <- 
  as.factor(data_complete_cases$complications_indicator)

data_complete_cases$first_birth <- factor(data_complete_cases$first_birth, 
                                                      levels = c(0, 1), 
                                                      labels = c("no", "yes"))

data_complete_cases$time_birth <- factor(data_complete_cases$time_birth, 
                                          levels = c(0, 1), 
                                          labels = c("no", "yes"))

data_complete_cases <- prepare_data_factors(data_complete_cases, c(
  "underlying_maternal_disease", 
  "age_mother", 
  "first_birth", 
  "missed_due_date", 
  "obesity",
  "time_birth"))
