## ---- create-variable

# create id var
raw_data_phase_two$id <- c(1:nrow(raw_data_phase_two))

# merge drg classifications
raw_data_phase_two <- merge(raw_data_phase_two, drg_classification, by = "drg")

# create intervention variable
raw_data_phase_two$group <- factor(raw_data_phase_two$group, 
                                    levels = c("IG", "KG", "KG-ET"), 
                                    labels = c("IG", "KG", "KG-ET"))

# create gestation days variable
raw_data_phase_two <- derive_gestation_days(raw_data_phase_two)