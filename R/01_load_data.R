## ---- data-load
raw_data_phase_one <- read.csv("input/phase_one_costs.csv", sep = ";")
raw_data_phase_two <- read.csv("input/phase_two_costs.csv")
drg_classification <- read.csv("metadata/drg_classification.csv", sep = ";")
