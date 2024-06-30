## ---- descriptive-cost-comparisons

# split-control-intervention

data_control <- data_complete_cases[data_complete_cases$group_collapse == "control", ]

data_int <- data_complete_cases[data_complete_cases$group_collapse == "intervention", ]

# mean

mean(data_control$drg_yield)

mean(data_int$drg_yield)

# range-sd

summary(data_control$drg_yield)

sd(data_control$drg_yield)

summary(data_int$drg_yield)

sd(data_int$drg_yield)
