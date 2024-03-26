## ----apply-bayesian

train_key_vars <- train[, 
                        c("drg_yield",
                          "group_collapse",
                          "age_delivery",
                          "gestation_days",
                          "time_birth",
                          "first_birth", 
                          "multiple_birth",
                          "missed_due_date", 
                          "complications_delivery", 
                          "complications_maternal", 
                          "complications_fetal", 
                          "pre_existing_risk_fetal", 
                          "pre_existing_risk_maternal")]

# find-predictors-BIC

n <- nrow(train_key_vars)
bayesian_variable_select <- lm(drg_yield ~ ., data=train_key_vars)
bayesian_variable_final <- step(bayesian_variable_select, k=log(n)) 

# formulate-priors-intervention-effect

before_costs <- raw_data_phase_one[raw_data_phase_one$year == 2018, "drg_yield_total"]
after_costs <- raw_data_phase_one[raw_data_phase_one$year == 2020, "drg_yield_total"]

before_costs_repeated_samples <- replicate(1000, sample(before_costs, 30, replace = TRUE))
after_costs_repeated_samples <- replicate(1000, sample(after_costs, 30, replace = TRUE))

before_costs_repeated_means <- colMeans(before_costs_repeated_samples)
after_costs_repeated_means <- colMeans(after_costs_repeated_samples)

differences_repeated <- after_costs_repeated_means - before_costs_repeated_means
mean_difference_repeated <- round(mean(differences_repeated), 2)
sd_differences_repeated <- round(sd(differences_repeated), 2)

prior_specification <- paste0("normal(", mean_difference_repeated, ",", sd_differences_repeated, ")")

print(paste0("prior mean", " = ", mean_difference_repeated))
print(paste0("prior sd", " = ", sd_differences_repeated))

prior <- prior("normal(-229.01, 245.81)", class = "b", coef = "group_collapse1")

# formulate-priors-intervention-effect

bayesian_model <- brm(drg_yield ~ group_collapse + 
               pre_existing_risk_maternal + 
               gestation_days, 
             data = train_key_vars, 
             prior = prior)

summary(bayesian_model)


