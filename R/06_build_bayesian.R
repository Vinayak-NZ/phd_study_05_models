
train_key_vars <- train[, 
                        c("group_collapse",
                          "age_delivery",
                          "gestation_days",
                          "time_birth",
                          "first_birth", 
                          "multiple_birth",
                          "missed_due_date", 
                          "complications_drg", 
                          "complications_delivery", 
                          "complications_maternal", 
                          "complications_fetal", 
                          "pre_existing_risk_fetal", 
                          "pre_existing_risk_maternal", 
                          "drg_yield")]

library(brms)

before_costs <- raw_data_phase_one[raw_data_phase_one$year == 2018, "drg_yield_total"]
after_costs <- raw_data_phase_one[raw_data_phase_one$year == 2020, "drg_yield_total"]

mean_before <- mean(before_costs)
mean_after <- mean(after_costs)

differences <- mean_after - mean_before

sd_estimate <- 248.33

prior <- prior("normal(-243.63, 248.33)", class = b, coef = "group_collapse")

model <- brm(drg_yield ~ group_collapse + pre_existing_risk_maternal + gestation_days, 
             data = train_key_vars, 
             prior = prior)

summary(model)

before_costs_repeated_samples <- replicate(300, sample(before_costs, 30, replace = TRUE))
after_costs_repeated_samples <- replicate(300, sample(after_costs, 30, replace = TRUE))

before_costs_repeated_means <- colMeans(before_costs_repeated_samples)
after_costs_repeated_means <- colMeans(after_costs_repeated_samples)

differences_repeated <- after_costs_repeated_means - before_costs_repeated_means
sd_differences_repeated <- sd(differences_repeated)
