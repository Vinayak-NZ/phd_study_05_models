## ----apply-glm-stepwise

train_key_vars <- train[, 
                        c("group_collapse",
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
                          "pre_existing_risk_maternal", 
                          "drg_yield")]

# model-all
model_all <- lm(drg_yield ~ ., 
              data = train_key_vars)

model_all_summary <- summary(model_all)

model_coefficients <- as.data.frame(model_all_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove first variable
model_1 <- update(model_all, . ~ . - complications_delivery)

model_1_summary <- summary(model_1)

model_coefficients <- as.data.frame(model_1_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove second variable
model_2 <- update(model_1, . ~ . - pre_existing_risk_fetal)

model_2_summary <- summary(model_2)

model_coefficients <- as.data.frame(model_2_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove third variable
model_3 <- update(model_2, . ~ . - first_birth)

model_3_summary <- summary(model_3)

model_coefficients <- as.data.frame(model_3_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove fourth variable
model_4 <- update(model_3, . ~ . - complications_fetal)

model_4_summary <- summary(model_4)

model_coefficients <- as.data.frame(model_4_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove fifth variable
model_5 <- update(model_4, . ~ . - multiple_birth)

model_5_summary <- summary(model_5)

model_coefficients <- as.data.frame(model_5_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove sixth variable
model_6 <- update(model_5, . ~ . - age_delivery)

model_6_summary <- summary(model_6)

model_coefficients <- as.data.frame(model_6_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove seventh variable
model_7 <- update(model_6, . ~ . - time_birth)

model_7_summary <- summary(model_7)

model_coefficients <- as.data.frame(model_7_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove eigth variable
model_8 <- update(model_7, . ~ . - complications_maternal)

model_8_summary <- summary(model_8)

model_coefficients <- as.data.frame(model_8_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# remove ninth variable
model_9 <- update(model_8, . ~ . - missed_due_date)

model_9_summary <- summary(model_9)

model_coefficients <- as.data.frame(model_9_summary$coefficients)

model_coefficients$variable_name <- rownames(model_coefficients)

rownames(model_coefficients) <- NULL

max_p_value <- model_coefficients[model_coefficients$`Pr(>|t|)`==  max(model_coefficients$`Pr(>|t|)`), ]

# final_model
final_model <- lm(drg_yield ~ group_collapse + 
                  gestation_days + 
                  pre_existing_risk_maternal, 
                data = train_key_vars)

final_model_summary <- summary(final_model)
