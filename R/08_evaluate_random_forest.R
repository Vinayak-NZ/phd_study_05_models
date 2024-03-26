## ----evaluate-random-forest

test_key_vars <- test[, 
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

# make predictions to test set
predictions <- predict(final_rf_model, newdata = test_key_vars)

# Evaluate the model (e.g., RMSE)
rmse <- sqrt(mean((test_key_vars$drg_yield - predictions)^2))

