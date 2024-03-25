## ----evaluate-xgboost

test_key_vars <- test[, 
                      c("drg_yield",
                        "group_collapse",
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
                        "pre_existing_risk_maternal")]

# Make predictions on the test set
predictions <- predict(nn_model, 
                       test_key_vars[, -which(names(test_key_vars) == "drg_yield")])

# Calculate RMSE
rmse <- sqrt(mean((test_key_vars$drg_yield - predictions)^2))
