## ----evaluate-glm

test_key_vars <- test[, 
                      c("group_collapse", 
                        "gestation_days", 
                        "pre_existing_risk_maternal", 
                        "drg_yield")]

# make predictions to test set
predictions <- predict(final_model, newdata = test_key_vars)

# Calculate RMSE
rmse <- sqrt(mean((test_key_vars$drg_yield - predictions)^2))
