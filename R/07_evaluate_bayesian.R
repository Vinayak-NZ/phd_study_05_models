## ----evaluate-bayesian

test_key_vars <- test[, 
                      c("drg_yield",
                        "group_collapse",
                        "gestation_days",
                        "pre_existing_risk_maternal")]


# make predictions to test set
predictions <- data.frame(predict(model, newdata = test_key_vars))

# Evaluate the model (e.g., RMSE)
rmse <- sqrt(mean((test_key_vars$drg_yield - predictions$Estimate)^2))
