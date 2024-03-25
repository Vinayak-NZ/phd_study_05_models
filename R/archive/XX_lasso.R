library(glmnet)

# Separate the predictors (X) and the response variable (y)
X <- as.matrix(train_key_vars[, -1])  # Assuming the response variable is in the first column
y <- train_key_vars[, 1]  # Assuming the response variable is in the first column

# Fit LASSO model
lasso_model <- glmnet(X, y, alpha = 1)


# Perform cross-validation to select lambda
cv_model <- cv.glmnet(X, y, alpha = 1)

# Plot the cross-validation results
plot(cv_model)

# Select the lambda with the lowest cross-validated error
best_lambda <- cv_model$lambda.min

# Extract coefficients for the selected lambda
selected_coef <- coef(lasso_model, s = best_lambda)

# Identify selected predictors (non-zero coefficients)
selected_predictors <- names(selected_coef[selected_coef != 0])

# Use selected predictors for further analysis
selected_data <- train_key_vars[, selected_predictors]
