## ----create-random-forest-model

train_key_vars <- train[, 
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

# default parameters

predictors <- names(train_key_vars)[2:14]
target <- "drg_yield"  

rf_model <- randomForest(
  formula = as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
  data = train_key_vars,
  ntree = 100,  
  importance = TRUE 
)

print(rf_model)

# fine tune parameters
param_grid <- expand.grid(
  mtry = c(4, 6, 8)           
)

ctrl <- trainControl(method = "cv", 
                     number = 5)  

rf_tune <- train(
  drg_yield ~ .,  
  data = train_key_vars,
  method = "rf",
  trControl = ctrl,
  tuneGrid = param_grid
)

print(rf_tune$bestTune)

final_rf_model <- randomForest(
  drg_yield ~ .,
  data = train_key_vars,
  ntree = 100,
  mtry = 4
)

