## ---- create-xgboost-model

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
xgb_params_default <- list(
  objective = "reg:squarederror",  
  max_depth = 6,                    
  eta = 0.3
)

xgb_model_default <- xgboost(data = as.matrix(train_key_vars[, -1]), 
                     label = train_key_vars$drg_yield, 
                     params = xgb_params_default, 
                     nrounds = 100)


# fine tune parameters
param_grid <- expand.grid(
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  subsample = c(0.6, 0.8, 1),
  colsample_bytree = c(0.6, 0.8, 1),
  nrounds = c(100, 200, 300),             
  min_child_weight = c(1, 3, 5)           
)

ctrl <- trainControl(method = "cv", number = 5) 

xgb_tune <- train(
  x = train_key_vars[, -1],   
  y = train_key_vars[, 1], 
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid
)

print(xgb_tune$bestTune)

xgb_params_tuned <- list(
  objective = "reg:squarederror",  
  max_depth = 6,                    
  eta = 0.1, 
  gamma = 0.2, 
  colsample_bytree = 1, 
  min_child_weight = 5, 
  subsample = 0.6
)

xgb_model <- xgboost(data = as.matrix(train_key_vars[, -1]), 
                     label = train_key_vars[, 1], 
                     nrounds = 300, 
                     params = xgb_params_tuned)


