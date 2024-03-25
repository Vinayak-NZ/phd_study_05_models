## ----apply-neuralnet

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

# default-parameters
formula <- as.formula("drg_yield ~ .")  

nn_model <- neuralnet(formula,
                      data = train_key_vars,
                      hidden = 1,  
                      linear.output = TRUE)


# fine tune parameters
param_grid <- expand.grid(
  layer1 = seq(1, 10, by = 3),  
  layer2 = seq(1, 10, by = 3),  
  layer3 = seq(1, 10, by = 3)   
)

ctrl <- trainControl(
  method = "cv",                    
  number = 5,                       
  summaryFunction = defaultSummary, 
  classProbs = FALSE,               
  savePredictions = TRUE            
)

nn_tune <- train(
  formula,
  data = train_key_vars,
  method = "neuralnet",
  trControl = ctrl,
  tuneGrid = param_grid
)

print(nn_tune$bestTune)

final_model <- neuralnet(formula, 
                         data = train_key_vars, 
                         hidden = c(4, 4, 1))

