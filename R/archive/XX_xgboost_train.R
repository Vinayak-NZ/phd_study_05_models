
xgb_model_test <- train(x = data.matrix(subset(train_key_vars, 
                                               select = -drg_yield)),
                        y = train_key_vars$drg_yield, 
                        method = "xgbTree", 
                        metric = "Rsquared",
                        trControl = trainControl(method = "cv", number = 5),
                        tuneLength = 10)


