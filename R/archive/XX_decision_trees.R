## ---- prepare-data

risk_factors <- c(
  maternal_risk,
  fetal_risk,
  "first_birth", 
  "time_birth", 
  "missed_due_date"
)

predictor_var <- "group_collapse"

outcome_var <- "drg_yield"

# filter dataset
data_complete_cases_subset <- data_complete_cases

data_complete_cases_subset <- data_complete_cases_subset[data_complete_cases_subset$group %in% c("IG", "KG", "KG-ET") & 
                                                        data_complete_cases_subset$complications_indicator == "complications", ]

# remove outlier
cost_summary <- summary(data_complete_cases_subset[[outcome_var]])
cost_iqr <- IQR(data_complete_cases_subset[[outcome_var]])

tmin = cost_summary[2] - (1.5*cost_iqr)
tmax = cost_summary[5] + (1.5*cost_iqr)

data_complete_cases_subset <- 
  data_complete_cases_subset[which(data_complete_cases_subset[[outcome_var]] > tmin &
                                     data_complete_cases_subset[[outcome_var]] < tmax), ]

data_complete_cases_subset$group_collapse <- ifelse(data_complete_cases_subset$group_collapse == "control", 0, 1)

data_complete_cases_subset <- as.data.frame(data_complete_cases_subset)
model_input <- data.matrix(data_complete_cases_subset)

model_input <- model_input[, c(risk_factors, predictor_var)]

label <- data_complete_cases_subset[, outcome_var]

# get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(label) * .7)

# training data
train_data <- model_input[1:numberOfTrainingSamples,]
train_labels <- label[1:numberOfTrainingSamples]

# testing data
test_data <- model_input[-(1:numberOfTrainingSamples),]
test_labels <- label[-(1:numberOfTrainingSamples)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

#default parameters
params <- list(booster = "gbtree", 
                 objective = "reg:squarederror", 
                 eta=0.3, gamma=0, 
                 max_depth=6, 
                 min_child_weight=1, 
                 subsample=1, 
                 colsample_bytree=1)

xgbcv <- xgb.cv(params = params, 
                data = dtrain, 
                nrounds = 100, 
                nfold = 5, 
                showsd = T, 
                stratified = T, 
                print_every_n = 10, 
                early_stop_round = 20, 
                maximize = F)

xgb1 <- xgb.train(params = params, 
                   data = dtrain, 
                   nrounds = 98, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stop_round = 10, 
                   maximize = F , 
                   eval_metric = "rmse")


#model prediction
xgbpred <- predict(xgb1,dtest)

# get & print the classification error
compare_pred <- as.data.frame(cbind(xgbpred, test_labels))
err <- sqrt(mean((compare_pred$test_labels - compare_pred$xgbpred)^2))/(max(compare_pred$test_labels) - min(compare_pred$test_labels))
print(paste("test-error=", err))

model <- xgboost(data = dtrain, 
                 nround = 98, 
                 params = list(
                   eta = 0.3,
                   max_depth = 6
                 ),
                 objective = "reg:squarederror")

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
compare_pred <- as.data.frame(cbind(pred, test_labels))
err <- sqrt(mean((compare_pred$test_labels - compare_pred$pred)^2))/(max(compare_pred$test_labels) - min(compare_pred$test_labels))
print(paste("test-error=", err))

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(train_data), model = model)
importance_matrix

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

