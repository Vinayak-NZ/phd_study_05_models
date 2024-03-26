## ---- format-for-modelling

# convert variables in training set to factors for modelling
train$group_collapse <- 
  as.factor(train$group_collapse)

train$time_birth <- 
  as.factor(train$time_birth)

train$first_birth <- 
  as.factor(train$first_birth)

train$multiple_birth <- 
  as.factor(train$multiple_birth)

train$missed_due_date <- 
  as.factor(train$missed_due_date)

train$complications_delivery <- 
  as.factor(train$complications_delivery)

train$complications_maternal <- 
  as.factor(train$complications_maternal)

train$complications_fetal <- 
  as.factor(train$complications_fetal)

train$pre_existing_risk_fetal <- 
  as.factor(train$pre_existing_risk_fetal)

train$pre_existing_risk_maternal <- 
  as.factor(train$pre_existing_risk_maternal)

# convert variables in test set to factors for predictions
test$group_collapse <- 
  as.factor(test$group_collapse)

test$time_birth <- 
  as.factor(test$time_birth)

test$first_birth <- 
  as.factor(test$first_birth)

test$multiple_birth <- 
  as.factor(test$multiple_birth)

test$missed_due_date <- 
  as.factor(test$missed_due_date)

test$complications_delivery <- 
  as.factor(test$complications_delivery)

test$complications_maternal <- 
  as.factor(test$complications_maternal)

test$complications_fetal <- 
  as.factor(test$complications_fetal)

test$pre_existing_risk_fetal <- 
  as.factor(test$pre_existing_risk_fetal)

test$pre_existing_risk_maternal <- 
  as.factor(test$pre_existing_risk_maternal)