## ----visualise-random-forest

# create-trees

sub_01 <- train[, c("drg_yield", "group_collapse", "complications_delivery", "pre_existing_risk_maternal")]
sub_02 <- train[, c("drg_yield", "group_collapse", "complications_delivery", "pre_existing_risk_fetal")]
sub_03 <- train[, c("drg_yield", "group_collapse", "age_delivery", "time_birth")]
sub_04 <- train[, c("drg_yield", "group_collapse", "gestation_days", "pre_existing_risk_maternal")]

sub_01$Group.Assignment <- as.factor(ifelse(sub_01$group_collapse == 0, "Control", "Intervention"))
sub_01$Delivery.Complications <- as.factor(ifelse(sub_01$complications_delivery == 0, "No", "Yes"))
sub_01$Maternal.Risk.Profile <- as.factor(ifelse(sub_01$pre_existing_risk_maternal == 0, "Low", "High"))

sub_02$Group.Assignment <- as.factor(ifelse(sub_02$group_collapse == 0, "Control", "Intervention"))
sub_02$Delivery.Complications <- as.factor(ifelse(sub_02$complications_delivery == 0, "No", "Yes"))
sub_02$Fetal.Risk.Profile <- as.factor(ifelse(sub_02$pre_existing_risk_fetal == 0, "Low", "High"))

sub_03$Group.Assignment <- as.factor(ifelse(sub_03$group_collapse == 0, "Control", "Intervention"))
sub_03$Age.of.mother <- sub_03$age_delivery
sub_03$Time.of.birth <- as.factor(ifelse(sub_03$time_birth == 0, "Core hours", "After hours"))

sub_04$Group.Assignment <- as.factor(ifelse(sub_04$group_collapse == 0, "Control", "Intervention"))
sub_04$Gestation.Time <- sub_04$gestation_days
sub_04$Maternal.Risk.Profile <- as.factor(ifelse(sub_04$pre_existing_risk_maternal == 0, "Low", "High"))

sub_01 <- sub_01[, c("drg_yield", "Group.Assignment", "Delivery.Complications", "Maternal.Risk.Profile")]
sub_02 <- sub_02[, c("drg_yield", "Group.Assignment", "Delivery.Complications", "Fetal.Risk.Profile")]
sub_03 <- sub_03[, c("drg_yield", "Group.Assignment", "Age.of.mother", "Time.of.birth")]
sub_04 <- sub_04[, c("drg_yield", "Group.Assignment", "Gestation.Time", "Maternal.Risk.Profile")]

tree_01 <- rpart(drg_yield ~ ., data = sub_01)
tree_02 <- rpart(drg_yield ~ ., data = sub_02)
tree_03 <- rpart(drg_yield ~ ., data = sub_03)
tree_04 <- rpart(drg_yield ~ ., data = sub_04)

# output-trees

png(file="output/random_forest_demo_plot_01.png", width = 500, heigh = 500)
par(mfrow = c(1, 2))
rpart.plot(tree_01, type=5, main = "Tree 1")
rpart.plot(tree_02, type=5, main = "Tree 2")
dev.off()

png(file="output/random_forest_demo_plot_02.png", width = 1000, heigh = 1000)
par(mfrow = c(1, 2))
rpart.plot(tree_03, type=5, main = "Tree 3")
rpart.plot(tree_04, type=5, main = "Tree 4")
dev.off()
