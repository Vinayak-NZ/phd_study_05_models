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

sub_01 <- sub_01[, c("drg_yield", "Group.Assignment", "Delivery.Complications", "Risks.Maternal")]
sub_02 <- sub_02[, c("drg_yield", "Group.Assignment", "Delivery.Complications", "Risks.Fetal")]
sub_03 <- sub_03[, c("drg_yield", "Group.Assignment", "Age.of.mother", "Time.of.birth")]
sub_04 <- sub_04[, c("drg_yield", "Group.Assignment", "Gestation.Time", "Risks.Maternal")]

tree_01 <- rpart(drg_yield ~ ., data = sub_01)
tree_02 <- rpart(drg_yield ~ ., data = sub_02)
tree_03 <- rpart(drg_yield ~ ., data = sub_03)
tree_04 <- rpart(drg_yield ~ ., data = sub_04)

# output-trees

png(file="output/random_forest_demo_plot.png")

par(mar = c(2.5, 2.5, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 5, 3, 6), ncol=2), heights=c(1, 3, 1, 3))
plot.new()
text(0.2, 0.2, "Random forest representation", cex = 3, font = 2, col = "#2F2E41")
rpart.plot(tree_01, type=5)
plot.new()
rpart.plot(tree_02, type=5)
rpart.plot(tree_03, type=5)
rpart.plot(tree_04, type=5)

dev.off()