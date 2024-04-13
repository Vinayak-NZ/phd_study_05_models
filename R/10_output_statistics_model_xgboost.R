## ----output-xgboost

# output-statistics

mse <- mean((test_key_vars$drg_yield - predictions)^2)
sigma_mse <- (1/nrow(test_key_vars)) * var((test_key_vars$drg_yield - predictions)^2)

mse_u <- mse + 1.96 * sqrt(sigma_mse)
mse_l <- mse - 1.96 * sqrt(sigma_mse)

# format output for variable importance plot
variable_importance <- xgb.importance(model = xgb_model)

variable_importance$variable <- variable_importance$Feature

variable_importance$importance <- variable_importance$Frequency

variable_importance <- variable_importance[, c("variable", 
                                               "importance")]

variable_importance$variable <- gsub("gestation_days", 
                                     "Gestation days", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("complications_delivery", 
                                     "Delivery complications", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("complications_maternal", 
                                     "Maternal complications", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("missed_due_date", 
                                     "Missed due date", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("group_collapse", 
                                     "Group assignment", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("complications_fetal", 
                                     "Fetal complications", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("time_birth", 
                                     "Time of birth", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("age_delivery", 
                                     "Age", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("pre_existing_risk_maternal", 
                                     "Maternal risk profile", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("multiple_birth", 
                                     "Multiple births", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("first_birth", 
                                     "First birth", 
                                     variable_importance$variable)

variable_importance$variable <- gsub("pre_existing_risk_fetal", 
                                     "Fetal risk profile", 
                                     variable_importance$variable)

# variable-importance-plot

xgboost_feature_importance <- 
  ggplot(variable_importance, aes(x = reorder(variable, importance), 
                                y = importance)) + 
  geom_col(fill = "#46e7fd") +
  labs(title = "XGBoost output", 
       subtitle = "Feature importance plot",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Variable") + 
  ylab("Importance (Frequency of feature selection)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) + 
  coord_flip()

ggsave("output/xgboost_feature_importance.png", 
       plot = xgboost_feature_importance)

# output estimates of each predictor effect on outcome variable

xgb_model_test <- train(x = data.matrix(subset(train_key_vars, 
                                               select = -drg_yield)),
                        y = train_key_vars$drg_yield, 
                        method = "xgbTree", 
                        metric = "RMSE",
                        trControl = trainControl(method = "cv", number = 5),
                        tuneGrid = param_grid)

pdp.lstat <- partial(xgb_model_test, pred.var = "group_collapse", plot = TRUE, rug = TRUE)


pdp.lstat <- partial(xgb_model_test, pred.var = c("group_collapse", "time_birth"), plot = TRUE, rug = TRUE)


pdp_plot_model <- Predictor$new(xgb_model_test, data = train_key_vars)

pdp_effect_group_collapse <- 
  FeatureEffect$new(pdp_plot_model, 
                    feature = "group_collapse", 
                    method = "pdp", 
                    grid.size = 30)

pdp_effect_gestation_days <- 
  FeatureEffect$new(pdp_plot_model, 
                    feature = "gestation_days", 
                    method = "pdp", 
                    grid.size = 30)

pdp_effect_intervention_complications <- 
  FeatureEffect$new(pdp_plot_model, 
                    feature = c("group_collapse", "complications_delivery"), 
                    method = "pdp", 
                    grid.size = 30)


# pdp-plot-intervention

pdp_effect_group_collapse_df <- 
  data.frame(group_collapse = c(0, 1), 
             prediction = c(pdp_effect_group_collapse$results[1, 2], pdp_effect_group_collapse$results[30, 2]), 
             frequency = c(nrow(train_key_vars[train_key_vars$group_collapse == 0, ]), nrow(train_key_vars[train_key_vars$group_collapse == 1, ])))

intervention_labels <- c("No participation", "Participation")

pdp_xgboost_intervention <- 
  ggplot(pdp_effect_group_collapse_df, aes(x = group_collapse, 
                                  y = prediction)) + 
  geom_col(fill = "#46e7fd") + 
  labs(title = "Partial dependence plot", 
       subtitle = "Partial effect of intervention on cost of care predictions",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Communication intervention") +
  scale_x_discrete(labels = intervention_labels) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        axis.title.y = element_blank()) + 
  geom_rug(data = train_key_vars, aes(x = group_collapse, y = 0), 
           position = "jitter",
           sides = "b", inherit.aes = F)

ggsave("output/pdp_xgboost_intervention.png", 
       plot = pdp_xgboost_intervention)

# pdp-plot-gestation-days

pdp_xgboost_gestation <- 
  pdp_effect_gestation_days$plot() +
  labs(title = "Partial dependence plot", 
       subtitle = "Partial effect of gestation days on cost of care predictions",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Gestation period (days)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        axis.title.y = element_blank())

ggsave("output/pdp_xgboost_gestation.png", 
       plot = pdp_xgboost_gestation)

# pdp-plot-intervention-complications

complication_labels <- c("No", "Yes")

processed_intervention_complications_plot <- pdp_effect_intervention_complications$plot()

processed_intervention_complications_plot$layers[[2]] <- NULL

pdp_xgboost_intervention_complications <- 
  processed_intervention_complications_plot +
  labs(title = "Partial dependence plot", 
       subtitle = "Interaction effects of intervention and delivery complications on cost of care predictions",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Communication intervention") + 
  ylab("Complications during delivery") + 
  scale_x_continuous(breaks = c(0.25, 0.75), labels = intervention_labels) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = complication_labels) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.title = element_blank())

ggsave("output/pdp_xgboost_intervention_complications.png", 
       plot = pdp_xgboost_intervention_complications)