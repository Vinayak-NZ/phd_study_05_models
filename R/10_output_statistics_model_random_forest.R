## ----output-random-forest

# output-statistics

mse <- mean((test_key_vars$drg_yield - predictions)^2)
sigma_mse <- (1/nrow(test_key_vars)) * var((test_key_vars$drg_yield - predictions)^2)

mse_u <- mse + 1.96 * sqrt(sigma_mse)
mse_l <- mse - 1.96 * sqrt(sigma_mse)

# format output for variable importance plot
variable_importance <- as.data.frame(final_rf_model$importance)

variable_importance$variable <- rownames(variable_importance)

variable_importance$importance <- variable_importance$`%IncMSE`

rownames(variable_importance) <- NULL

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

random_forest_feature_importance <- 
  ggplot(variable_importance, aes(x = reorder(variable, importance), 
                                y = importance)) + 
  geom_col(fill = "#46e7fd") +
  labs(title = "Random forest output", 
       subtitle = "Feature importance plot",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Variable") + 
  ylab("Importance (% Increase of MSE)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) + 
  coord_flip()

ggsave("output/random_forest_feature_importance.png", 
       plot = random_forest_feature_importance)

# output estimates of each predictor effect on outcome variable

pdp_plot_model <- Predictor$new(final_rf_model, data = train_key_vars)

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

pdp_effect_complications_delivery <- 
  FeatureEffect$new(pdp_plot_model, 
                    feature = "complications_delivery", 
                    method = "pdp", 
                    grid.size = 30)

pdp_effect_intervention_complications <- 
  FeatureEffect$new(pdp_plot_model, 
                    feature = c("group_collapse", "complications_delivery"), 
                    method = "pdp", 
                    grid.size = 30)


# pdp-plot-intervention

intervention_labels <- c("No participation", "Participation")

pdp_rf_intervention <- 
  pdp_effect_group_collapse$plot() +
  labs(title = "Partial dependence plot", 
       subtitle = "Partial effect of intervention on cost of care predictions",
       caption = "Data source: Obstetric Digital Health Intervention") +
  scale_x_discrete(labels = intervention_labels) +
  xlab("Communication intervention") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        axis.title.y = element_blank())

ggsave("output/pdp_rf_intervention.png", 
       plot = pdp_rf_intervention)

# pdp-plot-gestation-days

pdp_rf_gestation <- 
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

ggsave("output/pdp_rf_gestation.png", 
       plot = pdp_rf_gestation)

# pdp-plot-intervention-complications

complication_labels <- c("No", "Yes")

pdp_rf_intervention_complications <- 
  pdp_effect_intervention_complications$plot() +
  labs(title = "Partial dependence plot", 
       subtitle = "Interaction effects of intervention and delivery complications on cost of care predictions",
       caption = "Data source: Obstetric Digital Health Intervention") +
  scale_x_discrete(labels = intervention_labels) +
  scale_y_discrete(labels = complication_labels) +
  xlab("Communication intervention") + 
  ylab("Complications during delivery") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.title=element_blank())

ggsave("output/pdp_rf_intervention_complications.png", 
       plot = pdp_rf_intervention_complications)