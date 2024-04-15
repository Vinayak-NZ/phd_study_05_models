## ---- visualise-glm

input_best_fit <- test

# plot-line-best-fit
group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c(0, 1)

input_best_fit$pre_existing_risk_maternal <- 
  ifelse(input_best_fit$pre_existing_risk_maternal == 0, "Low", "High")

glm_best_fit_line <- 
  ggplot(data = input_best_fit, aes(x = gestation_days, y = drg_yield)) +
  geom_point(aes(colour = pre_existing_risk_maternal)) +
  scale_colour_manual(values = c("#46e7fd", "#e18b22")) +
  geom_smooth(method = "lm") +
  labs(title = "General linear model representation", 
       subtitle = "Line of best fit and standard errors for predicting cost of care",
       caption = "Data source: Obstetric Digital Health Intervention", 
       color = "Risk profile of mother") +
  xlab("Gestation days") + 
  ylab("Cost of care (Euro)") + 
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"))

# plot-predicted-observed
predicted_df <- data.frame(predicted = predict(final_model, test_key_vars, type = "response"), 
                           observed = test$drg_yield,
                           gestation_days = test$gestation_days, 
                           pre_existing_risk_maternal = test$pre_existing_risk_maternal, 
                           group_collapse = test$group_collapse)

predicted_df$group_collapse <- 
  ifelse(predicted_df$group_collapse == 0, "Control", "Intervention")

predicted_df$pre_existing_risk_maternal <- 
  ifelse(predicted_df$pre_existing_risk_maternal == 0, "Low", "High")

glm_pred_obs_plot <- 
  ggplot(predicted_df, aes(x=predicted, y= observed)) +
  geom_point(aes(colour = pre_existing_risk_maternal, shape = group_collapse)) +
  scale_colour_manual(values = c("#46e7fd", "#e18b22")) +
  geom_abline(intercept=0, slope=1) +
  labs(title = "General linear model representation", 
       subtitle = "Predicted vs observed values",
       caption = "Data source: Obstetric Digital Health Intervention", 
       shape = "Group assignment",
       color = "Risk profile of mother") +
  xlab("Predicted values") + 
  ylab("Observed values") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"))

ggsave("output/glm_best_fit_line.png", 
       plot = glm_best_fit_line)

ggsave("output/glm_pred_obs_plot.png", 
       plot = glm_pred_obs_plot)
