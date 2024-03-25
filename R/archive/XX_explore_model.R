## ---- explore-inference-model-complications

# model estimates
color_scheme_set("purple")
mcmc_intervals(model_bayes_complications, pars = c("group_collapseintervention", 
                                     "first_birthyes", 
                                     "time_birthyes",
                                     "maternal_risk_otherat-risk", 
                                     "fetal_riskat-risk"), 
               transformations = list("group_collapseintervention" = "back_transform", 
                                      "first_birthyes" = "back_transform", 
                                      "time_birthyes" = "back_transform", 
                                      "maternal_risk_otherat-risk" = "back_transform", 
                                      "fetal_riskat-risk" = "back_transform")) + 
  scale_y_discrete(labels=c("back_transform(group_collapseintervention)" = "intervention", 
                            "back_transform(first_birthyes)" = "first birth", 
                            "back_transform(time_birthyes)" = "after hours delivery", 
                            "back_transform(maternal_risk_otherat-risk)" = "other maternal risks", 
                            "back_transform(fetal_riskat-risk)" = "fetal risks")) +
  labs(title = "Predictors of obstetric costs", 
       subtitle = "Plots of credible intervals for model estimates", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  theme(axis.text.x = element_text(color="#205B87", 
                                   size=10),
        axis.text.y = element_text(color="#205B87", 
                                   size=10)) +
  xlab("Percentage change in cost") + 
  theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")) 


## ---- intervention-effect-detail-complications

post <- get_parameters(model_bayes_complications)

all_ci_comp <- hdi(model_bayes_complications)
intervention_comp_ci <- all_ci_comp[all_ci_comp$Parameter == "group_collapseintervention", ]
intervention_comp_lower <- back_transform(intervention_comp_ci$CI_low)
intervention_comp_upper <- back_transform(intervention_comp_ci$CI_high)

posterior_comp <- as.array(model_bayes_complications)

color_scheme_set("blue")
mcmc_hist(posterior_comp, 
          pars = c("group_collapseintervention"), 
          transformations = list("group_collapseintervention" = "back_transform")) +
  labs(title = "Impact of intervention on cost", 
       subtitle = "Histogram of posteriors", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  theme(axis.text.x = element_text(color="#205B87", 
                                   size=10),
        axis.text.y = element_text(color="#205B87", 
                                   size=10)) +
  xlab("Percentage change in cost") + 
  scale_fill_manual("#A4BFD2") +
  theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")) +
  vline_at(back_transform(median(post$group_collapse)), size = 1, col="#E13530") +
  vline_at(intervention_comp_lower, size = 2, col="#205B87",  linetype = 2) +
  vline_at(intervention_comp_upper, size = 2, col="#205B87", linetype = 2)

## ---- explore-inference-model-no-complications

# model estimates
color_scheme_set("purple")
mcmc_intervals(model_bayes_no_complications, pars = c("group_collapseintervention", 
                                                   "first_birthyes", 
                                                   "time_birthyes",
                                                   "maternal_risk_otherat-risk", 
                                                   "fetal_riskat-risk"), 
               transformations = list("group_collapseintervention" = "back_transform", 
                                      "first_birthyes" = "back_transform", 
                                      "time_birthyes" = "back_transform", 
                                      "maternal_risk_otherat-risk" = "back_transform", 
                                      "fetal_riskat-risk" = "back_transform")) + 
  scale_y_discrete(labels=c("back_transform(group_collapseintervention)" = "intervention", 
                            "back_transform(first_birthyes)" = "first birth", 
                            "back_transform(time_birthyes)" = "after hours delivery", 
                            "back_transform(maternal_risk_otherat-risk)" = "other maternal risks", 
                            "back_transform(fetal_riskat-risk)" = "fetal risks")) +
  labs(title = "Predictors of obstetric costs", 
       subtitle = "Plots of credible intervals for model estimates", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  theme(axis.text.x = element_text(color="#205B87", 
                                   size=10),
        axis.text.y = element_text(color="#205B87", 
                                   size=10)) +
  xlab("Percentage change in cost") + 
  theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")) 


## ---- intervention-effect-detail-no-complications

post <- get_parameters(model_bayes_no_complications)

all_ci_no_comp <- hdi(model_bayes_no_complications)
intervention_ci_no_comp <- all_ci_no_comp[all_ci_no_comp$Parameter == "group_collapseintervention", ]
intervention_lower_no_comp <- back_transform(intervention_ci_no_comp$CI_low)
intervention_upper_no_comp <- back_transform(intervention_ci_no_comp$CI_high)

posterior_no_comp <- as.array(model_bayes_no_complications)

color_scheme_set("blue")
mcmc_hist(posterior_no_comp, 
          pars = c("group_collapseintervention"), 
          transformations = list("group_collapseintervention" = "back_transform")) +
  labs(title = "Impact of intervention on cost", 
       subtitle = "Histogram of posteriors", 
       caption = "Data source: Aktionbündis Patientensicherheit") +
  theme(axis.text.x = element_text(color="#205B87", 
                                   size=10),
        axis.text.y = element_text(color="#205B87", 
                                   size=10)) +
  xlab("Percentage change in cost") + 
  scale_fill_manual("#A4BFD2") +
  theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")) +
  vline_at(back_transform(median(post$group_collapse)), size = 1, col="#E13530") +
  vline_at(intervention_lower_no_comp, size = 2, col="#205B87",  linetype = 2) +
  vline_at(intervention_upper_no_comp, size = 2, col="#205B87", linetype = 2)
