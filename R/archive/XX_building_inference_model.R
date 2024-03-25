## ---- prepare-data

data_complete_cases_subset <- data_complete_cases

# filter dataset
data_complete_cases_complications <- 
  data_complete_cases_subset[data_complete_cases_subset$group %in% c("IG", "KG", "KG-ET") & 
                               data_complete_cases_subset$complications_indicator == "complications", ]

data_complete_cases_no_complications <- 
  data_complete_cases_subset[data_complete_cases_subset$group %in% c("IG", "KG", "KG-ET") & 
                               data_complete_cases_subset$complications_indicator == "no-complications", ]

## ---- build-model

# with complications
model_bayes_complications <- stan_glm(drg_yield ~ 
                         first_birth + 
                         time_birth +
                         maternal_risk_other + 
                         fetal_risk +
                         group_collapse, 
                       data=data_complete_cases_complications, 
                       family = Gamma(link = "log"),
                       seed=555)

# without complications
model_bayes_no_complications <- stan_glm(drg_yield ~ 
                                        first_birth + 
                                        time_birth +
                                        maternal_risk_other + 
                                        fetal_risk +
                                        group_collapse, 
                                      data=data_complete_cases_no_complications, 
                                      family = Gamma(link = "log"),
                                      seed=555)

## ---- evaluate-model

# variation of residuals

intervention.res.comp = resid(model_bayes_complications)
plot(fitted(model_bayes_complications), intervention.res.comp)
abline(0,0)

intervention.res.no.comp = resid(model_bayes_no_complications)
plot(fitted(model_bayes_no_complications), intervention.res.no.comp)
abline(0,0)

# normality of residuals

qqnorm(intervention.res.comp)
qqline(intervention.res.comp) 

qqnorm(intervention.res.no.comp)
qqline(intervention.res.no.comp) 

# outlier check
loo_bayes_1 <- loo::loo(model_bayes_complications)
plot(loo_bayes_1, label_points = TRUE)

loo_bayes_2 <- loo::loo(model_bayes_no_complications)
plot(loo_bayes_2, label_points = TRUE)

