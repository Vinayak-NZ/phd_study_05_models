## ---- transform-data

data_complete_cases_subset <- data_complete_cases
data_complete_cases_subset$drg_yield_tx <- log(data_complete_cases_subset$drg_yield)

# filter dataset
data_complete_cases_subset <- data_complete_cases_subset[data_complete_cases_subset$group %in% c("IG", "KG", "KG-ET") & 
                                                           data_complete_cases_subset$complications_indicator == "complications", ]


## ---- linear-model

# null model
null.lm <- lm(drg_yield_tx ~ maternal_risk_other + 
                first_birth + 
                time_birth + 
                missed_due_date +
                fetal_risk, 
              data = data_complete_cases_subset)

null_model <- summary(null.lm)

saveRDS(null_model, file="output/models/null_model.RData")

# intervention effect
intervention.lm <- lm(drg_yield_tx ~ maternal_risk_other + 
                        first_birth + 
                        time_birth + 
                        missed_due_date + 
                        fetal_risk +
                        group_collapse, 
              data = data_complete_cases_subset)

intervention_model <- summary(intervention.lm)

(exp(coef(intervention.lm)["group_collapseintervention"])-1)*100

intervention.res = resid(intervention_model)
plot(fitted(intervention.lm), intervention.res)
abline(0,0)

qqnorm(intervention.res)
qqline(intervention.res) 

saveRDS(intervention_model, file="output/models/intervention_model.RData")

null_intervention_comparison <- anova(null.lm, intervention.lm)

saveRDS(null_intervention_comparison, file="output/models/null_intervention_comparison.RData")