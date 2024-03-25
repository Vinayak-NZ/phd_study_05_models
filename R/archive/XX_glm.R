simple.fit = lm(drg_yield ~ pre_existing_risk + group_collapse, data=data_complete_cases)

summary(simple.fit)
