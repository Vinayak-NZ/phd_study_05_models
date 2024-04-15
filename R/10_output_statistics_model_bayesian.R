## ----output-bayesian

# create a data-frame with glm output
terms <- c("Intercept", 
           "Group assignment", 
           "Maternal risk profile", 
           "Gestation days")

summary_bayes <- as.data.frame(summary(bayesian_model)$fixed)

output_bayes_data_frame <- cbind(Term = terms, 
                               summary_bayes)

rownames(output_bayes_data_frame) <- NULL

output_bayes_data_frame <- output_bayes_data_frame[-1, ]

# back transform glm output
names(output_bayes_data_frame) <- c("Term", 
                                  "Estimate", 
                                  "SE", 
                                  "CI_lower", 
                                  "CI_upper", 
                                  "Rhat", 
                                  "Bulk effective sample size", 
                                  "Tail effective sample size")

bayesian_table <- nice_table(output_bayes_data_frame)

print(bayesian_table, preview = "docx")
