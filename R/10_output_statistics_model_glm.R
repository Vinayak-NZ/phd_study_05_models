## ----output-glm

# create a data-frame with glm output
terms <- c("Intercept", 
           "Group assignment", 
           "Gestation days", 
           "Maternal risk profile")

summary_glm <- as.data.frame(summary(final_model)$coefficients)

confidence_intervals_glm <- confint(final_model)

output_glm_data_frame <- cbind(Term = terms, 
                               summary_glm, 
                               confidence_intervals_glm)

rownames(output_glm_data_frame) <- NULL

output_glm_data_frame <- output_glm_data_frame[-1, ]

# back transform glm output
vars <- c("Estimate", "Std. Error", "2.5 %", "97.5 %")

output_glm_data_frame[vars] <- lapply(output_glm_data_frame[vars], 
                                      function(x) {(exp(x)-1)*100})

names(output_glm_data_frame) <- c("Term", 
                                  "Estimate", 
                                  "SE", 
                                  "t", 
                                  "p", 
                                  "CI_lower", 
                                  "CI_upper")

nice_table(output_glm_data_frame)