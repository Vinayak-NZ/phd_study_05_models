## ---- visualise-bayesian

# posterior-prior

prior <- rnorm(10000, mean = -229.01, sd = 245.81)
posterior <- rnorm(10000, mean = -422.60, sd = 199.75)

prior_data <- data.frame(values = prior, distribution = "Prior")
posterior_data <- data.frame(values = posterior, distribution = "Posterior")

distribution_compare <- rbind(prior_data, posterior_data)

ggplot(distribution_compare, aes(x = values, fill = distribution)) +
  geom_area(aes(y = ..density..), stat = "bin", alpha = 0.6) + 
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  labs(title = "Bayesian model representation", 
       subtitle = "Posterior and prior distributions",
       caption = "Data source: Obstetric Digital Health Intervention", 
       fill = "Distribution") +
  xlab("Estimate of intervention effect") + 
  ylab("Density") + 
  annotate("text", x = 50, y = 0.003, label = "\u03b8|X ~ X(-422.60, 199.75)") + 
  annotate("text", x = 450, y = 0.0005, label = "\u03b8 ~ X(-229.01, 245.81)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")) 