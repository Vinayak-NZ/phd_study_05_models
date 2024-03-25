raw_data_one_group_drg <- merge(raw_data_phase_one, drg_classification, by = "drg")

data_2018 <- raw_data_one_group_drg[raw_data_one_group_drg$year == 2018, ]
data_2020 <- raw_data_one_group_drg[raw_data_one_group_drg$year == 2020, ]

t.test(data_2018$drg_yield, data_2020$drg_yield, alternative = "two.sided", var.equal = FALSE)
t.test(data_2018$internal_cost, data_2020$internal_cost, alternative = "two.sided", var.equal = FALSE)

hist(data_2018$drg_yield)
hist(data_2020$drg_yield)

# statistical test
wilcox.test(data_2020$drg_yield,
            data_2018$drg_yield,
            conf.int = TRUE,
            conf.level = 0.95,
            exact = FALSE)

priors <- list(muM = 3155, muSD = 2321)

BESTout <- BESTmcmc(data_2020$drg_yield, 
                    data_2018$drg_yield, 
                    priors=priors, 
                    parallel=FALSE)

plot(BESTout)

indBayesWilcox <- bayes.wilcox.test(data_2018$drg_yield, 
                                    data_2020$drg_yield)

summary(indBayesWilcox)
plot(indBayesWilcox)
diagnostics.bayes_two_sample_wilcox_test(indBayesWilcox)
model.code.bayes_two_sample_wilcox_test(indBayesWilcox)

# plot distributions
res <- raw_data_one_group_drg %>%
  ggplot( aes(x=drg_yield, fill=year)) +
  geom_histogram( color="#EAF3F9", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#A4BFD2", "#E13530")) +
  labs(fill="")

  theme(text = element_text(family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#205B87"),
        plot.caption = element_text(color = "#5B8AAA", face = "italic")) 


