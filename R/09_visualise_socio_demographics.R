# ---- demographics

# age
data_complete_cases_age_plot <- data_complete_cases

data_complete_cases_age_plot <- data_complete_cases_age_plot[data_complete_cases_age_plot$age_delivery > 18, ] 

data_complete_cases_age_plot <- data_complete_cases_age_plot[!is.na(data_complete_cases_age_plot$age_delivery), ]

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

socio_demo_age <- 
  ggplot(data_complete_cases_age_plot, aes(x = age_delivery)) +
  geom_histogram(bins = 30, fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Histogram of age distribution in sample",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Age") + 
  ylab("Frequency") + 
  scale_x_continuous(limits = c(18, 50), oob = scales::oob_keep) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/socio_demo_age.png", 
       plot = socio_demo_age)

# ---- health

# gestation-days

data_complete_cases_gest_plot <- data_complete_cases

data_complete_cases_gest_plot <- data_complete_cases_gest_plot[!is.na(data_complete_cases_gest_plot$gestation_days), ]

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

health_gestation_days <- 
  ggplot(data_complete_cases_gest_plot, aes(x = gestation_days)) +
  geom_histogram(bins = 20, fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Histogram of gestational age distribution",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Age") + 
  ylab("Frequency") + 
  scale_x_continuous(limits = c(175, 300), oob = scales::oob_keep) +
  scale_y_continuous(limits = c(0, 30)) +
  facet_wrap(~ group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic")
  )

ggsave("output/health_gestation_days.png", 
       plot = health_gestation_days)

# complications-maternal
data_complete_cases_mat_complications <- data_complete_cases

complications_mat_control <- 
  data_complete_cases_mat_complications[
    data_complete_cases_mat_complications$group_collapse == "control", ]

complications_mat_int <- 
  data_complete_cases_mat_complications[
    data_complete_cases_mat_complications$group_collapse == "intervention", ]

complications_mat_control_agg <- 
  as.data.frame(table(complications_mat_control$complications_maternal))

complications_mat_control_agg$percentage <- 
  (complications_mat_control_agg$Freq/nrow(complications_mat_control))*100

colnames(complications_mat_control_agg)[
  names(complications_mat_control_agg) == "Var1"] <- 
  "complications_maternal"

complications_mat_control_agg <- 
  complications_mat_control_agg[, c("complications_maternal", 
                                "percentage")]

complications_mat_control_agg$group_collapse <- "control"

complications_mat_int_agg <- 
  as.data.frame(table(complications_mat_int$complications_maternal))

complications_mat_int_agg$percentage <- 
  (complications_mat_int_agg$Freq/nrow(complications_mat_int))*100

colnames(complications_mat_int_agg)[
  names(complications_mat_int_agg) == "Var1"] <- 
  "complications_maternal"

complications_mat_int_agg <- 
  complications_mat_int_agg[, c("complications_maternal", 
                            "percentage")]

complications_mat_int_agg$group_collapse <- "intervention"

data_agg_mat_complications <- 
  rbind(complications_mat_control_agg, 
        complications_mat_int_agg)

data_agg_mat_complications$complications_maternal <- 
  ifelse(
    data_agg_mat_complications$complications_maternal == "no-complications", 
    "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

yes_no_order <- c('No', 'Yes')

complications_maternal_plot <- 
  ggplot(data_agg_mat_complications, 
         aes(x = factor(complications_maternal, level = yes_no_order), 
             y = percentage)) +
  geom_bar(stat = "identity", fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications attributed to mother",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications related to mother") + 
  ylab("Percentage \n (% of treatment group)") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/complications_maternal_plot.png", 
       plot = complications_maternal_plot)

# complications-newborn
data_complete_cases_ft_complications <- data_complete_cases

complications_ft_control <- 
  data_complete_cases_ft_complications[
    data_complete_cases_ft_complications$group_collapse == "control", ]

complications_ft_int <- 
  data_complete_cases_ft_complications[
    data_complete_cases_ft_complications$group_collapse == "intervention", ]

complications_ft_control_agg <- 
  as.data.frame(table(complications_ft_control$complications_fetal))

complications_ft_control_agg$percentage <- 
  (complications_ft_control_agg$Freq/nrow(complications_ft_control))*100

colnames(complications_ft_control_agg)[
  names(complications_ft_control_agg) == "Var1"] <- 
  "complications_fetal"

complications_ft_control_agg <- 
  complications_ft_control_agg[, c("complications_fetal", 
                                    "percentage")]

complications_ft_control_agg$group_collapse <- "control"

complications_ft_int_agg <- 
  as.data.frame(table(complications_ft_int$complications_fetal))

complications_ft_int_agg$percentage <- 
  (complications_ft_int_agg$Freq/nrow(complications_ft_int))*100

colnames(complications_ft_int_agg)[
  names(complications_ft_int_agg) == "Var1"] <- 
  "complications_fetal"

complications_ft_int_agg <- 
  complications_ft_int_agg[, c("complications_fetal", 
                                "percentage")]

complications_ft_int_agg$group_collapse <- "intervention"

data_agg_ft_complications <- 
  rbind(complications_ft_control_agg, 
        complications_ft_int_agg)

data_agg_ft_complications$complications_fetal <- 
  ifelse(
    data_agg_ft_complications$complications_fetal == "no-complications", 
    "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

yes_no_order <- c('No', 'Yes') 

complications_fetus_plot <- 
  ggplot(data_agg_ft_complications, 
         aes(x = factor(complications_fetal, level = yes_no_order), 
             y = percentage)) +
  geom_bar(stat = "identity", fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications attributed to fetus",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications related to fetus") + 
  ylab("Percentage \n (% of treatment group)") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/complications_fetus_plot.png", 
       plot = complications_fetus_plot)

# complications-delivery
data_complete_cases_complications <- data_complete_cases

complications_control <- 
  data_complete_cases_complications[
    data_complete_cases_complications$group_collapse == "control", ]

complications_int <- 
  data_complete_cases_complications[
    data_complete_cases_complications$group_collapse == "intervention", ]

complications_control_agg <- 
  as.data.frame(table(complications_control$complications_delivery))

complications_control_agg$percentage <- 
  (complications_control_agg$Freq/nrow(complications_control))*100

colnames(complications_control_agg)[
  names(complications_control_agg) == "Var1"] <- 
  "complications_delivery"

complications_control_agg <- 
  complications_control_agg[, c("complications_delivery", 
                                 "percentage")]

complications_control_agg$group_collapse <- "control"

complications_int_agg <- 
  as.data.frame(table(complications_int$complications_delivery))

complications_int_agg$percentage <- 
  (complications_int_agg$Freq/nrow(complications_int))*100

colnames(complications_int_agg)[
  names(complications_int_agg) == "Var1"] <- 
  "complications_delivery"

complications_int_agg <- 
  complications_int_agg[, c("complications_delivery", 
                                "percentage")]

complications_int_agg$group_collapse <- "intervention"

data_agg_complications <- 
  rbind(complications_control_agg, 
        complications_int_agg)

data_agg_complications$complications_delivery <- 
  ifelse(data_agg_complications$complications_delivery == 
           "no-complications", "No", "Yes")

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

yes_no_order <- c('No', 'Yes') 

complications_delivery_plot <- 
  ggplot(data_agg_complications, 
         aes(x = factor(complications_delivery, level = yes_no_order), 
             y = percentage)) +
  geom_bar(stat = "identity", fill = "#4739a2") +
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of complications at delivery",
       caption = "Data source: Obstetric Digital Health Intervention") +
  xlab("Complications at delivery") + 
  ylab("Percentage \n (% of treatment group)") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"), 
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt")) 

ggsave("output/complications_delivery_plot.png", 
       plot = complications_delivery_plot)

# risk-profile
data_complete_cases_risk <- data_complete_cases

data_complete_cases_risk_control <- 
  data_complete_cases_risk[
    data_complete_cases_risk$group_collapse == "control", ]

data_complete_cases_risk_int <- 
  data_complete_cases_risk[
    data_complete_cases_risk$group_collapse == "intervention", ]

control_risk <- as.data.frame(
  table(data_complete_cases_risk_control$pre_existing_risk_fetal, 
        data_complete_cases_risk_control$pre_existing_risk_maternal))

control_risk$percentage <- 
  (control_risk$Freq/nrow(data_complete_cases_risk_control))*100

colnames(control_risk)[names(control_risk) == "Var1"] <- 
  "pre_existing_risk_fetal"

colnames(control_risk)[names(control_risk) == "Var2"] <- 
  "pre_existing_risk_maternal"

control_risk <- control_risk[, c("pre_existing_risk_fetal", 
                 "pre_existing_risk_maternal", 
                 "percentage")]

control_risk$group_collapse <- "control"

int_risk <- as.data.frame(
  table(data_complete_cases_risk_int$pre_existing_risk_fetal, 
        data_complete_cases_risk_int$pre_existing_risk_maternal))

int_risk$percentage <- 
  (int_risk$Freq/nrow(data_complete_cases_risk_int))*100

colnames(int_risk)[names(int_risk) == "Var1"] <- 
  "pre_existing_risk_fetal"

colnames(int_risk)[names(int_risk) == "Var2"] <- 
  "pre_existing_risk_maternal"

int_risk <- int_risk[, 
                     c("pre_existing_risk_fetal", 
                       "pre_existing_risk_maternal", 
                       "percentage")]

int_risk$group_collapse <- "intervention"

data_risk_agg <- rbind(control_risk, int_risk)

data_risk_agg$pre_existing_risk_maternal <- 
  ifelse(data_risk_agg$pre_existing_risk_maternal == "low", 
         "Low", "High")

data_risk_agg$pre_existing_risk_fetal <- 
  ifelse(data_risk_agg$pre_existing_risk_fetal == "low", 
         "Low", "High")

risk_level_order <- c('Low', 'High') 

group_collapse.labs <- c("Control", "Intervention")
names(group_collapse.labs) <- c("control", "intervention")

risk_profile_plot <- 
  ggplot(data_risk_agg, 
         aes(x = factor(pre_existing_risk_maternal, level = risk_level_order), 
             fill = factor(pre_existing_risk_fetal, level = risk_level_order),  
             y = percentage)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#46e7fd", "#e18b22")) + 
  labs(title = paste0("Characteristics of sample"), 
       subtitle = "Bar chart of risk profile of pregnancy ",
       caption = "Data source: Obstetric Digital Health Intervention", 
       fill = "Risk profile of fetus") +
  xlab("Risk profile of mother") + 
  ylab("Percentage \n (% of treatment group)") + 
  facet_wrap(~group_collapse, 
             labeller = labeller(group_collapse = group_collapse.labs)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543"),
        plot.caption = element_text(color = "#454543", face = "italic"))

ggsave("output/risk_profile_plot.png", 
       plot = risk_profile_plot)
