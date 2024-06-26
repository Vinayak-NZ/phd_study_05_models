## ---- visualise-xgboost

# create predictor variables
Group.assignment <- rbinom(n = 10000, size = 1, prob = 0.5)
Risks.maternal <- rbinom(n = 10000, size = 1, prob = 0.7)
Time.of.birth <- rbinom(n = 10000, size = 1, prob = 0.58)

set.seed(555)
Gestation.days <- round(rnorm(10000, mean = 269, sd = 19.46))
Gestation.days <- pmax(180, Gestation.days)
Gestation.days <- pmin(294, Gestation.days)

set.seed(555)
Age.of.mother <- round(rnorm(10000, mean = 33.44, sd = 3.48))
Age.of.mother <- pmax(25, Age.of.mother)
Age.of.mother <- pmin(44, Age.of.mother)

# create datasets
sub_tree_01 <- data.frame(Group.assignment, Gestation.days, Risks.maternal)
sub_tree_02 <- data.frame(Group.assignment, Time.of.birth)
sub_tree_03 <- data.frame(Group.assignment, Age.of.mother)
sub_tree_04 <- data.frame(Group.assignment)

# create outcome variable as a function of predictor variables
sub_tree_01$Cost.of.care.p1 <- ifelse(
  sub_tree_01$Group.assignment == 1, 
  round(pmax(600, rnorm(nrow(sub_tree_01), mean = 628, sd = 500))), 
  round(pmax(800, rnorm(nrow(sub_tree_01), mean = 1000, sd = 150))))

sub_tree_01$Cost.of.care.p2 <- ifelse(
  sub_tree_01$Gestation.days >= 259, 
  round(pmax(300, rnorm(nrow(sub_tree_01), mean = 500, sd = 50))), 
  round(pmax(600, rnorm(nrow(sub_tree_01), mean = 800, sd = 45))))

sub_tree_01$Cost.of.care.p3 <- ifelse(
  sub_tree_01$Risks.maternal == 0, 
  round(pmax(300, rnorm(nrow(sub_tree_01), mean = 600, sd = 100))), 
  round(pmax(600, rnorm(nrow(sub_tree_01), mean = 900, sd = 50))))

sub_tree_02$Cost.of.care.p1 <- 
  ifelse(sub_tree_02$Group.assignment == 1, 
         round(pmax(600, rnorm(nrow(sub_tree_02), mean = 628, sd = 250))), 
         round(pmax(800, rnorm(nrow(sub_tree_02), mean = 1000, sd = 75))))

sub_tree_02$Cost.of.care.p2 <- ifelse(
  sub_tree_02$Time.of.birth == 0, 
  round(pmax(300, rnorm(nrow(sub_tree_02), mean = 500, sd = 25))), 
  round(pmax(600, rnorm(nrow(sub_tree_02), mean = 800, sd = 22))))

sub_tree_03$Cost.of.care.p1 <- ifelse(
  sub_tree_03$Group.assignment == 1, 
  round(pmax(600, rnorm(nrow(sub_tree_03), mean = 628, sd = 125))), 
  round(pmax(800, rnorm(nrow(sub_tree_03), mean = 1000, sd = 35))))

sub_tree_03$Cost.of.care.p2 <- ifelse(
  sub_tree_03$Age.of.mother < 35, 
  round(pmax(300, rnorm(nrow(sub_tree_03), mean = 500, sd = 12))), 
  round(pmax(600, rnorm(nrow(sub_tree_03), mean = 800, sd = 12))))

sub_tree_04$Cost.of.care.p1 <- ifelse(
  sub_tree_04$Group.assignment == 1, 
  round(pmax(600, rnorm(nrow(sub_tree_04), mean = 628, sd = 50))), 
  round(pmax(800, rnorm(nrow(sub_tree_04), mean = 1000, sd = 10))))

sub_tree_01$Cost.of.care <- 
  sub_tree_01$Cost.of.care.p1 + 
  sub_tree_01$Cost.of.care.p2 + 
  sub_tree_01$Cost.of.care.p3

sub_tree_02$Cost.of.care <- 
  2*sub_tree_02$Cost.of.care.p1 + 
  sub_tree_02$Cost.of.care.p2

sub_tree_03$Cost.of.care <- 
  3*sub_tree_03$Cost.of.care.p1 + 
  sub_tree_03$Cost.of.care.p2

sub_tree_04$Cost.of.care <- 
  4*sub_tree_04$Cost.of.care.p1

# format-variables-datasets
sub_tree_01$Group.assignment <- 
  as.factor(ifelse(sub_tree_01$Group.assignment == 0, 
                   "Control", 
                   "Intervention"))

sub_tree_01$Risks.maternal <- 
  as.factor(ifelse(sub_tree_01$Risks.maternal == 0, 
                   "Low", 
                   "High"))

sub_tree_02$Group.assignment <- 
  as.factor(ifelse(sub_tree_02$Group.assignment == 0, 
                   "Control", 
                   "Intervention"))

sub_tree_02$Time.of.birth <- 
  as.factor(ifelse(sub_tree_02$Time.of.birth == 0, 
                   "Core hours", 
                   "After hours"))

sub_tree_03$Group.assignment <- 
  as.factor(ifelse(sub_tree_03$Group.assignment == 0, 
                   "Control", 
                   "Intervention"))

sub_tree_04$Group.assignment <- 
  as.factor(ifelse(sub_tree_04$Group.assignment == 0, 
                   "Control", 
                   "Intervention"))

sub_tree_01 <- sub_tree_01[, c("Group.assignment", 
                               "Gestation.days", 
                               "Risks.maternal", 
                               "Cost.of.care")]

sub_tree_02 <- sub_tree_02[, c("Group.assignment", 
                               "Time.of.birth", 
                               "Cost.of.care")]

sub_tree_03 <- sub_tree_03[, c("Group.assignment", 
                               "Age.of.mother", 
                               "Cost.of.care")]

sub_tree_04 <- sub_tree_04[, c("Group.assignment", 
                               "Cost.of.care")]

# create-decision-trees
sub_tree_fit_01 <- rpart(Cost.of.care ~ 
                           Group.assignment + 
                           Gestation.days + 
                           Risks.maternal, 
                         data = sub_tree_01)

sub_tree_fit_02 <- rpart(Cost.of.care ~ 
                           Group.assignment + 
                           Time.of.birth, 
                         data = sub_tree_02)

sub_tree_fit_03 <- rpart(Cost.of.care ~ 
                           Group.assignment + 
                           Age.of.mother, 
                         data = sub_tree_03)

sub_tree_fit_04 <- rpart(Cost.of.care ~ 
                           Group.assignment, 
                         data = sub_tree_04)

# reformat-decision-tree-objects
sub_tree_party_01 <- as.party(sub_tree_fit_01)

sub_tree_party_02 <- as.party(sub_tree_fit_02)

sub_tree_party_03 <- as.party(sub_tree_fit_03)

sub_tree_party_04 <- as.party(sub_tree_fit_04)

# output-trees
plot_tree_01 <- 
  ggparty(sub_tree_party_01) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = 
                   list(geom_histogram(aes(x = !!sub_tree_party_01$terms[[2]])), 
                        scale_fill_manual("#46e7fd"), 
                        xlab("\u20AC"), 
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"), 
                              axis.title.y=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank(), 
                              axis.ticks.x=element_blank())))

plot_tree_02 <- 
ggparty(sub_tree_party_02) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = 
                   list(geom_histogram(aes(x = !!sub_tree_party_02$terms[[2]])), 
                        xlab("\u20AC"), 
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"), 
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.ticks.x = element_blank())))

plot_tree_03 <- 
ggparty(sub_tree_party_03) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = 
                   list(geom_histogram(aes(x = !!sub_tree_party_03$terms[[2]])), 
                        xlab("\u20AC"), 
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"), 
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.ticks.x = element_blank())))

plot_tree_04 <- 
ggparty(sub_tree_party_04) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = 
                   list(geom_histogram(aes(x = !!sub_tree_party_04$terms[[2]])), 
                        xlab("\u20AC"), 
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"), 
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.ticks.x = element_blank())))

ggsave("output/plot_xgboost_demo_tree_01.png", 
       plot = plot_tree_01)

ggsave("output/plot_xgboost_demo_tree_02.png", 
       plot = plot_tree_02)

ggsave("output/plot_xgboost_demo_tree_03.png", 
       plot = plot_tree_03)

ggsave("output/plot_xgboost_demo_tree_04.png", 
       plot = plot_tree_04)