## ---- split-data

# make this example reproducible
set.seed(5)

# use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), 
                 nrow(data_complete_cases), 
                 replace=TRUE, 
                 prob=c(0.8, 0.2))

train  <- data_complete_cases[sample, ]
test   <- data_complete_cases[!sample, ]

