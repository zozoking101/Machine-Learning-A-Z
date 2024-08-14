# Data Processing

# Importing the dataset
dataset = read.csv('Data.csv')
# dataset = dataset[, 2:3]

# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)

# Split data set to Training and Test sets
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[, 2:3] = scale(training_set[,2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])
