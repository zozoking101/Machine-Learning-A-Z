# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Split data set to Training and Test sets
# library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[, 2:3] = scale(training_set[,2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])

# Linear Regression
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)

# Predicting Test set results
y_pred_test = predict(regressor, newdata = test_set)

# Predicting the Training set results
y_pred_train = predict(regressor, newdata = training_set)

# Visualizing the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary, color = 'Datapoint'), size = 2) +
  geom_line(aes(x = training_set$YearsExperience, y = y_pred_train, color = 'Line of best fit'), size = 1) +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of Experience') +
  ylab('Salary') +
  scale_color_manual(name = 'Legend', values = c('Datapoint' = 'red', 'Line of best fit' = 'blue')) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary, color = 'Datapoint'), size = 2) +
  geom_line(aes(x = training_set$YearsExperience, y = y_pred_train, color = 'Line of best fit'), size = 1) +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of Experience') +
  ylab('Salary') +
  scale_color_manual(name = 'Legend', values = c('Datapoint' = 'red', 'Line of best fit' = 'blue')) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# Calculating accuracy metrics

# Mean Squared Error (MSE) for Test set
mse_test <- mean((y_pred_test - test_set$Salary)^2)
cat("Mean Squared Error (Test set):", mse_test, "\n")

# Mean Squared Error (MSE) for Training set
mse_train <- mean((y_pred_train - training_set$Salary)^2)
cat("Mean Squared Error (Training set):", mse_train, "\n")

# R-squared value for both sets
r_squared_test <- summary(regressor)$r.squared
cat("R-squared (Test set):", r_squared_test, "\n")

r_squared_train <- summary(lm(Salary ~ YearsExperience, data = training_set))$r.squared
cat("R-squared (Training set):", r_squared_train, "\n")
