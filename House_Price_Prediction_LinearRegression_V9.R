#Ridge Lasso Regression and Linear Regression

install.packages("readxl")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("psych")
install.packages("lattice")
install.packages("caret")
install.packages("readxl")
install.packages("Metrics")
install.packages("dplyr")
install.packages("glmnet")
library(glmnet)
library("dplyr")
library("Metrics")
library("readxl")
library("ggplot2")
library("ggcorrplot")
library("psych")
library("lattice")
library("caret")


# *******************************************
# Data collection
#Read the data
setwd("C:/Users/SAYAN/OneDrive/Desktop/Sem 2/ML/House price Prediction")
Data <- read.csv("data.csv")
D <- Data 

head(Data)

#Summary of the data
summary(Data)

# Checking the missing value
missing_value <- is.na(Data)
nawhere <- which(is.na(Data))
colSums(is.na(Data))

#checking the distribution of the target variable

boxplot(Data$price)
hist(Data$price, breaks = 1000000)

ggplot(Data, aes(x=price))+geom_histogram(bins = 1000)+
  theme_minimal() +
  xlab("Price") + 
  ylab("Frequency")


#Finding the diff attribute
sapply(Data,class)

#Find the correlation between the value
Data <- as.data.frame(sapply(Data[1:nrow(Data),1:ncol(Data)], as.numeric))
correlation_matrix <- cor(Data[,2:13])

# Visualize the correlation matrix
# install.packages("ggcorrplot")
# library(ggcorrplot)
View(correlation_matrix)
ggcorrplot(correlation_matrix)

#Partition of the data
#*************Training Data - 70%***************

set.seed(123)
#Df <- subset.data.frame(Data,2:13)
training_index <- createDataPartition(Data$price, p= 0.7,list = FALSE)

training_data <- Data[training_index, ]

testing_data <- Data[-training_index, ]

# Build the linear regression model

model <- lm(price~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view,data = training_data)

summary(model)


# Make predictions on the testing data
predictions <- predict(model, newdata = testing_data)


rmse <- rmse(testing_data$price, predictions)
r_squared <- R2(testing_data$price, predictions)

# Print the performance metrics

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

adj_rsq <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq)

## Perform Lasso regression

model1 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

# Select the optimal lambda value using cross-validation
cvmodel <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

#####plot(cvmodel)
bestlam <- cvmodel$lambda.min

# Fit the model with the selected lambda value
lassomodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1, lambda = bestlam)

# Predict on test data
pred <- predict(lassomodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
lrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",lrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])

adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)


##Perform Ridge Regression
model2 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

# Select the optimal lambda value using cross-validation
cvmodel1 <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

#plot(cvmodel1)
bestlam <- cvmodel1$lambda.min

#Fit the 
ridgemodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0, lambda = bestlam)

# Predict on test data
pred <- predict(ridgemodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
rrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",rrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])
adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)


#*************Train data - 80%**************
set.seed(123)
#Df <- subset.data.frame(Data,2:13)
training_index <- createDataPartition(Data$price, p= 0.8,list = FALSE)

training_data <- Data[training_index, ]

testing_data <- Data[-training_index, ]

# Build the linear regression model

model <- lm(price~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view,data = training_data)

summary(model)


# Make predictions on the testing data
predictions <- predict(model, newdata = testing_data)


# install.packages("Metrics")
# library(Metrics)
rmse <- rmse(testing_data$price, predictions)
r_squared <- R2(testing_data$price, predictions)

# Print the performance metrics

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

adj_rsq <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq)

## Perform Lasso regression
# install.packages("glmnet")
# library(glmnet)
model1 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

# Select the optimal lambda value using cross-validation
cvmodel <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

###plot(cvmodel)
bestlam <- cvmodel$lambda.min

# Fit the model with the selected lambda value
lassomodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1, lambda = bestlam)

# Predict on test data
pred <- predict(lassomodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
lrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",lrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])

adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)


##Perform Ridge Regression
model2 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

# Select the optimal lambda value using cross-validation
cvmodel1 <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

#plot(cvmodel1)
bestlam <- cvmodel1$lambda.min

#Fit the 
ridgemodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0, lambda = bestlam)

# Predict on test data
pred <- predict(ridgemodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
rrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",rrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])
adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)

#*********Tarining data - 90%*********


set.seed(123)
#Df <- subset.data.frame(Data,2:13)
training_index <- createDataPartition(Data$price, p= 0.8,list = FALSE)

training_data <- Data[training_index, ]

testing_data <- Data[-training_index, ]

# Build the linear regression model

model <- lm(price~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view,data = training_data)

summary(model)


# Make predictions on the testing data
predictions <- predict(model, newdata = testing_data)


# install.packages("Metrics")
# library(Metrics)
rmse <- rmse(testing_data$price, predictions)
r_squared <- R2(testing_data$price, predictions)

# Print the performance metrics

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

adj_rsq <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq)

## Perform Lasso regression
# install.packages("glmnet")
# library(glmnet)
model1 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

# Select the optimal lambda value using cross-validation
cvmodel <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1)

###plot(cvmodel)
bestlam <- cvmodel$lambda.min

# Fit the model with the selected lambda value
lassomodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 1, lambda = bestlam)

# Predict on test data
pred <- predict(lassomodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
lrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",lrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])

adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)


##Perform Ridge Regression
model2 <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

# Select the optimal lambda value using cross-validation
cvmodel1 <- cv.glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0)

##plot(cvmodel1)
bestlam <- cvmodel1$lambda.min

#Fit the 
ridgemodel <- glmnet(x = as.matrix(training_data[, 3:14]), y = training_data$price, alpha = 0, lambda = bestlam)

# Predict on test data
pred <- predict(ridgemodel, newx = as.matrix(testing_data[, 3:14]))

# Calculate R-squared value
rsq <- cor(testing_data$price, pred)^2

# Print R-squared value
cat("R-squared value:", round(rsq, 3))

#Calculating RMS Value
rrms <- rmse(testing_data$price, pred)
#Printing the RMSE Value
cat("RMSE :",rrms)

#calcuating the Adjusted R-square
y_true <- testing_data$price
n <- length(y_true)
p <- ncol(testing_data[, 3:14])
adj_R2 <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
cat("Ajusted R^2 : ",adj_R2)


