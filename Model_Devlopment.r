# Name- Pruthvi Samir Patel, Utkarsh Kothari, Jatin Satija
# Class - ALY6015 Intermediate Analytics CRN - 21626

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

#---- Loading Packages ----
library(pacman)
p_load(tidyverse)
library(ggplot2)
library(BSDA)
library(gridExtra)
library(janitor)
library(corrplot)
library(ggcorrplot)
library(car)
library(MASS)
library(leaps)
library(glmnet)
library(caret)
library(pROC)
library(glmnet)
#-------------------------

#---- Loading Data ----
data <- read.csv("Cardiovascular_Diseases_Risk.csv")
names(data)

str(data)
summary(data)
#-----------------------

#----- Data Cleaning --------
# Checking NAs
checking_Nas <-  colSums(is.na(data))
checking_Nas

#No NA values found in dataset

number_of_rows <- nrow(data)
number_of_rows

number_of_columns <- ncol(data)
number_of_columns

#----------- Handling Outliers ---------------

numeric_fields <- data[, sapply(data, is.numeric)]
names(numeric_fields)
fields_with_outliers <- c("BMI", "Alcohol_Consumption", "Fruit_Consumption", "Green_Vegetables_Consumption")

for (field_name in fields_with_outliers) {
  
  Q1 <- quantile(data[, field_name], 0.25)
  Q3 <- quantile(data[, field_name], 0.75)
  
  IQR <- Q3 - Q1
  
  lower_bound_value <- Q1 - 0.5 * IQR
  upper_bound_value <- Q3 + 0.5 * IQR
  
  presence_of_outliers <- data[, field_name] < lower_bound_value | data[, field_name] > upper_bound_value
  
  data <- data[!presence_of_outliers, ]
}

#-----------------------------------------------

#---- Q.1 ---- 
#Question 1: Can we build a predictive model using logistic regression to assess the risk of cardiovascular diseases based on various health indicators and lifestyle factors? Specifically, we want to understand the 
#influence of exercise, cancer history, mental health (depression), diabetes, arthritis, and other factors on the likelihood of developing heart disease.

data_yes <- subset(data, Heart_Disease == "Yes")
data_no <- subset(data, Heart_Disease == "No")

# Sample 10,000 rows from each subset
data_yes_sampled <- data_yes[sample(nrow(data_yes), 10000), ]
data_no_sampled <- data_no[sample(nrow(data_no), 10000), ]

# Combine the sampled subsets
balanced_data <- rbind(data_yes_sampled, data_no_sampled)

# Shuffle the rows to randomize the order
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

data <- balanced_data

#Converting categorical variables to factors

data$Heart_Disease <- as.factor(data$Heart_Disease)
data$Smoking_History <- as.factor(data$Smoking_History)
data$Diabetes <- as.factor(data$Diabetes)
data$Arthritis <- as.factor(data$Arthritis)
data$Skin_Cancer <- as.factor(data$Skin_Cancer)
data$Other_Cancer <- as.factor(data$Other_Cancer)
data$Exercise <- as.factor(data$Exercise)
data$Age_Category <- as.factor(data$Age_Category)
data$Checkup <- as.factor(data$Checkup)

# Handling binary Fields.
data$Exercise_conv <- ifelse(data$Exercise == "Yes", 1, 0)
data$Smoking_History_conv <- ifelse(data$Smoking_History == "Yes", 1, 0)
data$Arthritis_conv <- ifelse(data$Arthritis == "Yes", 1, 0)
data$Skin_Cancer_conv <- ifelse(data$Skin_Cancer == "Yes", 1, 0)
data$Other_Cancer_conv <- ifelse(data$Other_Cancer == "Yes", 1, 0)


# One-hot encoding for Diabetes field (as it have 4 categories)
unique(data$Diabetes)
data <- cbind(data, model.matrix(~ Diabetes - 1, data = data))

#Changing the column names

colnames(data)[colnames(data) == "DiabetesNo"] <- "No_Diabetes"
colnames(data)[colnames(data) == "DiabetesYes"] <- "Yes_Diabetes"
colnames(data)[colnames(data) == "DiabetesNo, pre-diabetes or borderline diabetes"] <- "Pre_Diabetes"
colnames(data)[colnames(data) == "DiabetesYes, but female told only during pregnancy"] <- "Pregnancy_Diabetes"

## One-hot encoding for General Health field (as it have 5 categories)
data <- cbind(data, model.matrix(~ General_Health - 1, data = data))

colnames(data)[colnames(data) == "General_HealthVery Good"] <- "General_HealthVery_Good"

#One-hot encoding for Age Group 
data <- cbind(data, model.matrix(~ Age_Category - 1, data = data))


#One-hot encoding for Checkup
data <- cbind(data, model.matrix(~ Checkup - 1, data = data))


colnames(data)[colnames(data) == "Checkup5 or more years ago"] <- "More than 5 years"
colnames(data)[colnames(data) == "CheckupNever"] <- "Never"
colnames(data)[colnames(data) == "CheckupWithin the past 2 years"] <- "Past 2 years"
colnames(data)[colnames(data) == "CheckupWithin the past 5 years"] <- "PAst 5 years"
colnames(data)[colnames(data) == "CheckupWithin the past year"] <- "Past 1 year"

#Train and Test data
set.seed(123)
trainIndex <- createDataPartition(data$Heart_Disease, p = 0.7, list = FALSE, times = 1)
trainIndex

train <- data[trainIndex,]
train

test <- data[-trainIndex,]
test

train_data <- subset(train, select = -c(Smoking_History, Diabetes, Arthritis, Skin_Cancer, Other_Cancer, Exercise,
                                        Age_Category, General_Health, Sex, Depression, Checkup))

test_data <- subset(test, select = -c(Smoking_History, Diabetes, Arthritis, Skin_Cancer, Other_Cancer, Exercise,
                                      Age_Category, General_Health, Sex, Depression, Checkup))

#Creating a Logistic regression model
#Response varibale: Heart_Disease
#Predictors(categorical variables): 
#Model


logistic_model_heart <- glm(Heart_Disease ~ ., data = train_data, family = binomial)
summary(logistic_model_heart)

coef(logistic_model_heart)
exp(coef(logistic_model_heart))

#Confusion matrix for train data
train_predictions <- predict(logistic_model_heart, train_data, type = "response")
train_predictions

train_predictions_binary <- as.factor(ifelse(train_predictions > 0.5, "Yes", "No"))
train_predictions_binary

head(train_predictions_binary)

confusionMatrix(train_predictions_binary, train_data$Heart_Disease, positive = "Yes")

#Confusion matrix for test data
test_predictions <- predict(logistic_model_heart, test_data, type = "response")
test_predictions

test_predictions_binary <- as.factor(ifelse(test_predictions > 0.5, "Yes", "No"))
test_predictions_binary

head(test_predictions_binary)

confusionMatrix(test_predictions_binary, test_data$Heart_Disease, positive = "Yes")

#ROC
ROC1 <- roc(test_data$Heart_Disease, test_predictions)
ROC1

plot(ROC1, col = "black", ylab = "Sensitivity - TP rate", xlab = "Specificity - FP rate", main = "ROC Curve")

#AUC
auc <- auc(ROC1)
auc

#Model 2 (stepwise feature selection approach)
step_wise_selection <- step(logistic_model_heart, direction = "both", trace=TRUE)
selected_features_stepwise <- names(coef(step_wise_selection))

cat("Best Features based on stepwise selection:", selected_features_stepwise, "\n")

logistic_model_refined <- glm(Heart_Disease ~ Height_.cm. + Weight_.kg. + BMI + Alcohol_Consumption + Fruit_Consumption + 
                                Smoking_History_conv + Arthritis_conv + Skin_Cancer_conv + Other_Cancer_conv + 
                                Pre_Diabetes + Yes_Diabetes + General_HealthExcellent + General_HealthFair + 
                                General_HealthGood + General_HealthPoor + `Age_Category18-24` + `Age_Category25-29` + 
                                `Age_Category30-34` + `Age_Category35-39` + `Age_Category40-44` + `Age_Category45-49` + 
                                `Age_Category50-54` + `Age_Category55-59` + `Age_Category60-64` + `Age_Category65-69` + 
                                `Age_Category70-74` + `Age_Category75-79` + `More than 5 years` + `Past 2 years` + `PAst 5 years`, data = train_data, family = binomial)

summary(logistic_model_refined)



#Confusion matrix for train data
train_predictions <- predict(logistic_model_refined, train_data, type = "response")
train_predictions

train_predictions_binary <- as.factor(ifelse(train_predictions > 0.5, "Yes", "No"))
train_predictions_binary

head(train_predictions_binary)

confusionMatrix(train_predictions_binary, train_data$Heart_Disease, positive = "Yes")

#Confusion matrix for test data
test_predictions <- predict(logistic_model_refined, test_data, type = "response")
test_predictions

test_predictions_binary <- as.factor(ifelse(test_predictions > 0.5, "Yes", "No"))
test_predictions_binary

head(test_predictions_binary)

confusionMatrix(test_predictions_binary, test_data$Heart_Disease, positive = "Yes")

#ROC
ROC1 <- roc(test_data$Heart_Disease, test_predictions)
ROC1

plot(ROC1, col = "black", ylab = "Sensitivity - TP rate", xlab = "Specificity - FP rate", main = "ROC Curve")

#AUC
auc <- auc(ROC1)
auc

#Ridge regularization
train_x <- model.matrix(Heart_Disease ~., train_data)[,-1]
train_x

test_x <- model.matrix(Heart_Disease~., test_data)[,-1]
test_x

train_y <- train_data$Heart_Disease
train_y

test_y <- test_data$Heart_Disease
test_y

train_y <- as.numeric(train_y) - 1 
test_y <- as.numeric(test_y) - 1


set.seed(123)
cv.ridge <- cv.glmnet(train_x, train_y, alpha = 0, nfolds = 10)
plot(cv.ridge)

log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

ridge.model.min <- glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.min)
coef(ridge.model.min)

ridge.model.1se <- glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.1se)
coef(ridge.model.1se)

ridge.preds.train <- predict(ridge.model.1se, newx = train_x)
ridge.train.rmse <- rmse(train_y, ridge.preds.train)
ridge.train.rmse

ridge.preds.test <- predict(ridge.model.1se, newx = test_x)
ridge.test.rmse <- rmse(test_y, ridge.preds.test)
ridge.test.rmse

#LASSO regularization

set.seed(256)
cv.lasso <- cv.glmnet(train_x, train_y, nfolds = 10)
plot(cv.lasso)

log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

lasso.model.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)
coef(lasso.model.min)

lasso.model.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)
coef(lasso.model.1se)

lasso.preds.train <- predict(lasso.model.1se, newx = train_x)
lasso.train.rmse <- rmse(train_y, lasso.preds.train)
lasso.train.rmse

lasso.preds.test <- predict(lasso.model.1se, newx = test_x)
lasso.test.rmse <- rmse(test_y, lasso.preds.test)
lasso.test.rmse

#Confusion matrix for train data
train_predictions <- predict(lasso.model.1se, train_x, type = "response")
train_predictions

train_predictions_binary <- as.factor(ifelse(train_predictions > 0.5, "Yes", "No"))
train_predictions_binary

head(train_predictions_binary)


confusionMatrix(train_predictions_binary, train_data$Heart_Disease, positive = "Yes")

#Confusion matrix for test data
test_predictions <- predict(lasso.model.1se, test_x, type = "response")
test_predictions

test_predictions_binary <- as.factor(ifelse(test_predictions > 0.5, "Yes", "No"))
test_predictions_binary

head(test_predictions_binary)

confusionMatrix(test_predictions_binary, test_data$Heart_Disease, positive = "Yes")

#ROC
ROC1 <- roc(test_data$Heart_Disease, test_predictions)
ROC1

plot(ROC1, col = "black", ylab = "Sensitivity - TP rate", xlab = "Specificity - FP rate", main = "ROC Curve")

#AUC
auc <- auc(ROC1)
auc

#---- Q.2 ---- 
#Question 2: How do lifestyle decisions, including exercise habits, General health and eating habits, influence Body Mass Index (BMI) and, in turn, impact cardiovascular health?
  
# Handling binary Fields.(Exercise, Smoking_History)
data$Exercise_conv <- ifelse(data$Exercise == "Yes", 1, 0)
data$Smoking_History_conv <- ifelse(data$Smoking_History == "Yes", 1, 0)
data$Diabetes_conv <- ifelse(data$Diabetes == "Yes",1,0)

# One-hot encoding for general health field (as it have 5 categories)
data <- cbind(data, model.matrix(~ General_Health - 1, data = data))

# Fixing Col name
colnames(data)[colnames(data) == "General_HealthVery Good"] <- "General_HealthVery_Good"


# Model Development 
firstModel <- lm(BMI ~ Fruit_Consumption + Green_Vegetables_Consumption+ Alcohol_Consumption +FriedPotato_Consumption+
                   Exercise_conv + Smoking_History_conv + 
                   General_HealthExcellent + General_HealthFair + 
                   General_HealthGood + General_HealthPoor + 
                   General_HealthVery_Good, data = data)

summary(firstModel)

firstModel_extended <- lm(BMI ~ Fruit_Consumption + Green_Vegetables_Consumption+Height_.cm. +Weight_.kg.+Diabetes_conv+ Alcohol_Consumption +FriedPotato_Consumption+
                            Exercise_conv + Smoking_History_conv + 
                            General_HealthExcellent + General_HealthFair + 
                            General_HealthGood + General_HealthPoor + 
                            General_HealthVery_Good, data = data)

summary(firstModel_extended)


# Model Plots
plot(firstModel_extended)

# Multicollinearity test 
vif(firstModel)

#---------- Checking for Model Outliers ----------  
outlierTest(model = firstModel_extended)

# Insights from Hat Plot
hat_plot <- function(model){
  p <- length(coefficients(model))
  n <- length(fitted(model))
  
  plot(hatvalues(model), main ="Index Plot of Hat Values")
  abline(h= c(2,3)*p/n, col="red",lty=2)
  identify(1:n, hatvalues(model), names(hatvalues(model)))
}

hat_plot(firstModel)

# Handling Outliers in model
fix_outliers <- function(model, dataset, threshold = 4 / (nrow(dataset) - length(model$coefficients) - 2)) {
  cooks_distance <- cooks.distance(model)
  outliers <- cooks_distance > threshold
  
  if (any(outliers)) {
    new_dataset <- dataset[!outliers, ]
    cat("Outliers fixed by removing them.\n")
    return(new_dataset)
  } else {
    cat("No outliers found.\n")
    return(dataset)
  }
}

outlier_free_dataset <- fix_outliers(firstModel_extended, data)


secondModel <-lm(BMI ~ Fruit_Consumption + Green_Vegetables_Consumption+ Height_.cm. + Weight_.kg.+ Diabetes_conv + Alcohol_Consumption +FriedPotato_Consumption+
                   Exercise_conv + Smoking_History_conv + 
                   General_HealthExcellent + General_HealthFair + 
                   General_HealthGood + General_HealthPoor +General_HealthVery_Good
                 , data = outlier_free_dataset)
summary(secondModel)

# Using Transformation 
outlier_free_dataset$BMI_sqrt <- sqrt(outlier_free_dataset$BMI)
ThirdModel <-lm(BMI_sqrt ~ Fruit_Consumption + Green_Vegetables_Consumption + Alcohol_Consumption + FriedPotato_Consumption+Height_.cm. + Weight_.kg.+ Diabetes_conv +
                  Exercise_conv + Smoking_History_conv + 
                  General_HealthExcellent + General_HealthFair + 
                  General_HealthGood + General_HealthPoor + 
                  General_HealthVery_Good, data = outlier_free_dataset)
summary(ThirdModel)

# Using Feature Selection 
# Both stepwise and backward  approach

step_wise_selection <- step(ThirdModel, direction = "both", trace=TRUE)
selected_features_stepwise <- names(coef(step_wise_selection))

cat("Best Features based on stepwise selection:", selected_features_stepwise, "\n")

FourthModel <-lm(BMI_sqrt ~ Green_Vegetables_Consumption + Alcohol_Consumption + 
                   FriedPotato_Consumption + Height_.cm. + Weight_.kg. + Diabetes_conv + Exercise_conv 
                 + Smoking_History_conv + General_HealthExcellent + General_HealthFair + General_HealthPoor, data = outlier_free_dataset)
summary(FourthModel)

best_features <- c("Green_Vegetables_Consumption", "Alcohol_Consumption", "FriedPotato_Consumption",
                   "Height_.cm.", "Weight_.kg.", "Diabetes_conv", "Exercise_conv",
                   "Smoking_History_conv", "General_HealthExcellent", "General_HealthFair",
                   "General_HealthPoor")

X <- outlier_free_dataset[, best_features]
y <- outlier_free_dataset$BMI_sqrt

X <- as.matrix(X)
X

cv_for_lasso <- cv.glmnet(X, y, alpha = 1)  # Set alpha = 1 for Lasso
cv_for_ridge <- cv.glmnet(X, y, alpha = 0)  # Set alpha = 0 for Ridge


opt_lambda_for_lasso <- cv_for_lasso$lambda.min
opt_lambda_for_ridge <- cv_for_ridge$lambda.min

fifth_model_lasso <- glmnet(X, y, alpha = 1, lambda = opt_lambda_for_lasso)
coef(fifth_model_lasso)

sixth_model_ridge <- glmnet(X, y, alpha = 0, lambda = opt_lambda_for_ridge)
coef(sixth_model_ridge)

plot(ThirdModel)
vif(FourthModel)

AIC(firstModel)
AIC(firstModel_extended)
AIC(secondModel)
AIC(ThirdModel)
AIC(FourthModel)


# Model Accuracy Test 
values_prediction <- predict(FourthModel, newdata = outlier_free_dataset)
actual_values <- outlier_free_dataset$BMI_sqrt


predicted_and_actual_values_table <- data.frame(Actual = actual_values, Predicted = values_prediction)


# Mean Absolute Error (MAE)
mae <- mean(abs(predicted_and_actual_values_table$Actual - predicted_and_actual_values_table$Predicted))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_and_actual_values_table$Actual - predicted_and_actual_values_table$Predicted)^2))

#  R-squared
rsquared <- cor(predicted_and_actual_values_table$Actual, predicted_and_actual_values_table$Predicted)^2

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsquared, "\n")

#---- For LASSO and Ridge Model ----
preds_by_lasso_model <- predict(fifth_model_lasso, newx = X)
preds_by_ridge_model <- predict(sixth_model_ridge, newx = X)

# Calculate MAE,RMSE and R-squared  for Lasso and Ridge models
lasso_model_mae <- mean(abs(y - preds_by_lasso_model))
ridge_model_mae <- mean(abs(y - preds_by_ridge_model))


lasso_model_rmse <- sqrt(mean((y - preds_by_lasso_model)^2))
ridge_model_rmse <- sqrt(mean((y - preds_by_ridge_model)^2))


lasso_model_r_squared <- 1 - sum((y - preds_by_lasso_model)^2) / sum((y - mean(y))^2)
ridge_model_r_squared <- 1 - sum((y - preds_by_ridge_model)^2) / sum((y - mean(y))^2)

# Lasso And Ridge Model results
print("Lasso Model:")
print(paste("  MAE:", lasso_model_mae))
print(paste("  RMSE:", lasso_model_rmse))
print(paste("  R-squared:", lasso_model_r_squared))

print("Ridge Model:")
print(paste("  MAE:", ridge_model_mae))
print(paste("  RMSE:", ridge_model_rmse))
print(paste("  R-squared:", ridge_model_r_squared))

ggplot(outlier_free_dataset, aes(x = Heart_Disease, y = BMI, fill = Heart_Disease)) +
  geom_boxplot() +
  labs(title = "BMI Distribution Among People with and Without Heart Disease",
       x = "Heart Disease",
       y = "BMI") +
  scale_fill_manual(values = c("darkseagreen1","aquamarine"))+
  theme_minimal()


#---- Q.3 ---- 
#Question 3: How does exercise influence the occurrence of depression in an individual?
# Model to predict effect of exercise on depression

# Convert categorical variables to factors
data$Exercise <- as.factor(data$Exercise)
data$Depression <- as.factor(data$Depression)

# Fit the logistic regression model
model <- glm(Depression ~ Exercise, data = data, family = binomial)

# Summary of the model to check the coefficients
summary(model)

# Predicting the values
predicted_values <- predict(model, newdata = data, type = "response")

# Converting probabilities to Yes or No
predicted_depression <- ifelse(predicted_values > 0.5, "Yes", "No")

# To check the accuracy of the model using a confusion matrix
table(Predicted = predicted_depression, Actual = data$Depression)




# Calculating metrics to check the performance of the model

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and test set (30%)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# Fit the model on the training data
model <- glm(Depression ~ Exercise, data = train_data, family = binomial)

# Make predictions on the test data
predicted_values <- predict(model, newdata = test_data, type = "response")
predicted_depression <- ifelse(predicted_values > 0.5, "Yes", "No")




# Convert predicted values to factor and ensure levels match actual values
predicted_depression <- factor(predicted_depression, levels = levels(test_data$Depression))

# Now create the confusion matrix
cm <- confusionMatrix(table(Predicted = predicted_depression, Actual = test_data$Depression))

# Precision is the same as Positive Predictive Value
precision <- cm$byClass["Pos Pred Value"]

# Recall is the same as Sensitivity
recall <- cm$byClass["Sensitivity"]

# F1 score is the harmonic mean of precision and recall
f1_score <- 2 * ((precision * recall) / (precision + recall))

print(paste("Precision: ", round(precision, 2)))
print(paste("Recall: ", round(recall, 2)))
print(paste("F1 Score: ", round(f1_score, 2)))


# Calculate the AUC-ROC
roc_obj <- roc(test_data$Depression, predicted_values)
auc <- auc(roc_obj)

print(paste("AUC-ROC: ", round(auc, 2)))



# PREDICTIOn

# Fit the full model with all predictors
full_model <- glm(Depression ~ Smoking_History + Exercise + Alcohol_Consumption + Fruit_Consumption + Green_Vegetables_Consumption, data = data, family = binomial)

# Perform backward elimination
backward_model <- stepAIC(full_model, direction = "backward")
summary(backward_model)

# Perform forward selection
null_model <- glm(Depression ~ 1, data = data, family = binomial)
forward_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(forward_model)

# Perform stepwise selection
stepwise_model <- stepAIC(full_model, direction = "both")

# Print the summary of the final model
summary(stepwise_model)
#ALL VARIABLES WERE SELECTED AS THEY WERE SIGNIFICANT




#Logistic Regression on the newer model.
# Convert categorical variables to factors
data$Smoking_History <- as.factor(data$Smoking_History)
data$Depression <- as.factor(data$Depression)
data$Exercise <- as.factor(data$Exercise)

# Fit the logistic regression model
model <- glm(Depression ~ Smoking_History + Exercise + Alcohol_Consumption + Fruit_Consumption + Green_Vegetables_Consumption, data = data, family = binomial)
summary(model)

# Predicting the values
predicted_values <- predict(model, newdata = data, type = "response")

# Converting probabilities to Yes or No
predicted_depression <- ifelse(predicted_values > 0.5, "Yes", "No")

# Convert predicted values to factor and ensure levels match actual values
predicted_depression <- factor(predicted_depression, levels = levels(data$Depression))

# Create a confusion matrix
cm <- confusionMatrix(table(Predicted = predicted_depression, Actual = data$Depression))

# Accuracy is the sum of the diagonal divided by the total observations
accuracy <- sum(diag(cm$table)) / sum(cm$table)

# Precision is the same as Positive Predictive Value
precision <- cm$byClass["Pos Pred Value"]

# Recall is the same as Sensitivity
recall <- cm$byClass["Sensitivity"]

# F1 score is the harmonic mean of precision and recall
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Print the metrics
print(paste("Accuracy: ", round(accuracy, 2)))
print(paste("Precision: ", round(precision, 2)))
print(paste("Recall: ", round(recall, 2)))
print(paste("F1 Score: ", round(f1_score, 2)))

# Calculate the AUC-ROC
roc_obj <- roc(data$Depression, predicted_values)
auc <- auc(roc_obj)

print(paste("AUC-ROC: ", round(auc, 2)))


#Lasso Regression

# Convert categorical variables to numeric
data$Depression <- ifelse(data$Depression == "Yes", 1, 0)
data$Smoking_History <- ifelse(data$Smoking_History == "Yes", 1, 0)
data$Exercise <- ifelse(data$Exercise == "Yes", 1, 0)

# Define the response variable and predictor variables for the model
y2 <- data$Depression
x2 <- data.frame(Smoking_History = data$Smoking_History, Exercise = data$Exercise, Alcohol_Consumption = data$Alcohol_Consumption, Fruit_Consumption = data$Fruit_Consumption, Green_Vegetables_Consumption = data$Green_Vegetables_Consumption)

# Perform Lasso and Ridge regression for the model
lasso_model2 <- glmnet(x2, y2, family = "binomial", alpha = 1)
ridge_model2 <- glmnet(x2, y2, family = "binomial", alpha = 0)



# Convert predictor data to matrix
x2_matrix <- as.matrix(x2)

# Predict using the Lasso model
lasso_pred <- predict(lasso_model2, newx = x2_matrix, type = "response")

# Convert predicted probabilities to binary predictions
lasso_pred_binary <- ifelse(lasso_pred > 0.5, 1, 0)





# Define your predictors and response
train_x <- as.matrix(data[, c("Smoking_History", "Exercise", "Alcohol_Consumption", "Fruit_Consumption", "Green_Vegetables_Consumption")])
train_y <- data$Depression

set.seed(256)
cv.lasso <- cv.glmnet(train_x, train_y, family = "binomial", nfolds = 10)
plot(cv.lasso)

log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

lasso.model.min <- glmnet(train_x, train_y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
coef(lasso.model.min)

lasso.model.1se <- glmnet(train_x, train_y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.1se)
coef(lasso.model.1se)

# Predict on your training data
lasso.preds.train <- predict(lasso.model.1se, newx = train_x, type = "response")

# Convert predictions to binary
lasso.preds.train.binary <- ifelse(lasso.preds.train > 0.5, 1, 0)

# Compute confusion matrix for train data
confusionMatrix(as.factor(lasso.preds.train.binary), as.factor(train_y))