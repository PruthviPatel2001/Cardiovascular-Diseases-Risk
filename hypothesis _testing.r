# Name- Pruthvi Samir Patel
# Class - ALY6015 Intermediate Analytics CRN - 21626

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
#-----------------------

data <- read.csv("Cardiovascular_Diseases_Risk.csv")
names(data)

str(data)
summary(data)

#---- Question 1 ----

#Question : Question/Problem: Can we build a predictive model using logistic regression to 
#assess the risk of cardiovascular diseases based on various health indicators and lifestyle factors? 
#Specifically, we want to understand the influence of exercise, cancer history, mental health (depression), 
#diabetes, arthritis, and other factors on the likelihood of developing heart disease.

#Exploratory Data Analysis 

ggplot(data, aes(x = Age_Category, fill = factor(Heart_Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Analysis of Age and Heart Disease",
       x = "Age group", y = "Count")

ggplot(data, aes(x = General_Health, fill = factor(Heart_Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Analysis of General Health and Heart Disease",
       x = "General Health", y = "Count")

ggplot(data, aes(x = Smoking_History, fill = factor(Heart_Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Analysis of Smoking History and Heart Disease",
       x = "Smoking History", y = "Count")

ggplot(data, aes(x = Diabetes, fill = factor(Heart_Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Analysis of Diabetes and Heart Disease",
       x = "Diabetes", y = "Count")

ggplot(data, aes(x = Arthritis, fill = factor(Heart_Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Analysis of Arthritis and Heart Disease",
       x = "Arthritis", y = "Count")


# Chi-square test

#The Chi-Square Test of Independence will be used to assess the association 
#between presence or absence of heart disease (response variable) and smoking history, 
#diabetes, and arthritis (independent variables).

#Exploring the influence of smoking history on heart disease

#Hypothesis statement

#H0: There is no association between heart disease and smoking history.

#H1: There is an association between heart disease and smoking history

contingency_table <- table(data$Heart_Disease, 
                           data$Smoking_History)
contingency_table

data$Smoking_History <- as.factor(data$Smoking_History)

rownames(contingency_table) <- c("No_Heart_Disease", "Heart_Disease")
colnames(contingency_table) <- levels(data$Smoking_History)

chi_square_test <- chisq.test(contingency_table)

print(chi_square_test)

if (chi_square_test$p.value < 0.05) {
  print("Reject the null hypothesis. Hence, there is a significant association between smoking history and presence/absence of heart disease")
} else {
  print("Fail to reject the null hypothesis for interaction. Hence, there is no significant association between smoking history and heart disease.")
}

#Exploring the influence of diabetes on heart disease

#Hypothesis statement

#H0: There is no association between diabetes and smoking history.

#H1: There is an association between diabetes and smoking history

contingency_table <- table(data$Heart_Disease, 
                           data$Diabetes)
contingency_table

data$Diabetes <- as.factor(data$Diabetes)

rownames(contingency_table) <- c("No_Heart_Disease", "Heart_Disease")
colnames(contingency_table) <- levels(data$Diabetes)

chi_square_test <- chisq.test(contingency_table)

print(chi_square_test)

if (chi_square_test$p.value < 0.05) {
  print("Reject the null hypothesis. Hence, there is a significant association between diabetes and presence/absence of heart disease")
} else {
  print("Fail to reject the null hypothesis for interaction. Hence, there is no significant association between diabetes and heart disease.")
}

#Exploring the influence of Arthritis on heart disease

#Hypothesis statement

#H0: There is no association between Arthritis and smoking history.

#H1: There is an association between Arthritis and smoking history

contingency_table <- table(data$Heart_Disease, 
                           data$Arthritis)
contingency_table

data$Arthritis <- as.factor(data$Arthritis)

rownames(contingency_table) <- c("No_Heart_Disease", "Heart_Disease")
colnames(contingency_table) <- levels(data$Arthritis)

chi_square_test <- chisq.test(contingency_table)

print(chi_square_test)

if (chi_square_test$p.value < 0.05) {
  print("Reject the null hypothesis. Hence, there is a significant association between Arthritis and presence/absence of heart disease")
} else {
  print("Fail to reject the null hypothesis for interaction. Hence, there is no significant association between Arthritis and heart disease.")
}


#------ Question 2 ----

# Question/Problem: How does cardiovascular health relate to Body Mass Index (BMI)? 
# We want to model the association between BMI and other characteristics 
# like exercise, eating and few others habits using linear regression. 
# We will gain a better understanding of how lifestyle decisions affect BMI and, in turn, 
# cardiovascular health thanks to this analysis.

# 1. Exploring the Influence of Exercise and General Health on BMI

# Hypothesis Statement:

# H0 (Null Hypothesis): There is not enough interaction between "Exercise" and "General Health" to explain the variation in BMI.

# H1 (Alternative Hypothesis): "Exercise" and "General Health" interact significantly to explain BMI variance.

# Test type : Two-way ANOVA

data$Exercise <- as.factor(data$Exercise)
data$General_Health <- as.factor(data$General_Health)

anova_result <- aov(BMI ~ Exercise * General_Health, data = data)

summary(anova_result)

alpha <- 0.05

p_value_for_interaction <- summary(anova_result)[[1]]$`Pr(>F)`[3]
p_value_for_interaction

f_value_for_interaction <- summary(anova_result)[[1]]$`F value`[3]
f_value_for_interaction

interaction_df <- summary(anova_result)[[1]]$`Df`[3]
interaction_df

test_residuals <- summary(anova_result)[[1]]$`Df`[4]
test_residuals

interaction_critical_value <- qf(1 - alpha, interaction_df, test_residuals)
interaction_critical_value

if (p_value_for_interaction < 0.05) {
  print("Reject the null hypothesis for interaction. Hence, 'Exercise' and 'General Health' interact significantly to explain BMI variance.")
} else {
  print("Fail to reject the null hypothesis for interaction. Hence, There is not enough interaction between 'Exercise' and 'General Health' to explain the variation in BMI.")
}

if(f_value_for_interaction > interaction_critical_value){
  print("Reject the null hypothesis for interaction. Hence, 'Exercise' and 'General Health' interact significantly to explain BMI variance.")
} else {
  print("Fail to reject the null hypothesis for interaction. Hence, There is not enough interaction between 'Exercise' and 'General Health' to explain the variation in BMI.")
}


ggplot(data, aes(x = Exercise, y = BMI, fill = General_Health)) +
  geom_boxplot() +
  labs(title = "BMI Distribution Across Exercise and General Health Categories",
       x = "Exercise",
       y = "BMI",
       fill = "General Health") +
  theme_minimal()


# 2. Relationship Between Fruit Consumption and BMI

# H0 (Null Hypothesis): There is no significant relationship between mean BMI and levels of fruit consumption.
# H1 (Alternative Hypothesis): There is a substantial link between mean BMI and fruit consumption.

linear_model <- lm(BMI ~ Fruit_Consumption, data = data)

summary(linear_model)

significance_level <- 0.05

if (summary(linear_model)$coefficients["Fruit_Consumption", "Pr(>|t|)"] < significance_level) {
  cat("Reject the null hypothesis. There is a substantial link between mean BMI and fruit consumption.\n")
} else {
  cat("Accept the null hypothesis. There is no significant relationship between mean BMI and levels of fruit consumption.\n")
}

ggplot(data, aes(x = Fruit_Consumption, y = BMI)) +
  geom_point(color="coral") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adding a linear regression line
  labs(title = "Effect of Fruit Consumption on BMI",
       x = "Fruit Consumption",
       y = "BMI") +
  theme_minimal()

# 3. Relationship Between Green_Vegetables_Consumption and BMI
# H0 (Null Hypothesis): There is no significant relationship between BMI and green vegetable consumption.
# H1 (Alternative Hypothesis): There is a substantial relationship between BMI and green vegetable consumption.

linear_model_two <- lm(BMI ~ Green_Vegetables_Consumption, data = data)

summary(linear_model_two)

if (summary(linear_model_two)$coefficients["Green_Vegetables_Consumption", "Pr(>|t|)"] < significance_level) {
  cat("Reject the null hypothesis. There is a substantial relationship between BMI and green vegetable consumption.\n")
} else {
  cat("Accept the null hypothesis.There is no significant relationship between BMI and green vegetable consumption.\n")
}

ggplot(data, aes(x = Green_Vegetables_Consumption, y = BMI)) +
  geom_point(color="darkolivegreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adding a linear regression line
  labs(title = "Effect of Green Vegetable Consumption on BMI",
       x = "Green Vegetable Consumption",
       y = "BMI") +
  theme_minimal()

# BMI effect on heart disease

ggplot(data, aes(x = Heart_Disease, y = BMI, fill = Heart_Disease)) +
  geom_boxplot() +
  labs(title = "BMI Distribution Among People with and Without Heart Disease",
       x = "Heart Disease",
       y = "BMI") +
  scale_fill_manual(values = c("darkseagreen1","aquamarine"))+
  theme_minimal()

#---- Question 3 ----

# Question/Problem: Can we predict the mental health of an individual (specifically, the presence of depression) 
# based on their exercise habits? We aim to model the association between exercise 
# and depression using logistic regression. This analysis will help us understand 
# how exercise might affect an individual’s mental health.
# Perform a chi-squared test

chisq_test <- chisq.test(data$Exercise, data$Depression)

# Print the p-value
print(chisq_test$p.value)


# Create a bar plot
ggplot(data, aes(x = Exercise, fill = Depression)) +
  geom_bar(position = "dodge") +
  labs(x = "Exercise", y = "Count", fill = "Depression") +
  theme_minimal()