#title: Random Forest and Logistic Regression
#author: Tianyang Jiang, Changhao Jiang

# R library
library(data.table)
library(randomForest)
library(sandwich)
library(car)

# Find scores with random forest

set.seed(1)
fit.forest= randomForest(Diabetes_binary ~ HvyAlcoholConsump + Smoker + 
                           PhysActivity + Fruits + Veggies,
                         data = balanced_diabetes, na.action = na.roughfix)
importance(fit.forest)

w1 = importance(fit.forest)[1] / sum(importance(fit.forest))
w2 = importance(fit.forest)[2] / sum(importance(fit.forest))
w3 = importance(fit.forest)[3] / sum(importance(fit.forest))
w4 = importance(fit.forest)[4] / sum(importance(fit.forest))
w5 = importance(fit.forest)[5] / sum(importance(fit.forest))

diabetes$score = -diabetes$HvyAlcoholConsump * w1 - diabetes$Smoker * w2 + 
  diabetes$PhysActivity * w3 + diabetes$Fruits * w4 + diabetes$Veggies * w5

summary(diabetes$score)
hist(diabetes$score)

# Check the number of scores

mean_score <- mean(diabetes$score)

sum(diabetes$score <= mean_score)
sum(diabetes$score > mean_score)

sum(diabetes[diabetes$score <= mean_score,]$Diabetes_binary == 1)
sum(diabetes[diabetes$score > mean_score,]$Diabetes_binary == 1)

diabetes$scoreCat <- ifelse(diabetes$score > mean_score, 1, 0)


# Fit logistic regression

# Only consider exposure

model_score <- glm(Diabetes_binary ~ scoreCat, data = diabetes, 
                   family = binomial)
summary(model_score)

Confint(model_score, vcov. = vcovHC(model_score, type = "HC0")) 

# Regression Adjustment

model_score_adj <- glm(Diabetes_binary ~ scoreCat + Sex + Age + Income,
                       data = diabetes, family = binomial)
summary(model_score_adj)

Confint(model_score_adj, vcov. = vcovHC(model_score_adj, type = "HC0"))




