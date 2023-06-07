#title: "R-learner"
#author: Tianyang Jiang

library(data.table)
library(ggplot2)
library(rlearner)

set.seed(1)
rW  = cbind(data$Age,data$Sex,data$Income)
rW = as.matrix(rW)
rA = data$scoreCat
rY = data$Diabetes_binary

rlasso_fit = rlasso(rW, rA, rY,k_folds = 10)
rlasso_est = predict(rlasso_fit, rW)


plotdata <- data.frame(Income = data$Income, 
                       Estimator =rlasso_est)
plotdata$Income_Group <- cut(plotdata$Income, breaks=c(0,2, 4,6, 8), 
                             labels=c("<15000","[15000,25000)","[25000,50000)", ">=50000"))

ggplot(plotdata, aes(x = Income_Group, y = Estimator, fill = Income_Group)) + 
  geom_boxplot() + 
  ggtitle("The result of R-learner(Income)") +
  xlab("Income Group") +
  ylab("Estimator") +
  scale_fill_brewer(palette="Set1") +
  theme_bw()

plotdata <- data.frame(Sex = data$Sex, 
                       Estimator =rlasso_est)
plotdata$Gender_Group <- ifelse(plotdata$Sex==0,"Female","Male")

ggplot(plotdata, aes(x = Gender_Group, y = Estimator, fill = Gender_Group)) + 
  geom_boxplot() + 
  ggtitle("The result of R-learner(Gender)") +
  xlab("Gender Group") +
  ylab("Estimator") +
  scale_fill_brewer(palette="Set2") +
  theme_bw()

plotdata <- data.frame(Age = data$Age, 
                       Estimator =rlasso_est)
plotdata$Age = as.factor(plotdata$Age)
age_labels <- c("18-24","25-29","30-34","35-39","40-44","45-49","51-54","55-59",
                "60-64","65-69","70-74","75-79",">=80")

ggplot(plotdata, aes(x = Age, y = Estimator, fill = Age)) + 
  geom_boxplot() + 
  ggtitle("The result of R-learner(Age)") +
  xlab("Age Group") +
  ylab("Estimator") +
  scale_fill_manual(values = rainbow(length(unique(plotdata$Age))), labels = age_labels) +
  theme_bw()



