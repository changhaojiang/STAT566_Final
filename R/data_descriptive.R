#title: Data Descriptive Analysis
#author: Liuye Huang

library(knitr)
library(tidyverse)
library(rigr)
library(dplyr)
library(gtsummary)
library(flextable)
library(ggplot2)
library(gridExtra) 
library(patchwork) 

data <- read.csv("Diabetes.csv")

data$ageCat <- factor(data$Age, levels = 1:13, labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", ">=80") )

data$eduCat <- factor(data$Education, levels = 1:6, labels = c("Never attended school or only kindergarten", "Elementary", "Some high school", "High school graduate", "Some college or technical school", "College graduate"))

data$incomeCat <- factor(data$Income, levels = 1:8, labels = c("< $10,000", "[$10,000, $15,000)", "[$15,000, $20,000)", "[$20,000, $25,000)", "[$25,000, $35,000)", "[$35,000, $50,000)", "[$50,000, $75,000)", ">= $75,000"))

data$diaCat <- factor(data$Diabetes_binary, levels = 0:1, labels = c("Healthy", "Prediabetes or diabetes"))

data$scoreCat2 = ifelse(data$scoreCat == "> mean score", 1, 0)

data$scoreCat2 <- as.numeric(data$scoreCat2)
data$Diabetes_binary <- as.numeric(data$Diabetes_binary)
data$gender <- factor(data$Sex, levels = 0:1, labels = c("Female", "Male"))

tabl1 <- data %>% select(Diabetes_binary, Sex, ageCat, eduCat, incomeCat, scoreCat, Smoker,
                         PhysActivity, Fruits, Veggies, HvyAlcoholConsump, HighBP, HighChol, 
                         BMI, Stroke, HeartDiseaseorAttack, AnyHealthcare) %>%
  tbl_summary(by = Diabetes_binary, label = list(Diabetes_binary ~ "Diagnosis of diabetes",
                                                 Sex ~ "Gender", ageCat ~ "5-year age category", eduCat ~ "Education", incomeCat ~ "Income", scoreCat ~ "Lifestyle composition score", Smoker ~ "Have smoked at least 100 cigarettes lifetime", PhysActivity ~ "Physical activity in past 30 days (not including job)", Fruits ~ "Consume fruit >= 1 times per day", Veggies ~ "Consume vegetables >= 1 times per day", HvyAlcoholConsump ~ "Heavy drinkers", HighBP ~ "High blood pressure", HighChol ~ "High cholesterol", Stroke ~ "Ever had a stroke", HeartDiseaseorAttack ~ "Coronary heart disease or myocardial infarction", AnyHealthcare ~ "Any health coverage"), 
              statistic = all_continuous() ~ "{mean}({sd})",
              type = all_continuous() ~ 'continuous2',
              missing = "no") %>% add_p()

myTable1 <- as_flex_table(tabl1) %>% set_caption("Table 2: Demographic, lifestyle, and medical characteristics of the healthy and participants diagnosed with prediabetes or diabetes")

# current confounders: age, income, gender
# plots on income
ggplot(data, 
                      aes(x=factor(incomeCat), fill = factor(diaCat))) + 
  geom_bar(position = "dodge", width = 0.8)+
  scale_fill_manual(values = c("Healthy" = "gold3", "Prediabetes or diabetes" = "purple4"))+
  labs(x = "Income category", y = "Count", fill = "Diabetes status") +
  facet_wrap(~scoreCat, ncol =1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Income of participants in different lifestyle score category and diabetes status")

# plots on age categories 
ggplot(data, aes(x=factor(ageCat), fill = factor(diaCat))) + 
  geom_bar(position = "dodge", width = 0.8)+
  scale_fill_manual(values = c("Healthy" = "gold3", "Prediabetes or diabetes" = "purple4"))+
  labs(x = "Age category", y = "Count", fill = "Diabetes status") +
  facet_wrap(~scoreCat, ncol =1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Age of participants in different lifestyle score category and diabetes status")

# plot on gender
ggplot(data, 
                      aes(x=gender, fill = factor(diaCat))) + 
  geom_bar(position = "dodge", width = 0.8)+
  scale_fill_manual(values = c("Healthy" = "gold3", "Prediabetes or diabetes" = "purple4"))+
  labs(x = "Gender", y = "Count", fill = "Diabetes status") +
  facet_wrap(~scoreCat, ncol =1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Gender of participants in different lifestyle score category and diabetes status")

# plot of different lifestyle factors on diabetes
ggplot(data, aes(x = factor(Smoker), fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Have smoked at least 100 cigarettes 
lifetime and diabetes status",
       x = "Smoker", y = "Frequency")

ggplot(data, aes(x = factor(PhysActivity), fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Physical activity in past 30 days 
(not including job) and diabete status",
       x = "Physical activity", y = "Frequency")

ggplot(data, aes(x = factor(Fruits), fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Consume fruits at least 
1 times per day and diabete status",
       x = "Fruits consumption", y = "Frequency")

ggplot(data, aes(x = factor(Veggies), fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Consume vegetables at least 
1 times per day and diabete status",
       x = "Vegetables consumption", y = "Frequency")

ggplot(data, aes(x = factor(HvyAlcoholConsump), fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Heavy drinkers and diabete status",
       x = "Heavy drinkers", y = "Frequency")

ggplot(data, aes(x = scoreCat, fill = (diaCat))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("gold3", "purple4"))+
  labs(title = "Combination score categories 
and diabete status",
       x = "Combination score categories", y = "Frequency")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

