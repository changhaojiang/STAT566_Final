#title: Structure Learning
#author: Changhao Jiang

# R library and data input

library(Rgraphviz)
library(RBGL)
library(abind)
library(corpcor)
library(sfsmisc)
library(robustbase)
library(pcalg)
library(graph)
library(abind)
library(data.table)

# Using the FCI Algorithm to estimate an ancestral graph

# make Gaussian assumption

continuous <- diabetes[,c("scoreCat", "Stroke", "HighChol", "HighBP", "BMI", 
                          "HeartDiseaseorAttack", "Education", "AnyHealthcare", 
                          "Income", "Sex", "Age", "Diabetes_binary")]

n <- nrow(continuous)
p <- ncol(continuous)
indepTest <- gaussCItest
suffStat <- list(C=cor(continuous), n = n)

label_fci <- c("SC", "S", "HD", "HB", "BMI", 
               "HA", "E", "HC", "I", 
               "G", "A", "DB")
alpha <- 0.05
fci.fit <- fci(suffStat, indepTest, labels = label_fci, alpha = alpha, 
               verbose = TRUE)

## draw the graph

fci.fit@amat

if (plotcpdag) {
  plot(fci.fit)
}

# for part of discrete case due to computational cost

fci_dis <- diabetes[,c("scoreCat", "HighBP", 
                       "HeartDiseaseorAttack", "Education", "AnyHealthcare", 
                       "Diabetes_binary")]

V <- colnames(fci_dis)
sapply(fci_dis, function(v) nlevels(as.factor(v)))
suffStat <- list(dm = fci_dis, nlev = c(2,2,2,6,2,2), adaptDF = FALSE)

pc.D <- fci(suffStat, indepTest = disCItest, alpha = 0.01, 
            labels = V, verbose = TRUE)

## draw the graph
if (require(Rgraphviz)) {
  
  plot(pc.D, main = "Estimated CPDAG", labels = c("scoreCat", "HighBP", 
                                                  "HeartDiseaseorAttack", 
                                                  "Education", "AnyHealthcare", 
                                                  "Diabetes_binary"))
}

# Using the PC Algorithm for discete variables

# Split into 4 parts due to computation cost

dis1 <- diabetes[, c("scoreCat", "Stroke", "HeartDiseaseorAttack", "Sex", 
                     "Diabetes_binary", "Income")]
V1 <- colnames(dis1)
sapply(dis1, function(v) nlevels(as.factor(v)))
suffStat1 <- list(dm = dis1, nlev = c(2,2,2,2,2,8), adaptDF = FALSE)

dis2 <- diabetes[, c("HighChol", "HighBP", "Education", "AnyHealthcare", 
                     "Diabetes_binary", "scoreCat")]
V2 <- colnames(dis2)
sapply(dis2, function(v) nlevels(as.factor(v)))
suffStat2 <- list(dm = dis2, nlev = c(2,2,6,2,2,2), adaptDF = FALSE)

dis3 <- diabetes[, c("Education", "Stroke", "HeartDiseaseorAttack", "Sex", 
                     "Income")]
V3 <- colnames(dis3)
sapply(dis3, function(v) nlevels(as.factor(v)))
suffStat3 <- list(dm = dis3, nlev = c(6,2,2,2,8), adaptDF = FALSE)

dis4 <- diabetes[, c("Stroke", "HighChol", "HighBP", "HeartDiseaseorAttack", 
                     "AnyHealthcare", "Sex")]
V4 <- colnames(dis4)
sapply(dis4, function(v) nlevels(as.factor(v)))
suffStat4 <- list(dm = dis4, nlev = c(2,2,2,2,2,2), adaptDF = FALSE)

# Calculate the relationship for each part

pc.D1 <- pc(suffStat1, indepTest = disCItest, alpha = 0.01, 
            labels = V1, verbose = TRUE)
pc.D2 <- pc(suffStat2, indepTest = disCItest, alpha = 0.01, 
            labels = V2, verbose = TRUE)
pc.D3 <- pc(suffStat3, indepTest = disCItest, alpha = 0.01, 
            labels = V3, verbose = TRUE)
pc.D4 <- pc(suffStat4, indepTest = disCItest, alpha = 0.01, 
            labels = V4, verbose = TRUE)

# Draw the four plots

if (require(Rgraphviz)) {
  
  plot(pc.D1, main = "Estimated CPDAG", labels = c("scoreCat", "Stroke", 
                                                   "HeartDiseaseorAttack", "Sex", 
                                                   "Diabetes_binary", "Income"))
  
  plot(pc.D2, main = "Estimated CPDAG", labels = c("HighChol", "HighBP", 
                                                   "Education", "AnyHealthcare", 
                                                   "Diabetes_binary", "scoreCat"))
  
  plot(pc.D3, main = "Estimated CPDAG", labels = c("Education", "Stroke", 
                                                   "HeartDiseaseorAttack", 
                                                   "Sex", "Income"))
  
  plot(pc.D4, main = "Estimated CPDAG", labels = c("Stroke", "HighChol", 
                                                   "HighBP", 
                                                   "HeartDiseaseorAttack", 
                                                   "AnyHealthcare", "Sex"))
  
}




