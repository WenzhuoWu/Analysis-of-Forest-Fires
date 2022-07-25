library(ggplot2) 
library(MASS) 
library(class) 
library(e1071) 
library(randomForest) 
library(tidyverse) 
library(colorspace) 
library(ggpubr) 
library("PerformanceAnalytics")

        
# Data visualization 
raw.data <- read.csv("forestfires.csv",header = T) 
data <- raw.data [5:13] 
library("PerformanceAnalytics") 
chart.Correlation(data, histogram=TRUE, pch=19) 
data$Class <- factor(ifelse(data$area ==0, 'Absence','Presence')) 
A <- ggboxplot(data, x="Class", y="FFMC", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter", xlab="Occurence of forest fire", ylab="FFMC") 
B <- ggboxplot(data, x="Class", y="DMC", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"),add="jitter", xlab="Occurence of forest fire", ylab="DMC") 
C <- ggboxplot(data, x="Class", y="DC", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="DC") 
D <- ggboxplot(data, x="Class", y="ISI", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="ISI") 
E <- ggboxplot(data, x="Class", y="temp", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="temp") 
F <- ggboxplot(data, x="Class", y="RH", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="RH") 
G <- ggboxplot(data, x="Class", y="wind", color="Class", palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="wind") 
H <- ggboxplot(data, x="Class", y="rain", color="Class",palette = qualitative_hcl(n=2, palette = "Dark3"), add="jitter",xlab="Occurence of forest fire", ylab="rain") ggarrange(A,B,C,D,E,F,G,H, labels = c("A", "B", "C","D","E", "F","G", "H"), ncol = 4, nrow = 2)

# Logistic Regression
# Functions that created to calculate error rate and error rate without false positive 
er.cal <- function(x) { 
  return(1 - sum(diag(x))/sum(x)) } 
er.fp.cla <- function(y) { 
  return(y[2,1]/sum(y)) } 

# Full model 
fire = read.csv('forestfires.csv', header = T) 
fire$month = as.factor(fire$month) 
fire$day = as.factor(fire$day) 
fire$area = ifelse( test = fire$area == 0, yes = 0, no = 1) 
fire$area = as.factor(fire$area) 
train.data = fire[1:400,] 
test.data = fire[401:517,] 
fire_model = glm(area ~ X + DC + wind, data = train.data, family = 'binomial') 
y_pred_glm = predict(fire_model, newdata = test.data[c('X','DC','wind')]) 
summary(fire_model) 
confusion <- table(ifelse(y_pred_glm > 0.5, 1, 0), test_glm[,13]) 
confusion 
er.cal(confusion) 
er.fp.cla(confusion) 

# AIC for logistic regression 
stepAIC(fire_model, direction = "backward") 

# Reduced model 
fire_model_reduced <- glm(area ~ DMC+DC+temp+RH, data = train_glm, family = 'binomial') y_pred_glm = predict(fire_model_reduced, newdata = test_glm[c('DMC','DC','temp','RH')]) 
summary(fire_model_reduced) 
confusion <- table(ifelse(y_pred_glm > 0.5, 1, 0), test_glm[,13]) 
confusion 
er.cal(confusion) 
er.fp.cla(confusion)

# SVM 
classifier = svm(formula = area ~ temp + RH + wind, data = train.data, type = 'C-classification', kernel = 'linear') 
y_pred = predict(classifier, newdata = test.data[,9:11]) 
confusion = table(test.data[, 13], y_pred) 
confusion 
er.cal(confusion) 
er.fp.cla(confusion) 

# Random Forest 
set.seed(56) 
classifier_3 = randomForest(x = train.data[c('temp', 'RH' , 'wind')], y = train.data$area, ntree = 500) 
y_pred_3 = predict(classifier_3, newdata = test.data[c('temp', 'RH' , 'wind')]) 
confusion = table(test.data[, 13], y_pred_3) 
er.cal(confusion)
               