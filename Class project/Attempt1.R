# Week 3 MGMT 605 MGMT 605 OL GENERALIZED LINEAR MODELS
## Exr_week03
### Header
#### Coded by Javor Mladenoff

Sys.time()
rm(list = ls())
# set my working directory 
setwd('C:\\Users\\jmlad\\Desktop\\Moravian College\\MGMT 605 OL GENERALIZED LINEAR MODELS\\Class project')
getwd()

#install.packages('igraph')
library(igraph)
library(dplyr)
library(ggplot2)
library(ggraph)
# load packages for network exploration
library(readr)
library(igraph)
#install.packages('mlbench')
library(car)
library(mlbench)
library(caret)
#install.packages('caretEnsemble')
library(caretEnsemble)
#install.packages('wordcloud')
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages('wordcloud2')
library(wordcloud2)
#install.packages("tm")
library(tm)
#install.packages('datarium')
library(datarium)
library(tidyverse)
library(ggpubr)
library(MASS)
library(ISLR)
library(class)
library(boot)
#install.packages('faraway')
library(faraway)
#install.packages('leaps')
library(leaps)
#install.packages('glmnet')
library(glmnet)
#install.packages('pls')
library(pls)
#install.packages('gam')
library(gam)
#install.packages('splines')
library(splines)
#install.packages('MVA')
library(MVA)
library(psych)
library(Matrix)
#install.packages('VIM')
library (VIM)
library(corrplot)
#install.packages('WRS')
library(WRS)
#install.packages('nortest')
library(nortest)
#install.packages('multcomp')
library(multcomp)
#install.packages('stats')
library(stats)
#install.packages('biotools')
library(biotools)
#install.packages('mvnormtest')
library(mvnormtest)
library(psych)
#install.packages('pastecs')
library(pastecs)
library(caret)
#install.packages('olsrr')
library(olsrr)
library(gvlma)
#install.packages('rrcov')
library(rrcov)
#install.packages('nlme')
library(nlme)
#install.packages("RLRsim")
library(RLRsim)
#install.packages('GPArotation')
library(GPArotation)
#install.packages('fpc')
library(fpc)
#install.packages("NbClust")
library("NbClust")
#install.packages("caTools")
library(caTools)

data01 <- read.csv('Environment_Temperature_change_E_All_Data_NOFLAG.csv', header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
head(data01)
str(data01)
sum(is.na(data01)) # [1] 60
data02 <- na.omit(data01)
head(data02)
str(data02)
describe(data02)
names(data02)
lapply(data02, function(x) { length(which(is.na(x))) })
str(data02)
data02$areac <- factor(data02$Area.Code)
data02$area <- factor(data02$Area)
data02$monthc <- factor(data02$Month.Code)
data02$month <- factor(data02$Months)
data02$element <- factor(data02$Element)
data02$unit <- factor(data02$Unit)
str(data02)
data02$Area.Code <- NULL
data02$Area <- NULL
data02$Months <- NULL
data02$Element <- NULL
data02$Unit <- NULL
str(data02)
# Model training and evaluation

data03<-data02[complete.cases(data02),]
head(data03)
str(data03)
l <- sapply(data03, function(x) is.factor(x))
head(l)

lapply(data03[c("Y1962", "Y1963", "Y1964","Y1965","Y1972")], unique)
data03$areac <- NULL
data03$area <- NULL
data03$unit <- NULL
str(data03)
data03
sum(is.na(data03))
### Attempt for Machine Learning Approach
set.seed(42) 
sampleSplit <- sample.split(Y=data03$month, SplitRatio=0.7) 
trainSet <- subset(x=data03, sampleSplit==TRUE) 
testSet <- subset(x=data03, sampleSplit==FALSE)
# train the model with the function.
model_02 <- lm(Element.Code ~ element + Y1963 + Y1968 + Y1972 + Y1986 + Y1999 + Y2018, data=trainSet, family=binomial(link='logit'))
sum(is.na(model_02))
summary(model_02)
model_02
probabs <- predict(model_02, testSet, type='response') 
preds <- ifelse(probabs > 0.5, 1, 0)
preds
# Work in progress......














###############################################################################################
trainSet
testSet
names(model_02)

confusionMatrix(factor(preds), factor(testSet$Element.Code))
#ifelse(n <- sapply(data03, function(x) length(levels(x))) == 1, "DROP", "NODROP")





