# LIBRARIES
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(DataCombine)
library(descr)
library(fBasics)
library(stargazer)
library(sandwich)
library(lmtest)
library(splines)
library(readr)
library(gmodels)
library(mfx)
library(descr)
library(rstan)

# CLEAR MEMORY
rm(list = ls())

# SET WORKING DIRECTORY
setwd("C:\\Users\\kszavuly\\OneDrive\\CEU\\BAJP 5010 - Data Science for Business\\Project")
getwd()

## Reading and First Look at the Data
dataset <- read.csv("CATelcoCustomerChurnTrainingSample.csv")
colnames(dataset)
dataset$year <- NULL
dataset$month <- NULL
dataset$callingnum <- NULL
dataset$customerid <- NULL
sum(is.na(dataset))

## Analyzing Variables
summary(dataset)

## Predictor: Age
summary(dataset$age)
ggplot(dataset, aes(x = age)) +
  geom_histogram(stat = "bin", binwidth = 10, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 10", y = "Density")
reg1 <- lm(churn ~ age, data = dataset)
summary(reg1)
ggplot(data = dataset, aes(x = churn, y = age)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Annual Income
summary(dataset$annualincome)
ggplot(dataset, aes(x = annualincome)) +
  geom_histogram(stat = "bin", binwidth = 10000, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 10000", y = "Density")
reg2 <- lm(churn ~ annualincome, data = dataset)
summary(reg2)
ggplot(data = dataset, aes(x = churn, y = annualincome)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Call Drop Rate
summary(dataset$calldroprate)
ggplot(dataset, aes(x = calldroprate)) +
  geom_histogram(stat = "bin", binwidth = 0.01, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 0.01", y = "Density")
reg3 <- lm(churn ~ calldroprate, data = dataset)
summary(reg3)
ggplot(data = dataset, aes(x = churn, y = calldroprate)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Call Failure Rate
summary(dataset$callfailurerate)
ggplot(dataset, aes(x = callfailurerate)) +
  geom_histogram(stat = "bin", binwidth = 0.01, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 0.01", y = "Density")
reg4 <- lm(churn ~ callfailurerate, data = dataset)
summary(reg4)
ggplot(data = dataset, aes(x = churn, y = callfailurerate)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Customer Suspension
summary(dataset$customersuspended)
ggplot(dataset, aes(customersuspended, ..count..)) +
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Customer Suspended", y = "Volume")
reg5 <- lm(churn ~ customersuspended, data = dataset)
summary(reg5)

## Predictor: Education
summary(dataset$education)
ggplot(dataset, aes(education, ..count..)) +
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Education", y = "Volume")
reg6 <- lm(churn ~ education, data = dataset)
summary(reg6)

## Predictor: Gender
summary(dataset$gender)
ggplot(dataset, aes(gender, ..count..)) +
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Gender", y = "Volume")
reg7 <- lm(churn ~ gender, data = dataset)
summary(reg7)

## Predictor: Home Owner
summary(dataset$homeowner)
ggplot(dataset, aes(homeowner, ..count..)) +
geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Home Owner", y = "Volume")
reg8 <- lm(churn ~ homeowner, data = dataset)
summary(reg8)

## Predictor: Marital Status
summary(dataset$maritalstatus)
ggplot(dataset, aes(maritalstatus, ..count..)) +
geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Marital Status", y = "Volume")
reg9 <- lm(churn ~ maritalstatus, data = dataset)
summary(reg9)

## Predictor: Monthly Billed Amount
summary(dataset$monthlybilledamount)
ggplot(dataset, aes(x = monthlybilledamount)) +
  geom_histogram(stat = "bin", binwidth = 10, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 10", y = "Density")
reg10 <- lm(churn ~ monthlybilledamount, data = dataset)
summary(reg10)
ggplot(data = dataset, aes(x = churn, y = monthlybilledamount)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: No Additional Lines
summary(dataset$noadditionallines)

## Predictor: Number of Complaints
summary(dataset$numberofcomplaints)
ggplot(dataset, aes(x = numberofcomplaints)) +
  geom_histogram(stat = "bin", binwidth = 1, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 1", y = "Density")
reg11 <- lm(churn ~ numberofcomplaints, data = dataset)
summary(reg11)
ggplot(data = dataset, aes(x = churn, y = numberofcomplaints)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Number of Month Unpaid
summary(dataset$numberofmonthunpaid)
ggplot(dataset, aes(x = numberofmonthunpaid)) +
  geom_histogram(stat = "bin", binwidth = 1, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 1", y = "Density")
reg12 <- lm(churn ~ numberofmonthunpaid, data = dataset)
summary(reg12)
ggplot(data = dataset, aes(x = churn, y = numberofmonthunpaid)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Number of Days when the Contract Equipment Plan is Expiring
summary(dataset$numdayscontractequipmentplanexpiring)
ggplot(dataset, aes(x = numdayscontractequipmentplanexpiring)) +
  geom_histogram(stat = "bin", binwidth = 10, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 10", y = "Density")
reg13 <- lm(churn ~ numdayscontractequipmentplanexpiring, data = dataset)
summary(reg13)
ggplot(data = dataset, aes(x = churn, y = numdayscontractequipmentplanexpiring)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Occupation
summary(dataset$occupation)
ggplot(dataset, aes(occupation, ..count..)) +
geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Occupation", y = "Volume")
reg14 <- lm(churn ~ occupation, data = dataset)
summary(reg14)

## Predictor: Penalty to Switch
summary(dataset$penaltytoswitch)
ggplot(dataset, aes(x = penaltytoswitch)) +
  geom_histogram(stat = "bin", binwidth = 10, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 10", y = "Density")
reg15 <- lm(churn ~ penaltytoswitch, data = dataset)
summary(reg15)
ggplot(data = dataset, aes(x = churn, y = penaltytoswitch)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: State
summary(dataset$state)
ggplot(dataset, aes(state, ..count..)) +
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "State", y = "Volume")
reg16 <- lm(churn ~ state, data = dataset)
summary(reg16)

## Predictor: Total Minutes Used in Last Month
summary(dataset$totalminsusedinlastmonth)
ggplot(dataset, aes(x = totalminsusedinlastmonth)) +
  geom_histogram(stat = "bin", binwidth = 50, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 50", y = "Density")
reg17 <- lm(churn ~ totalminsusedinlastmonth, data = dataset)
summary(reg17)
ggplot(data = dataset, aes(x = churn, y = totalminsusedinlastmonth)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Unpaid Balance
summary(dataset$unpaidbalance)
ggplot(dataset, aes(x = unpaidbalance)) +
geom_histogram(stat = "bin", binwidth = 50, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 50", y = "Density")
reg18 <- lm(churn ~ unpaidbalance, data = dataset)
summary(reg18)
ggplot(data = dataset, aes(x = churn, y = unpaidbalance)) +
geom_point(size = 1.5, colour = "deepskyblue3") +
geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Uses Internet Service
summary(dataset$usesinternetservice)
ggplot(dataset, aes(usesinternetservice, ..count..)) +
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Uses Internet Service", y = "Volume")
reg19 <- lm(churn ~ usesinternetservice, data = dataset)
summary(reg19)

## Predictor: Uses Voice Service
summary(dataset$usesvoiceservice)
ggplot(dataset, aes(usesvoiceservice, ..count..)) +
geom_bar(colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "Uses Voice Service", y = "Volume")
reg20 <- lm(churn ~ usesvoiceservice, data = dataset)
summary(reg20)

## Predictor: Percentage Call Outside the Network
summary(dataset$percentagecalloutsidenetwork)
ggplot(dataset, aes(x = percentagecalloutsidenetwork)) +
geom_histogram(stat = "bin", binwidth = 0.1, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 0.1", y = "Density")
reg21 <- lm(churn ~ percentagecalloutsidenetwork, data = dataset)
summary(reg21)
ggplot(data = dataset, aes(x = churn, y = percentagecalloutsidenetwork)) +
geom_point(size = 1.5, colour = "deepskyblue3") +
geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Total Call Duration
summary(dataset$totalcallduration)
ggplot(dataset, aes(x = totalcallduration)) +
geom_histogram(stat = "bin", binwidth = 1000, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 1000", y = "Density")
reg22 <- lm(churn ~ totalcallduration, data = dataset)
summary(reg22)
ggplot(data = dataset, aes(x = churn, y = totalcallduration)) +
geom_point(size = 1.5, colour = "deepskyblue3") +
geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Predictor: Average Call Duration
summary(dataset$avgcallduration)
ggplot(dataset, aes(x = avgcallduration)) +
  geom_histogram(stat = "bin", binwidth = 100, colour = "deepskyblue3", fill = "deepskyblue3") + labs(x = "X Axis, binwidth: 100", y = "Density")
reg23 <- lm(churn ~ avgcallduration, data = dataset)
summary(reg23)
ggplot(data = dataset, aes(x = churn, y = avgcallduration)) +
  geom_point(size = 1.5, colour = "deepskyblue3") +
  geom_smooth(method = "lm", colour = "seagreen3", se = T)

## Prepare Final Dataset
finaldataset <- subset(dataset, dataset$totalcallduration < 10001)
finaldataset$customersuspended <- NULL
finaldataset$noadditionallines <- NULL
finaldataset$customerid <- NULL
finaldataset$customercallingnum <- NULL
finaldataset$month <- NULL
finaldataset$year <- NULL

lst <- strsplit(as.character(finaldataset$education),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$education <- NULL

lst <- strsplit(as.character(finaldataset$gender),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$gender <- NULL

lst <- strsplit(as.character(finaldataset$homeowner),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
colnames(res)[which(names(res) == "Yes")] <- "homeowner_yes"
colnames(res)[which(names(res) == "No")] <- "homeowner_no"
finaldataset <- cbind(finaldataset, res)
finaldataset$homeowner <- NULL

lst <- strsplit(as.character(finaldataset$maritalstatus),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$maritalstatus <- NULL

lst <- strsplit(as.character(finaldataset$occupation),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$occupation <- NULL

lst <- strsplit(as.character(finaldataset$state),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
finaldataset <- cbind(finaldataset, res)
finaldataset$state <- NULL

lst <- strsplit(as.character(finaldataset$usesinternetservice),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
colnames(res)[which(names(res) == "Yes")] <- "usesinternetservice_yes"
colnames(res)[which(names(res) == "No")] <- "usesinternetservice_no"
finaldataset <- cbind(finaldataset, res)
finaldataset$usesinternetservice <- NULL

lst <- strsplit(as.character(finaldataset$usesvoiceservice),"-")
lvl <- unique(unlist(lst))
res <- data.frame(do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), stringsAsFactors=FALSE)
colnames(res)[which(names(res) == "Yes")] <- "usesvoiceservice_yes"
colnames(res)[which(names(res) == "No")] <- "usesvoiceservice_no"
finaldataset <- cbind(finaldataset, res)
finaldataset$usesvoiceservice <- NULL

## Main remarks after our data exploration

## Split Train/Test Datasets
set.seed(123)
N <- nrow(finaldataset)
idx_train <- sample(1:N,N/2)
idx_valid <- sample(base::setdiff(1:N, idx_train), N/4)
idx_test <- base::setdiff(base::setdiff(1:N, idx_train),idx_valid)
d_train <- finaldataset[idx_train,]
d_valid <- finaldataset[idx_valid,]
d_test  <- finaldataset[idx_test,]

## Modeling

## Setup

library(h2o)
localH2O = h2o.init()

dx_train <- as.h2o(d_train)
dx_train$churn <- as.factor(dx_train$churn)
dx_valid <- as.h2o(d_valid)
dx_valid$churn <- as.factor(dx_valid$churn)
dx_test <- as.h2o(d_test)
dx_test$churn <- as.factor(dx_test$churn)

## Random forest 1
{
  md_rf <- h2o.randomForest(x = c(2:83), 
  y = "churn", 
  training_frame = dx_train, 
  mtries = -1, 
  ntrees = 100, 
  max_depth = 5, 
  nbins = 20)
}

md_rf

h2o.auc(md_rf)

h2o.auc(h2o.performance(md_rf, dx_valid))

## Random forest 2
{
  md_rf <- h2o.randomForest(x = c(2:83), 
  y = "churn", 
  training_frame = dx_train, 
  mtries = -1, 
  ntrees = 200, 
  max_depth = 10, 
  nbins = 20)
}

md_rf

h2o.auc(md_rf)

h2o.auc(h2o.performance(md_rf, dx_valid))

## Random forest 3
{
  md_rf <- h2o.randomForest(x = c(2:83), 
  y = "churn", 
  training_frame = dx_train, 
  mtries = -1, 
  ntrees = 1000, 
  max_depth = 15, 
  nbins = 20)
}

md_rf

h2o.auc(md_rf)

h2o.auc(h2o.performance(md_rf, dx_valid))

## GBM 1
{
  md_gbm <- h2o.gbm(x = c(2:83), 
  y = "churn", 
  training_frame = dx_train, 
  ntrees = 100, 
  max_depth = 5, 
  learn_rate = 0.01) 
}

md_gbm

h2o.auc(md_gbm)

h2o.auc(h2o.performance(md_gbm, dx_valid))

## GBM 2
{
  md_gbm <- h2o.gbm(x = c(2:83), 
  y = "churn", 
  training_frame = dx_train, 
  ntrees = 200, 
  max_depth = 10, 
  learn_rate = 0.03) 
}

md_gbm

h2o.auc(md_gbm)

h2o.auc(h2o.performance(md_gbm, dx_valid))

## GBM 3
{
  md_gbm <- h2o.gbm(x = c(2:83), 
                    y = "churn", 
                    training_frame = dx_train, 
                    ntrees = 1000, 
                    max_depth = 15, 
                    learn_rate = 0.1) 
}

md_gbm

h2o.auc(md_gbm)

h2o.auc(h2o.performance(md_gbm, dx_valid))

## Random forest model 3 test
{
  md_rf <- h2o.randomForest(x = c(2:83), 
                            y = "churn", 
                            training_frame = dx_train, 
                            mtries = -1, 
                            ntrees = 1000, 
                            max_depth = 15, 
                            nbins = 20)
}

md_rf

h2o.auc(md_rf)

h2o.auc(h2o.performance(md_rf, dx_test))

# Halt and catch fire