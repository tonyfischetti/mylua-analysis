#!/usr/local/bin/Rscript --vanilla

options(echo=FALSE)
options(width=80)
options(warn=1)
options(scipen=10)
options(datatable.prettyprint.char=50)
options(datatable.print.class=TRUE)
options(datatable.print.keys=TRUE)


library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(libbib)

set.seed(2)

# --------------------------------------------------------------- #

args <- commandArgs(trailingOnly=TRUE)
trime <- as.integer(args[1])

# --------------------------------------------------------------- #

# trime <- 0


dat <- fread(sprintf("./wides/t%d.csv", trime))

setnames(dat, "Complication-FetalStress", "ComplicationFetalStress")
setnames(dat, "Complication-BloodOxygen", "ComplicationBloodOxygen")


dat[, deptarget:=Depression>0]

dat %>% dt_counts_and_percents("deptarget")


# NOTE: so this treats every OB episode as independent (bad)

dat %>% dt_del_cols("MyLua_Index_PatientID",
                    # "AntidepressantMedication",
                    "Depression"
                    # "MyLua_OBEpisode_ID" ## ??
                    )

setcolorder(dat, "deptarget")


dat <- dat[complete.cases(dat), ]

dat



library(glmnet)
library(caret)
library(rfUtilities)
library(randomForest)
library(xgboost)

# --------------------------------------------------------------- #






library(caret)

dat


trainIndex <- createDataPartition(dat$deptarget, p = .8,
                                  list = FALSE,
                                  times = 1)

dense <- as.matrix(model.matrix(deptarget ~ ., data=dat)[,-1])

trainX <- dense[trainIndex,]
trainY <- dat[trainIndex,]$deptarget+0
testX  <- dense[-trainIndex,]
testY  <- dat[-trainIndex,]$deptarget+0


dim(trainX)
dim(trainY)






bstDense <- xgboost(data = trainX,
                    label = trainY,
                    max.depth = 5,
                    eta = 1,
                    nthread = 2,
                    nrounds = 5,
                    objective = "binary:logistic",
                    verbose=2)

pred <- predict(bstDense, testX)
prediction <- as.numeric(pred > 0.5)
err <- round(1-mean(as.numeric(pred > 0.5) != testY), 2)
print(data.table(trimester=trime, acc=err))

# importance_matrix <- xgb.importance(model = bstDense)
# print(importance_matrix)
# xgb.plot.importance(importance_matrix = importance_matrix)


