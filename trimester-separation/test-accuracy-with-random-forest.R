#!/usr/local/bin/Rscript --vanilla

options(echo=TRUE)
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
library(caret)
library(xgboost)

set.seed(2)




# library(glmnet)
# library(rfUtilities)

library(randomForest)

# --------------------------------------------------------------- #







dat


do.it <- function(trime){

  dat <- fread(sprintf("./wides/t%d.csv", trime))
  setnames(dat, "Complication-FetalStress", "ComplicationFetalStress")
  setnames(dat, "Complication-BloodOxygen", "ComplicationBloodOxygen")

  dat[, deptarget:=Depression>0]

  dat %>% dt_counts_and_percents("deptarget")

  dat %>% dt_del_cols("MyLua_Index_PatientID", "Depression")

  setcolorder(dat, "deptarget")

  dat <- dat[complete.cases(dat), ]

  DT <- copy(dat)

  trainIndex <- createDataPartition(DT$deptarget, p = .8, list = FALSE, times = 1)

  dense <- as.matrix(model.matrix(deptarget ~ ., data=DT)[,-1])

  trainX <- dense[trainIndex,]
  trainY <- DT[trainIndex,]$deptarget+0
  testX  <- dense[-trainIndex,]
  testY  <- DT[-trainIndex,]$deptarget+0

  bstDense <- xgboost(data = trainX,
                      label = trainY,
                      max.depth = 5,
                      eta = 1,
                      nthread = 2,
                      nrounds = 5,
                      objective = "binary:logistic",
                      verbose=0)

  pred <- predict(bstDense, testX)
  prediction <- as.numeric(pred > 0.5)
  err <- round(1-mean(as.numeric(pred > 0.5) != testY), 2)
  return(data.table(trimester=trime, acc=err))
}


# importance_matrix <- xgb.importance(model = bstDense)
# print(importance_matrix)
# xgb.plot.importance(importance_matrix = importance_matrix)


do.it(0)
do.it(1)
do.it(2)

