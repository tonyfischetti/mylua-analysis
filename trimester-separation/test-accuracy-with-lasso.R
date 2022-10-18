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
library(here)
library(glmnet)
library(rfUtilities)


setwd(here("trimester-separation"))

set.seed(5)



# --------------------------------------------------------------- #








do.it <- function(trime){

  dat <- fread(sprintf("./wides/t%d.csv", trime))

  # added 2022-09-04
  dat %>% dt_del_cols("PostnatalSleep", "marital_status", "hisp_latino_p",
                      "race", "PregnancyOverseeing-Age35plus", "NonMinority")

  dt_set_clean_names(dat, lower=FALSE)
  dat[, deptarget:=Depression>0]

  dat %>% dt_counts_and_percents("deptarget")

  dat %>% dt_del_cols("MyLua_Index_PatientID", "Depression")

  setcolorder(dat, "deptarget")

  dat <- dat[complete.cases(dat), ]

  DT <- copy(dat)

  trainIndex <- createDataPartition(DT$deptarget, p = .8, list = FALSE, times = 1)
  X <- model.matrix(deptarget ~ ., data=DT)[, -1]
  Y <- as.matrix(DT[, 1]+0)
  trainX <- X[trainIndex,]
  trainY <- Y[trainIndex,]
  testX <- X[-trainIndex,]
  testY <- Y[-trainIndex,]
  print(table(testY))


  cvfit <- cv.glmnet(trainX, trainY, alpha=1, standardize=TRUE, family="binomial")

  # print(coef(cvfit, s="lambda.min"))
  print(coef(cvfit, s="lambda.1se"))

  # coef(cvfit, s="lambda.min") -> tmp
  coef(cvfit, s="lambda.1se") -> tmp
  tmp <- as.data.table(as.matrix(tmp), keep.rownames=TRUE)
  tmp <- tmp[-1,]
  setnames(tmp, c("feature", "coefficient"))
  tmp[coefficient==0, coefficient:=NA]
  tmp[order(-coefficient)] %>%
    fwrite(sprintf("./results/coeffs/lasso-coeffs-%d.csv", trime), sep=",")

  # preds <- predict(cvfit, newx=testX, s="lambda.min", type="response")
  preds <- predict(cvfit, newx=testX, s="lambda.1se", type="response")
  preds <- fifelse(preds>0.5, 1, 0)
  # print(table(preds))
  # message("> ")
  # print(ci.auc(preds, testY, conf.level = 0.95, method="bootstrap"))
  obj <- accuracy(preds, testY)
  return(data.table(trimester=trime,
                    vari=c("PCC", "AUC", "Sensitivity", "Specificity",
                           "FScore", "TypeIError", "TypeIIError"),
                    val=c(obj$PCC, obj$auc, obj$sensitivity,
                          obj$specificity, obj$f.score, obj$typeI.error,
                          obj$typeII.error)))
}




0:8 %>%
  lapply(do.it) %>%
  rbindlist -> tmp

tmp %>% dcast(trimester ~ vari) %>%
  fwrite("./results/lasso.csv")



