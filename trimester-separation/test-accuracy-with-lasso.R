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
library(DescTools)
library(yardstick)
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




do.a.rep <- function(DT, trime, trial, trainIndex){
  X <- model.matrix(deptarget ~ ., data=DT)[, -1]
  Y <- as.matrix(DT[, 1]+0)
  trainX <- X[trainIndex,]
  trainY <- Y[trainIndex,]
  testX <- X[-trainIndex,]
  testY <- Y[-trainIndex,]

  cvfit <- cv.glmnet(trainX, trainY, alpha=1, standardize=TRUE, family="binomial")

  coef(cvfit, s="lambda.1se") -> tmp
  tmp <- as.data.table(as.matrix(tmp), keep.rownames=TRUE)
  tmp <- tmp[-1,]
  setnames(tmp, c("feature", "coefficient"))
  tmp[coefficient==0, coefficient:=NA]

  cres <- copy(tmp)
  cres[, trimester:=trime]
  cres[, trial:=trial]
  setcolorder(cres, c("trimester", "trial", "feature", "coefficient"))

  preds_resp <- predict(cvfit, newx=testX, s="lambda.1se", type="response")
  preds <- fifelse(preds_resp>0.5, 1, 0)

  obj <- accuracy(preds, testY)

  brier <- BrierScore(preds_resp, testY)

  tmp <- data.table(truth=as.factor(testY), estimate=as.factor(preds))
  ppv <- ppv(tmp, truth, estimate, estimator="binary")[1, 3] %>% unlist
  npv <- npv(tmp, truth, estimate, estimator="binary")[1, 3] %>% unlist

  rres <- data.table(trimester=trime,
                     trial=trial,
                     vari=c("PCC", "AUC", "Sensitivity", "Specificity",
                            "FScore", "TypeIError", "TypeIIError",
                            "BrierScore", "PPV", "NPV"),
                     val=c(obj$PCC, obj$auc, obj$sensitivity,
                           obj$specificity, obj$f.score, obj$typeI.error,
                           obj$typeII.error, brier, ppv, npv))
  return(list(rres=rres, cres=cres))
}







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

  trainIndices <- createDataPartition(DT$deptarget, p=.8, list=FALSE, times=10)

  lapply(1:10, function(x){
    do.a.rep(DT, trime, x, trainIndices[, x])
  }) -> tmp
  rres <- lapply(tmp, function(x) x$rres) %>% rbindlist
  cres <- lapply(tmp, function(x) x$cres) %>% rbindlist

  return(list(rres=rres, cres=cres))
}




0:8 %>%
  lapply(do.it) -> tmp

model_assessment   <- lapply(tmp, function(x) x$rres) %>% rbindlist
model_coefficients <- lapply(tmp, function(x) x$cres) %>% rbindlist

model_assessment %>% fwrite("./results/lasso-model-assessments.csv")
model_coefficients %>% fwrite("./results/lasso-model-coefficients.csv")

