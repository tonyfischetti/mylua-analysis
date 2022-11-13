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



# trime <- 3
# trial <- 1
# trainIndex <- trainIndices[, 1]



do.a.rep <- function(DT, trime, trial, trainIndex){
  X <- model.matrix(deptarget ~ ., data=DT)[, -1]
  Y <- as.matrix(DT[, 1]+0)
  trainX <- X[trainIndex,]
  trainY <- Y[trainIndex,]
  testX  <- X[-trainIndex,]
  testY  <- Y[-trainIndex,]

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
  preds <- fifelse(preds_resp>=0.5, 1, 0)

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
  return(list(rres=rres, cres=cres, prediction_response=preds_resp,
              actual=testY, the_model=cvfit))
}



make.confusion.row <- function(prediction_response, actual, cutoff) {
  preds <- fifelse(prediction_response>=cutoff, 1, 0)
  cm <- caret::confusionMatrix(factor(preds), reference=factor(actual))
  cm <- cm$table %>% as.data.table
  cm <- cm[, .(prob_cutoff=cutoff, predicted=Prediction, actual=Reference, N)]
  cm[predicted==0 & actual==0, type:="true_negatives"]
  cm[predicted==0 & actual==1, type:="false_negatives"]
  cm[predicted==1 & actual==0, type:="false_positives"]
  cm[predicted==1 & actual==1, type:="true_positives"]
  cm %>% dcast(prob_cutoff ~ type, value.var="N") -> ret
  ret[, N:=length(actual)]
  ret[, FPR:=false_positives/N]
  ret[, TPR:=1-FPR]
  ret[]
}





do.it <- function(trime){

  dat <- fread(sprintf("./wides/t%d.csv", trime))

  # added 2022-09-04
  dat %>% dt_del_cols("PostnatalSleep", "marital_status", "hisp_latino_p",
                      "race", "PregnancyOverseeing-Age35plus", "NonMinority")

  dt_set_clean_names(dat, lower=FALSE)
  dat[, deptarget:=Depression>0]

  dat %>% dt_counts_and_percents("deptarget")

  setcolorder(dat, "deptarget")

  # TODO: COMPLETE CASES SHOULD BE SOONER!!!!!
  dat <- dat[complete.cases(dat), ]
  patient_ids <- dat[, MyLua_Index_PatientID]
  patient_episode <- dat[, MyLua_OBEpisode_ID]
  bkdat <- copy(dat)
  dat %>% dt_del_cols("MyLua_Index_PatientID", "Depression")

  DT <- copy(dat)
  for_validation <- sample.int(DT[, .N], DT[, .N]*.1)
  DT <- DT[!for_validation]
  patient_ids <- dat[!for_validation]
  patient_episode <- dat[!for_validation]
  HOLD_OUT <- dat[for_validation]

  ## TODO: use folds instead of bootstrap resampling
  trainIndices <- createDataPartition(DT$deptarget, p=.8, list=FALSE, times=10)

  lapply(1:10, function(x){
    do.a.rep(DT, trime, x, trainIndices[, x])
  }) -> tmp
  rres <- lapply(tmp, function(x) x$rres) %>% rbindlist
  cres <- lapply(tmp, function(x) x$cres) %>% rbindlist
  the_model <- tmp[[1]]$the_model

  ## for validation set
  X <- model.matrix(deptarget ~ ., data=HOLD_OUT)[, -1]
  Y <- as.matrix(HOLD_OUT[, 1]+0)
  preds_resp <- predict(the_model, newx=X, s="lambda.1se", type="response")
  preds <- fifelse(preds_resp>=0.5, 1, 0)
  validation_set_accuracy <- accuracy(preds, Y)$PCC

  # (for getting the scores for everyone)
  X <- model.matrix(deptarget ~ ., data=dat)[, -1]
  Y <- as.matrix(dat[, 1]+0)
  preds_resp <- predict(the_model, newx=X, s="lambda.1se", type="response")
  preds <- fifelse(preds_resp>=0.5, 1, 0)
  obj <- accuracy(preds, Y)

  every_patient <- data.table(trimester=trime,
                              patient_id=bkdat[, MyLua_Index_PatientID],
                              patient_episode=dat[, MyLua_OBEpisode_ID],
                              predicted_prob=preds_resp,
                              predicted_boolean=preds,
                              actual_boolean=ifelse(Y==TRUE, TRUE, FALSE))
  every_patient <- unique(every_patient)

  tmp %>% lapply(function (x) unlist(x$prediction_response)) %>% unlist -> bpr
  tmp %>% lapply(function (x) unlist(x$actual))              %>% unlist -> ba

  lapply(seq(0, 1, by=0.02), function (x) {
    make.confusion.row(bpr, ba, x)
  }) %>% rbindlist -> cms
  cms[, trimester:=trime]
  setcolorder(cms, c("trimester"))

  return(list(rres=rres, cres=cres, cms=cms, every_patient=every_patient,
              validation_set_accuracy=validation_set_accuracy))
}


0:8 %>%
  lapply(do.it) -> tmp


model_assessment        <- lapply(tmp, function(x) x$rres)           %>% rbindlist
model_coefficients      <- lapply(tmp, function(x) x$cres)           %>% rbindlist
model_confusion         <- lapply(tmp, function(x) x$cms)            %>% rbindlist
individual_predictions  <- lapply(tmp, function(x) x$every_patient)  %>% rbindlist
validation_set_accuracy <- lapply(tmp, function(x) x$validation_set_accuracy) %>% unlist
validation_set_accuracy <- data.table(trimester=0:8,
                                      validation_set_accuracy=validation_set_accuracy)


model_assessment   %>% fwrite("./results/lasso-model-assessments.csv")
model_assessment %>% dcast(trimester + trial ~ vari) %>%
  fwrite("./results/lasso-model-assessments-wide.csv")
model_coefficients %>% fwrite("./results/lasso-model-coefficients.csv")
model_confusion    %>% fwrite("./results/lasso-model-confusion.csv")
individual_predictions %>% fwrite("./results/individual-predictions.csv")
validation_set_accuracy %>% fwrite("./results/validation-set-accuracy.csv")


# --------------------------------------------------------------- #



