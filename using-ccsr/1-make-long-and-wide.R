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
library(epitools)

library(here)

setwd(here("using-ccsr"))




# --------------------------------------------------------------- #

ccsr <- fread("../data/dx-ccsr.csv", na.strings=c("", "NULL", "' '"))
setnames(ccsr, str_replace_all(names(ccsr), "'", ""))
dt_set_clean_names(ccsr)

ccsr <- ccsr[, .(ICD10CM=icd_10_cm_code,
                 ccsr_cat_1_ip=default_ccsr_category_description_ip,
                 ccsr_cat_1_op=default_ccsr_category_description_op,
                 ccsr_cat_1=ccsr_category_1_description,
                 ccsr_cat_2=ccsr_category_2_description,
                 ccsr_cat_3=ccsr_category_3_description,
                 ccsr_cat_4=ccsr_category_4_description,
                 ccsr_cat_5=ccsr_category_5_description,
                 ccsr_cat_6=ccsr_category_6_description)]

ccsr[, ICD10CM:=str_replace_all(ICD10CM, "'", "")]
ccsr %<>% melt(id.var="ICD10CM")
setorder(ccsr, "ICD10CM")
ccsr <- ccsr[value!=" "][, .(ICD10CM, CCSR_cat=value)]
ccsr <- unique(ccsr)



trimesters <- fread("../data/trimester.csv", na.strings=c("", "NULL"))

# --------------------------------------------------------------- #


trimesters[, .(MyLua_Index_PatientID=MyLUA_Index_PatientID,
               MyLua_OBEpisode_ID=as.integer(str_replace(MyLua_OBEpisode_ID, "^\\d+\\-", "")),
               Trimester,
               ICD10CM=DX_Codes)] -> trimesters

trimesters[!is.na(ICD10CM)] -> trimesters


trimesters %<>%
  # make long (not wide)
  melt(id.vars=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester"),
       variable.name="codesystem",
       value.name="code") %>%
  # separate the array into rows
  separate_rows(code, sep="; ") %>%
  # separate_rows(code, sep="; ?") %>%
  filter(!is.na(code))

setDT(trimesters)

trimesters <- trimesters[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID,
                             Trimester, ICD10CM=code)]


setkey(trimesters, "ICD10CM")
setkey(ccsr, "ICD10CM")





trimesters %>% merge(ccsr, allow.cartesian=TRUE) -> comb


comb[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID,
         Trimester, ICD10CM, CCSR_cat)] -> comb
comb <- unique(comb)


## SHOULD I INCLUDE ALL???


# NOTE: let's try incidents 500 and above

# comb %>% dt_counts_and_percents("CCSR_cat") -> tmp
# keepers <- tmp[count>=500 &
#                CCSR_cat!="TOTAL" &
#                !str_detect(CCSR_cat, "Unacceptable"),
#              CCSR_cat]
#
# comb <- comb[CCSR_cat %chin% keepers, ]
# comb[, CCSR_cat:=str_trim(str_sub(CCSR_cat, 1, 40))]


comb
longform <- copy(comb)

setkey(longform, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")
longform %>% fwrite("./target/ccsr-long.csv")


dcast(longform[Trimester<=3, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ CCSR_cat,
      value.var="ICD10CM",
      fun.aggregate=uniqueN) -> part1

# TODO: hardcoded
part1[,3:64] %>% colSums -> tmp
tmp[tmp>0] %>% length
tmp[tmp>50] %>% names -> KEEPERS

KEEPERS <- c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", KEEPERS)

part1 %>% fwrite("./target/ccsr-wide-all-features.csv")
part1[, .SD, .SDcols=KEEPERS] -> part1
part1 %>% fwrite("./target/ccsr-wide.csv")


toenrich <- fread("../trimester-separation/wides/t3.csv")
toenrich %>% names

toenrich[, 1:9] -> toenrich
setkey(toenrich, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")

toenrich %>% merge(part1) -> comb

comb %>% fwrite("./target/ccsr-wide-enriched.csv")

# --------------------------------------------------------------- #

# build a model real quick just to C


DT <- copy(comb)

DT %>% dt_del_cols("PostnatalSleep", "marital_status", "hisp_latino_p",
                   "race", "PregnancyOverseeing-Age35plus", "NonMinority")

dt_set_clean_names(DT, lower=FALSE)
DT[, deptarget:=Depression>0]

DT %>% dt_counts_and_percents("deptarget")

patient_ids <- DT[, MyLua_Index_PatientID]
patient_episode <- DT[, MyLua_OBEpisode_ID]
DT %>% dt_del_cols("MyLua_Index_PatientID", "Depression")

setcolorder(DT, "deptarget")

# TODO: COMPLETE CASES SHOULD BE SOONER!!!!!
# dat <- dat[complete.cases(dat), ]
# patient_ids <- patient_ids[complete.cases(dat)]
# patient_episode <- patient_episode[complete.cases(dat)]

library(caret)
trainIndices <- createDataPartition(DT$deptarget, p=.8, list=FALSE, times=1)
trainIndex <- trainIndices[, 1]


  X <- model.matrix(deptarget ~ ., data=DT)[, -1]
  Y <- as.matrix(DT[, 1]+0)
  trainX <- X[trainIndex,]
  trainY <- Y[trainIndex,]
  testX  <- X[-trainIndex,]
  testY  <- Y[-trainIndex,]

  library(glmnet)
  cvfit <- cv.glmnet(trainX, trainY, alpha=1, standardize=TRUE, family="binomial")

plot(cvfit)


  preds_resp <- predict(cvfit, newx=testX, s="lambda.1se", type="response")
  preds_resp <- predict(cvfit, newx=testX, s="lambda.min", type="response")
  preds <- fifelse(preds_resp>=0.5, 1, 0)

  library(rfUtilities)
  obj <- accuracy(preds, testY)
  obj

  coef(cvfit, s="lambda.min") -> tmp
  tmp <- as.data.table(as.matrix(tmp), keep.rownames=TRUE)
  tmp <- tmp[-1,]
  setnames(tmp, c("feature", "coefficient"))
  tmp[coefficient==0, coefficient:=NA]
  tmp

# not so great (lambda.min) only uses PHQ and EPDS



