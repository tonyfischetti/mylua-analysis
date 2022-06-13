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


dat <- fread("./target/model-matrix.csv")



dat[, deptarget:=(AntidepressantMedication+Depression)>0]

dat %>% dt_counts_and_percents("deptarget")
# 15% have depression target


dat[, uniqueN(MyLua_OBEpisode_ID)]
dat[, .N]

# so this treats every OB episode as independent

dat %>% dt_del_cols("MyLua_Index_PatientID",
                    "AntidepressantMedication",
                    "Depression",
                    "MyLua_OBEpisode_ID" ## ??
                    )

setcolorder(dat, "deptarget")

# CesareanBirth
dat[, CesareanBirth:=ifelse(CesareanBirth>0, 1, CesareanBirth)]

# Diarrhea
dat[, Diarrhea:=Diarrhea]

# Hypertension
dat[, Hypertension:=ifelse(Hypertension>0, 1, Hypertension)]

# Migraine
dat[, Migraine:=ifelse(Migraine>0, 1, Migraine)]

# Pharyngitis
dat[, Pharyngitis:=ifelse(Pharyngitis>0, 1, Pharyngitis)]

# Preeclampsia
dat[, Preeclampsia:=ifelse(Preeclampsia>0, 1, Preeclampsia)]

# SleepDisorders
dat[, SleepDisorders:=ifelse(SleepDisorders>0, 1, SleepDisorders)]

# Vomiting
dat[, Vomiting:=ifelse(Vomiting>0, 1, Vomiting)]





library(glmnet)
library(caret)
library(rfUtilities)
library(randomForest)





dat <- dat[complete.cases(dat), ]

# --------------------------------------------------------------- #
#> ridge, no pre-processing

X <- model.matrix(deptarget ~ ., data=dat)[, -1]
y <- as.matrix(dat[, 1]+0)

fit <- glmnet(X, y, family="binomial")
plot(fit)

cvfit <- cv.glmnet(X, y, alpha=0, standardize=FALSE, family="binomial")
plot(cvfit)

preds <- predict(cvfit, newx=X, s="lambda.min", type="response")
preds <- as.matrix(fifelse(preds>.5, 1, 0))

confusionMatrix(factor(y, levels=c(1, 0)), factor(preds, levels=c(1, 0)))

preds <- predict(cvfit, newx=X, s="lambda.min", type="response")
preds <- fifelse(preds>.5, 1, 0)
accuracy(preds, y)

# 85% accuracy

# --------------------------------------------------------------- #

# --------------------------------------------------------------- #
#> lasso, no pre-processing

# same results as ridge

# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
#> random forest

bkDat <- copy(dat)
dat <- copy(bkDat)

# make race a factor
dat[, race:=as.factor(race)]
dat[, deptarget:=as.factor(deptarget)]

forest <- randomForest(deptarget ~ ., data=dat)

preds <- predict(forest, newdata=dat)

accuracy(preds, dat[, deptarget])

# 85%

# --------------------------------------------------------------- #




