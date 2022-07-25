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

set.seed(2)



dat <- fread("./target/fe-wideform.csv")

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

dense <- as.data.table(as.matrix(model.matrix(deptarget ~ ., data=dat)[,-1]))


# [15] "Anxiety"                  "BetaBlockers"
# [17] "CesareanBirth"            "ComplicationBloodOxygen"
# [19] "ComplicationFetalStress"  "Diarrhea"
# [21] "Hypertension"             "Hypothyroidism"
# [23] "Migraine"                 "Mood"
# [25] "Pharyngitis"              "Preeclampsia"
# [27] "Sleep"                    "UncomplicatedBirth"
# [29] "Vomiting"                 "PrenatalDepressionInd"
# [31] "PostnatalSleep"           "PriorDepressionIndTRUE"
# [33] "PriorAbortiveOutcomeTRUE" "NonMinorityTRUE"

dense$Anxiety %>% hist # maybe 2?
dense$BetaBlockers %>% hist # maybe 2?
dense$Hypertension %>% hist # maybe 2?
dense$Hypothyroidism %>% hist # maybe 1?
dense$Migraine %>% hist # maybe 2?
dense$Sleep %>% hist # maybe 2?
dense$Vomiting %>% hist # maybe 2?
dense$PostnatalSleep %>% hist # maybe 2?

dense[, Anxiety:=ifelse(Anxiety>=2, 1, 0)]
dense[, BetaBlockers:=ifelse(BetaBlockers>=2, 1, 0)]
dense[, Hypertension:=ifelse(Hypertension>=2, 1, 0)]
dense[, Migraine:=ifelse(Migraine>=2, 1, 0)]
dense[, Sleep:=ifelse(Sleep>=2, 1, 0)]
dense[, Vomiting:=ifelse(Vomiting>=2, 1, 0)]
dense[, PostnatalSleep:=ifelse(PostnatalSleep>=2, 1, 0)]







dense <- as.matrix(model.matrix(deptarget ~ ., data=dat)[,-1])







xformed <- as.matrix(dense)

# xformed[xformed>1] <- 1
# xformed[, 1] <- dense[, 1]
# xformed[, 2] <- dense[, 2]
# xformed[, "Age"]


trainIndex <- createDataPartition(dat$deptarget, p = .8,
                                  list = FALSE,
                                  times = 1)


trainX <- xformed[trainIndex,]
trainY <- dat[trainIndex,]$deptarget+0
testX  <- xformed[-trainIndex,]
testY  <- dat[-trainIndex,]$deptarget+0








bstDense <- xgboost(data = trainX,
                    label = trainY,
                    max.depth = 3,
                    eta = 1,
                    nthread = 2,
                    nrounds = 20,
                    objective = "binary:logistic",
                    verbose=2)

pred <- predict(bstDense, testX)
prediction <- as.numeric(pred > 0.5)
err <- round(1-mean(as.numeric(pred > 0.5) != testY), 2)
err
print(data.table(trimester=trime, acc=err))

# importance_matrix <- xgb.importance(model = bstDense)
# print(importance_matrix)
# xgb.plot.importance(importance_matrix = importance_matrix)

# 88%
