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

# --------------------------------------------------------------- #

args <- commandArgs(trailingOnly=TRUE)
trime <- as.integer(args[1])

# --------------------------------------------------------------- #

trime <- 4


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




# --------------------------------------------------------------- #

this <- copy(dat)

this[, race:=as.factor(race)]
this[, deptarget:=as.factor(deptarget)]

this[, PriorDepressionInd:=as.factor(PriorDepressionInd)]
this[, PriorAbortiveOutcome:=as.factor(PriorAbortiveOutcome)]
this[, NonMinority:=as.factor(NonMinority)]

trainIndex <- createDataPartition(dat$deptarget, p = .8,
                                  list = FALSE,
                                  times = 1)

train <- this[trainIndex,]
test  <- this[-trainIndex,]



# make race a factor
forest <- randomForest(deptarget ~ ., data=train)

preds <- predict(forest, newdata=test)

accuracy(preds, test[, deptarget])$f.score

