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


# --------------------------------------------------------------- #

dt_apply_fun_to_cols_in_place <- function(DT, fun, cols){
  DT[, (cols):=lapply(.SD, fun), .SDcols=cols]
}

dt_apply_fun_to_col_pattern_in_place <- function(DT, fun, regex){
  tmp <- str_subset(names(DT), regex)
  DT[, (tmp):=lapply(.SD, fun), .SDcols=tmp]
}

dt_cols_matching_predicate <- function(DT, predicate){
  DT %>% lapply(predicate) -> tmp
  tmp[tmp==TRUE] %>% names
}

dt_apply_fun_to_cols_matching_predicate_in_place <- function(DT, fun, predicate){
  dt_apply_fun_to_cols_in_place(DT, fun,
                                cols=dt_cols_matching_predicate(DT, predicate))
}

# --------------------------------------------------------------- #




dat <- fread("./target/fe-wideform.csv")



dat[, deptarget:=Depression>0]

dat %>% dt_counts_and_percents("deptarget")
# 27% have depression target


dat[, uniqueN(MyLua_OBEpisode_ID)]
dat[, .N]

# NOTE: so this treats every OB episode as independent (bad)

dat %>% dt_del_cols("MyLua_Index_PatientID",
                    # "AntidepressantMedication",
                    "Depression"
                    # "MyLua_OBEpisode_ID" ## ??
                    )

setcolorder(dat, "deptarget")


make_bool     <- function(x) ifelse(x>0, 1, x)
make_logical  <- function(x) ifelse(x>0, TRUE, FALSE)


dat <- dat[complete.cases(dat), ]
dat1 <- copy(dat)
dat2 <- copy(dat)
dat3 <- copy(dat)

dt_apply_fun_to_col_pattern_in_place(dat1, make_bool, "^[A-Z]")
dt_apply_fun_to_col_pattern_in_place(dat2, make_logical, "^[A-Z]")




library(glmnet)
library(caret)
library(rfUtilities)
library(randomForest)








# --------------------------------------------------------------- #
#> random forest

this <- copy(dat1)

# make race a factor
this[, race:=as.factor(race)]
this[, deptarget:=as.factor(deptarget)]

forest <- randomForest(deptarget ~ ., data=this)

preds <- predict(forest, newdata=this)

accuracy(preds, this[, deptarget])

# 79%

# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
#> random forest

this <- copy(dat2)

# make race a factor
this[, race:=as.factor(race)]
this[, deptarget:=as.factor(deptarget)]

forest <- randomForest(deptarget ~ ., data=this)

preds <- predict(forest, newdata=this)

accuracy(preds, this[, deptarget])

# 79%

# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
#> random forest

this <- copy(dat3)

# make race a factor
this[, race:=as.factor(race)]
this[, deptarget:=as.factor(deptarget)]

forest <- randomForest(deptarget ~ ., data=this)

preds <- predict(forest, newdata=this)

accuracy(preds, this[, deptarget])

# 94%

# --------------------------------------------------------------- #










this <- copy(dat)

# make race a factor
dat[, race:=as.factor(race)]
dat[, deptarget:=as.factor(deptarget)]

forest <- randomForest(deptarget ~ ., data=dat)

preds <- predict(forest, newdata=dat)

accuracy(preds, dat[, deptarget])














make_rf_imp_table <- function(afit){
  co <- importance(afit, type=1)
  tmp <- row.names(co)
  DT <- data.table(feature=tmp, coe=co)
  setnames(DT, c("feature", "coe"))
  setorder(DT, -coe)
  DT[]
}

make_rf_imp_table(forest)

fo <- copy(dat2)
dat[, race:=as.factor(race)]
fo[, deptarget:=as.factor(fifelse(deptarget==1, TRUE, FALSE))]


forest <- randomForest(deptarget ~ ., data=fo,
                       ntree=500, importance=TRUE)
                       # ntree=5000, importance=TRUE)

preds <- predict(forest, newdata=dat)

accuracy(preds, dat[, deptarget])




make_a_forest <- function(){
  set.seed(1)

  tmp <- sample.split(fo[,DepTarget], SplitRatio = .8)
  train <- subset(fo, tmp==TRUE)
  test  <- subset(fo, tmp==FALSE)

  forest <- randomForest(DepTarget ~ ., data=train[, -1],
                         ntree=5000, importance=TRUE)

  preds <- predict(forest, newdata=test)
  ac <- accuracy(preds, test[, DepTarget])$PCC
  nfeat <- ncol(train[, -1])

  co <- make_rf_imp_table(forest)
  data.table(num_features=nfeat, accuracy=ac, next_to_rm=co[.N, feature],
             included=paste(co[, feature], collapse=","))[]
}


make_a_forest()

run_sims <- function(){
  res <- make_a_forest()
  ena <- copy(res)
  rm_this <- res[.N, next_to_rm]
  nfeat <- res[.N, num_features]
  while(nfeat>3){
    message("On num features: ", nfeat)
    dt_del_cols(fo, rm_this)
    res <- make_a_forest()
    ena <- rbind(ena, res)
    rm_this <- res[.N, next_to_rm]
    nfeat <- res[.N, num_features]
  }
  ena[]
}

res <- run_sims()

fres <- copy(res)

ggplot(res, aes(x=num_features, y=accuracy)) +
  geom_line()


## ABSOLUTE VALUE??


