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

myscale <- function(x){ (x-mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE) }




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


make_bool <- function(x) ifelse(x>0, 1, x)


dat1 <- copy(dat)
dat1 <- dat1[complete.cases(dat1), ]
dat2 <- copy(dat)
dat2 <- dat2[complete.cases(dat2), ]

dt_apply_fun_to_col_pattern_in_place(dat1, make_bool, "^[A-Z]")




library(glmnet)
library(caret)
library(rfUtilities)
library(randomForest)





dat <- copy(dat2)

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


