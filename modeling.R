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
# 26% have depression target


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

## but random forest works best with dat3 (the "magnitudes" included)



library(glmnet)
library(caret)
library(rfUtilities)
library(randomForest)




this <- copy(dat3)







# --------------------------------------------------------------- #
#> random forest

this <- copy(dat3)

# make race a factor
this[, race:=as.factor(race)]
this[, deptarget:=as.factor(deptarget)]

this[, PriorDepressionInd:=as.factor(PriorDepressionInd)]
this[, PriorAbortiveOutcome:=as.factor(PriorAbortiveOutcome)]
this[, NonMinority:=as.factor(NonMinority)]

forest <- randomForest(deptarget ~ ., data=this)

preds <- predict(forest, newdata=this)

accuracy(preds, this[, deptarget])

forest

# 94%

# --------------------------------------------------------------- #


perform <- function(aseed){
  set.seed(aseed)
  trainIndex <- createDataPartition(this$deptarget, p = .8,
                                    list = FALSE, 
                                    times = 1)
  trainD <- this[trainIndex,]
  testD <- this[-trainIndex,]
  forest <- randomForest(deptarget ~ ., data=trainD)
  preds <- predict(forest, newdata=testD)
  return(data.table(theseed=aseed, pcc=accuracy(preds, testD[, deptarget])$PCC))
}

1:100 %>% lapply(perform) %>% rbindlist -> dark
dark[order(-pcc)]
# 9 (and .9) is 76%






this <- copy(dat3)

# make race a factor
this[, race:=as.factor(race)]
this[, deptarget:=as.factor(ifelse(deptarget, "yes", "no"))]
# this[, deptarget:=as.factor(deptarget)]

this[, PriorDepressionInd:=as.factor(PriorDepressionInd)]
this[, PriorAbortiveOutcome:=as.factor(PriorAbortiveOutcome)]
this[, NonMinority:=as.factor(NonMinority)]

control <- trainControl(
  method = 'repeatedcv',
  number = 30,
  repeats = 1,
)


mtry <- sqrt(ncol(this)-1)
tunegrid <- expand.grid(.mtry=mtry)

train(deptarget ~ ., data=this,
      method='rf',
      tuneGrid=tunegrid,
      trControl=control)

# Random Forest
#
# 1272 samples
#   19 predictor
#    2 classes: 'no', 'yes'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times)
# Summary of sample sizes: 1146, 1146, 1145, 1144, 1144, 1145, ...
# Resampling results:
#
#   Accuracy  Kappa
#   0.721722  0.07101505
#
# Tuning parameter 'mtry' was held constant at a value of 4.358899







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












# --------------------------------------------------------------- #
#> ridge, no pre-processing

X <- model.matrix(deptarget ~ ., data=dat3)[, -1]
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

# 73%

# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
#> lasso, no pre-processing

X <- model.matrix(deptarget ~ ., data=dat3)[, -1]
y <- as.matrix(dat[, 1]+0)

fit <- glmnet(X, y, family="binomial")
plot(fit)

cvfit <- cv.glmnet(X, y, alpha=1, standardize=FALSE, family="binomial")
plot(cvfit)

preds <- predict(cvfit, newx=X, s="lambda.min", type="response")
preds <- as.matrix(fifelse(preds>.5, 1, 0))

confusionMatrix(factor(y, levels=c(1, 0)), factor(preds, levels=c(1, 0)))

preds <- predict(cvfit, newx=X, s="lambda.min", type="response")
preds <- fifelse(preds>.5, 1, 0)
accuracy(preds, y)

# 73%

# --------------------------------------------------------------- #




this <- copy(dat3)
#
# # make race a factor
# this[, race:=as.factor(race)]
# this[, deptarget:=as.factor(ifelse(deptarget, "yes", "no"))]
# # this[, deptarget:=as.factor(deptarget)]
#
# this[, PriorDepressionInd:=as.factor(PriorDepressionInd)]
# this[, PriorAbortiveOutcome:=as.factor(PriorAbortiveOutcome)]
# this[, NonMinority:=as.factor(NonMinority)]

perform <- function(aseed){
  set.seed(aseed)
  trainIndex <- createDataPartition(this$deptarget, p = .8,
                                    list = FALSE, 
                                    times = 1)
  trainD <- this[trainIndex,]
  testD <- this[-trainIndex,]
  forest <- randomForest(deptarget ~ ., data=trainD)
  preds <- predict(forest, newdata=testD)
  return(data.table(theseed=aseed, pcc=accuracy(preds, testD[, deptarget])$PCC))
}

1:100 %>% lapply(perform) %>% rbindlist -> dark
dark[order(-pcc)]
# 9 (and .9) is 76%


model <- glm(deptarget ~ ., family=binomial(link='logit'), data=this)
model
summary(model)
anova(model, test="Chisq")

preds <- predict(model, newdata=this, type='response')
preds <- ifelse(preds > 0.5, TRUE, FALSE)
accuracy(preds, this[, deptarget])$PCC



perform <- function(DT, aseed){
  set.seed(aseed)
  trainIndex <- createDataPartition(DT$deptarget, p = .8,
                                    list = FALSE, 
                                    times = 1)
  trainD <- DT[trainIndex,]
  testD <- DT[-trainIndex,]
  model <- glm(deptarget ~ ., family=binomial(link='logit'), data=trainD)
  preds <- predict(model, newdata=testD, type='response')
  preds <- ifelse(preds > 0.5, TRUE, FALSE)
  return(accuracy(preds, testD[, deptarget])$PCC)
}


perform(this[, .(deptarget, PrenatalDepressionInd)], 1)
perform(this[, .(deptarget,
                 PrenatalDepressionInd,
                 Age
                 )], 1)
perform(this[, .(deptarget,
                 PrenatalDepressionInd,
                 Age,
                 Anxiety
                 )], 1)



PrenatalDepressionInd  1  23.1246      1105     1245.6 0.000001518 ***
Age                    1   9.7803      1116     1293.3    0.001764 **
Anxiety                1   8.8111      1115     1284.5    0.002994 **
BetaBlockers           1   9.7830      1114     1274.7    0.001761 **
Hypertension           1   4.2418      1111     1270.3    0.039441 *
PostnatalSleep         1   4.0499      1104     1241.6    0.044175 *
PriorAbortiveOutcome   1   5.0293      1092     1221.7    0.024922 *
NonMinority            1   4.5133      1091     1217.2    0.033633 *

                      Df Deviance Resid. Df Resid. Dev    Pr(>Chi)
NULL                                   1118     1303.1
MyLua_OBEpisode_ID     1   0.0005      1117     1303.1    0.981448
CesareanBirth          1   0.1542      1113     1274.5    0.694588
Diarrhea               1   0.0431      1112     1274.5    0.835572
Hypothyroidism         0   0.0000      1111     1270.3
Migraine               1   0.0772      1110     1270.2    0.781194
Mood                   1   0.2794      1109     1269.9    0.597127
Pharyngitis            1   0.3075      1108     1269.6    0.579194
Preeclampsia           1   0.8275      1107     1268.8    0.362996
Sleep                  1   0.0070      1106     1268.8    0.933112
marital_status         5   9.1138      1099     1232.5    0.104609
race                   4   5.6845      1095     1226.8    0.223980
hisp_latino_p          1   0.0207      1094     1226.8    0.885687
PriorDepressionInd     1   0.0762      1093     1226.7    0.782553

