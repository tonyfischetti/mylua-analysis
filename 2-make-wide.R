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


### now wide form

longform <- fread("./target/fe-longform.csv")

## first get the ages
longform[, .(Age=min(Age),
             race=race[1],
             marital_status=marital_status[1],
             hisp_latino_p=hisp_latino_p[1],
             PHQ_Dep=PHQ[1]>=5,
             EPDS_Dep=EPDS[1]>=14,
             FirstAbortedInd=FirstAbortedInd[1],
             FirstDepressionInd=FirstDepressionInd[1]),
  .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] -> others
setkey(others, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")
others[is.na(PHQ_Dep), PHQ_Dep:=FALSE]
others[is.na(EPDS_Dep), EPDS_Dep:=FALSE]

## columns to limit to trimesters 0-3
dcast(longform[Trimester<=3, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part1

# TODO: use "MOOD", too?

part1 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AntidepressantMedication",
                         "UncomplicatedBirth",
                         "Anxiety",
                         "Hypertension",
                         "Autoimmune",
                         "Depression",
                         "BetaBlockers",
                         "Vomiting",
                         "CesareanBirth",
                         "Migraine",
                         "Cervix-Infection",
                         "Preeclampsia",
                         "Pharyngitis",
                         "Sleep",
                         "Complication-FetalStress",
                         "Diarrhea",
                         "Mood",
                         "Complication-BloodOxygen",
                         "Complication-AlcoholUse",
                         "Complication-IllicitDrugUse",
                         "Hypothyroidism",
                         "Inflammation",
                         "PregnancyOverseeing-Age35plus",
                         "Pregnancyorotherrelateddiagnoses",
                         "Complication-Obesity",
                         "Complication-Smoking",
                         "GestationalDiabetes",
                         "Hemoglobin",
                         "Complication-HxHypertension",
                         "HighRiskPregnancy",
                         "MentalDisorders",
                         "Obesity",
                         "Smoking",
                         "MentalDisorder",
                         "ThreatenedAbortion",
                         "ThreatenedMiscarriage"
                         ))


part1[, Obesity:=`Complication-Obesity`+Obesity]
part1 %>% dt_del_cols("Complication-Obesity")

part1[, Smoking:=`Complication-Smoking`+Smoking]
part1 %>% dt_del_cols("Complication-Smoking")

part1[, MentalDisorders:=MentalDisorder+MentalDisorders]
part1 %>% dt_del_cols("MentalDisorder")


# TODO: more

part1[, PrenatalDepressionInd:=AntidepressantMedication+Depression]
part1[, Depression:=NULL]
part1[, AntidepressantMedication:=NULL]




## columns to limit to trimester 4 and after
dcast(longform[Trimester>=4, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part2
part2 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                         "Sleep",
                         "Depression"))
setnames(part2, "Sleep", "PostnatalSleep")

part2 %<>% merge(others[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID, PHQ_Dep, EPDS_Dep)])

part2[, .N, Depression>0]
#    Depression     N
#        <lgcl> <int>
# 1:      FALSE  4644
# 2:       TRUE   477
part2[, Depression:=Depression+PHQ_Dep+EPDS_Dep]
part2[, .N, Depression>0]
#    Depression     N
#        <lgcl> <int>
# 1:      FALSE  4545
# 2:       TRUE   576

part2 %>% dt_del_cols("PHQ_Dep", "EPDS_Dep")



##!! TODO: make sure you don't lose records !!@@@@@@@@@@@@@@@@@@@@@@@
## putting together pieces for the wide form
others %>%
  merge(part1) %>%
  merge(part2) -> wideform


setcolorder(wideform, c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID"))

wideform
wideform[, PriorDepressionInd:=FALSE]
wideform[, PriorAbortiveOutcome:=FALSE]
wideform[FirstDepressionInd<MyLua_OBEpisode_ID, PriorDepressionInd:=TRUE]
wideform[FirstAbortedInd<MyLua_OBEpisode_ID, PriorAbortiveOutcome:=TRUE]

wideform %>% names
wideform %>% dt_del_cols("FirstDepressionInd", "FirstAbortedInd")


## now some derived features (that might help)
wideform[, NonMinority:=FALSE]
wideform[race=="white" & hisp_latino_p==FALSE, NonMinority:=TRUE]

# wideform %>% dt_del_cols("PHQ_Dep", "EPDS_Dep")

fwrite(wideform, "target/fe-wideform.csv")

# --------------------------------------------------------------- #
