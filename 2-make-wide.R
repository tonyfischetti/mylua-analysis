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
             FirstAbortedInd=FirstAbortedInd[1],
             FirstDepressionInd=FirstDepressionInd[1]),
  .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] -> others
setkey(others, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")


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
                         "Depression",
                         "BetaBlockers",
                         "Vomiting",
                         "CesareanBirth",
                         "Migraine",
                         "Preeclampsia",
                         "Pharyngitis",
                         "Sleep",
                         "Complication-FetalStress",
                         "Diarrhea",
                         "Mood",
                         "Complication-BloodOxygen",
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
                         "MentalDisorders",  # !!!!
                         "Obesity",
                         "MentalDisorder",   # !!!!
                         "ThreatenedAbortion",
                         "ThreatenedMiscarriage"
                         ))

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


fwrite(wideform, "target/fe-wideform.csv")

# --------------------------------------------------------------- #
