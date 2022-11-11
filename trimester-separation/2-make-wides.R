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

setwd(here("trimester-separation"))

args <- commandArgs(trailingOnly=TRUE)
trime <- as.integer(args[1])


enrich <- fread("../target/fe-wideform.csv")
enrich %>% dt_keep_cols(c("MyLua_Index_PatientID", "Age", "race",
                          "marital_status", "hisp_latino_p",
                          "Depression", "PHQ_Dep", "EPDS_Dep"))
setkey(enrich, "MyLua_Index_PatientID")



longform <- fread("../target/fe-longform.csv")

# renaming
longform[vsacname=="Emergency Visits", vsacname:="EmergencyVisits"]

priors <- fread("../target/fe-longform.csv",
                select=c("MyLua_Index_PatientID", "FirstDepressionInd",
                         "FirstAbortedInd"))
priors <- priors[!duplicated(MyLua_Index_PatientID)]


### FILTER HERE?!

longform <- longform[Trimester<=trime]


enrich[is.na(PHQ_Dep), PHQ_Dep:=FALSE]
enrich[is.na(EPDS_Dep), EPDS_Dep:=FALSE]

## columns to limit to trimesters 0-3
dcast(longform[Trimester<=3, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part1

# TODO: use "MOOD", too?

KEEPERS <- c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
             "AntidepressantMedication", "UncomplicatedBirth", "Anxiety",
             "Hypertension", "Autoimmune", "Depression", "BetaBlockers",
             "Vomiting", "CesareanBirth", "Migraine", "Cervix-Infection",
             "Preeclampsia", "Pharyngitis", "Sleep",
             "Complication-FetalStress", "Diarrhea", "Mood",
             "Complication-BloodOxygen", "Complication-AlcoholUse",
             "Complication-IllicitDrugUse", "Hypothyroidism", "Inflammation",
             "PregnancyOverseeing-Age35plus",
             "Pregnancyorotherrelateddiagnoses", "Complication-Obesity",
             "Complication-Smoking", "GestationalDiabetes", "Hemoglobin",
             "Complication-HxHypertension", "HighRiskPregnancy",
             "MentalDisorders", "Obesity", "Smoking", "MentalDisorder",
             "ThreatenedAbortion", "ThreatenedMiscarriage",
             # 2022-09-04
             "MaternalCare", "PrematureLabor", "EmergencyVisits",
             "AbdominalPain", "FalseLabor", "PlacentaPlacement",
             "Screening", "MoreThanOneFetusPerUterus", "Placenta", #!!!!
             "Hemorrage", "PregnancyOverseeing-HistoryPretermLabor",
             "PregnancyOverseeing-FollowingNeglect", "PretermLabor", #!!!!
             "UrinaryTract-Infection", "HeartIrregularities",
             "Smoke")
             ## TODO: add more (but which ones?!)


part1 %>% dt_keep_cols(KEEPERS)


part1[, Obesity:=`Complication-Obesity`+Obesity]
part1 %>% dt_del_cols("Complication-Obesity")

part1[, Smoking:=`Complication-Smoking`+Smoking]
part1 %>% dt_del_cols("Complication-Smoking")

part1[, MentalDisorders:=MentalDisorder+MentalDisorders]
part1 %>% dt_del_cols("MentalDisorder")

part1[, Hypertension:=Hypertension+`Complication-HxHypertension`]
part1 %>% dt_del_cols("Complication-HxHypertension")
part1[, PrenatalDepressionInd:=AntidepressantMedication+Depression]
part1[, Depression:=NULL]
part1[, AntidepressantMedication:=NULL]




if(trime>=4){
  ## columns to limit to trimester 4 and after
  dcast(longform[Trimester>=4, ],
        MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
        value.var="code",
        fun.aggregate=uniqueN) -> part2
  part2 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                           "Sleep"))
  setnames(part2, "Sleep", "PostnatalSleep")
  part1 %>% merge(part2) -> parts
} else {
  parts <- copy(part1)
}


##!! TODO: make sure you don't lose records !!@@@@@@@@@@@@@@@@@@@@@@@
## putting together pieces for the wide form
enrich %>%
  merge(parts) -> wideform

wideform %<>% merge(priors, by="MyLua_Index_PatientID")


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


fwrite(wideform, sprintf("./wides/t%d.csv", trime))

# --------------------------------------------------------------- #

