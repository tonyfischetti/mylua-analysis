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




# --------------------------------------------------------------- #

valueset <- fread("./data/valuesets.csv", na.strings=c("", "NULL")) %>%
  select(vsacname, codesystem, code, description)

valueset

valueset[codesystem=="ICD10CM", code:=str_replace_all(code, "\\.", "")]

trimesters <- fread("./data/trimester.csv", na.strings=c("", "NULL"))


# --------------------------------------------------------------- #

trimesters %<>%
  ### just the necessary columns, for demonstration
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID,
         Age=Age_at_Trimester,
         PHQ=PHQ_Scrs,
         EPDS=EPDS_Scrs) %>%
  select(MyLua_Index_PatientID, MyLua_OBEpisode_ID, Trimester, Age, DX_Codes,
         Meds_RXNORM, CPT_Codes, ObsrvtnData.Obsrvtn_Array,
         PHQ, EPDS) %>%
  # remove the patient ID from the episode ID
  mutate(MyLua_OBEpisode_ID=as.integer(str_replace(MyLua_OBEpisode_ID, "^\\d+-", ""))) %>%
  # rename weird column name
  rename(LOINC=ObsrvtnData.Obsrvtn_Array,     # observations (TODO: describe)
         RXNORM=Meds_RXNORM,                  # medicines
         CPT=CPT_Codes,                       # procedures
         ICD10CM=DX_Codes) %>%                # diagnoses
  # we want to use data.table's implementation of "melt"
  as.data.table

trimesters %<>%
  # make long (not wide)
  melt(id.vars=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester",
                 "Age", "PHQ", "EPDS"),
       variable.name="codesystem",
       value.name="code") %>%
  # separate the array into rows
  separate_rows(code, sep="; ") %>%
  # separate_rows(code, sep="; ?") %>%
  filter(!is.na(code)) %>%
  # extract the LOINC value
  mutate(loincvalue = ifelse(codesystem=="LOINC",
                             str_replace(code, "^.+:(.+)$", "\\1"),
                             NA_character_)) %>%
  # conditionally fix the codes for LOINCs using regular expressions
  mutate(code = case_when(codesystem=="LOINC" ~ str_replace(code, ":.+$", ""),
                          TRUE                ~ code))

setDT(trimesters)

tphq <- trimesters[, PHQ]
tphq <- sapply(sapply(sapply(tphq, function(x) str_split(x, "; ")), as.integer), max)
trimesters[, PHQ:=tphq]

tepds <- trimesters[, EPDS]
tepds <- sapply(sapply(sapply(tepds, function(x) str_split(x, "; ")), as.integer), max)
trimesters[, EPDS:=tepds]


trimesters[codesystem=="ICD10CM"]
trimesters[codesystem=="ICD10CM", code:=str_replace_all(code, "\\.", "")]

trimesters %>%
  # do the join
  inner_join(valueset, by=c("codesystem", "code")) %>%
  # left_join(valueset, by=c("codesystem", "code")) %>%
  # re-order variables
  select(MyLua_Index_PatientID,
         MyLua_OBEpisode_ID,
         Trimester,
         Age,
         PHQ,
         EPDS,
         vsacname,
         codesystem,
         code,
         loincvalue,
         description,
         # everything()
  ) -> longform

trimesters[,.N]
# [1] 1135135
longform[, .N]
## 2022-09-04
# [1] 108268
## 2022-08-03
# [1] 103157
# [1] 67886
# Olivia's additions added 35,271


setDT(longform)

longform


# how much coverage of depression scores?
longform %>% dt_na_breakdown("PHQ")
longform %>% dt_na_breakdown("EPDS")
# > longform %>% dt_na_breakdown("PHQ")
#    PHQ_not_na count percent
#        <char> <int>   <num>
# 1:      FALSE 39640   58.39
# 2:       TRUE 28246   41.61
# 3:      TOTAL 67886  100.00
# > longform %>% dt_na_breakdown("GAD")
#    GAD_not_na count percent
#        <char> <int>   <num>
# 1:      FALSE 67098   98.84
# 2:       TRUE   788    1.16
# 3:      TOTAL 67886  100.00
# > longform %>% dt_na_breakdown("EPDS")
#    EPDS_not_na count percent
#         <char> <int>   <num>
# 1:       FALSE 57208   84.27
# 2:        TRUE 10678   15.73
# 3:       TOTAL 67886  100.00

# ## get rid of for now
# longform[!is.na(PHQ), PHQ]
# longform %>% dt_del_cols("PHQ", "GAD", "EPDS")


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #

# set up for features that indicate prior events

setkey(longform, "MyLua_Index_PatientID")

setorder(longform, MyLua_Index_PatientID, MyLua_OBEpisode_ID)

longform[vsacname %chin% c("Depression", "AntidepressantMedication"),
         .(MyLua_Index_PatientID,
           FirstDepressionInd=MyLua_OBEpisode_ID)][
         !duplicated(MyLua_Index_PatientID)] -> priordep

longform[vsacname=="AbortiveOutcome",
         .(MyLua_Index_PatientID,
           FirstAbortedInd=MyLua_OBEpisode_ID)][
         !duplicated(MyLua_Index_PatientID)] -> priorabor

## TODO: what if trimester > 3 bleeds into another episodeID??!
longform[MyLua_Index_PatientID==1812 & MyLua_OBEpisode_ID==1]

setkey(priordep, "MyLua_Index_PatientID")
setkey(priorabor, "MyLua_Index_PatientID")

longform %<>%
  merge(priordep, all.x=TRUE) %>%
  merge(priorabor, all.x=TRUE)

longform[vsacname %chin% c("Depression", "AntidepressantMedication"), ]
longform[vsacname=="AbortiveOutcome"]

# --------------------------------------------------------------- #
# --------------------------------------------------------------- #


## exclude abortive outcomes
## TODO: should we _really_ be removing aborted episodes?

longform[, "AbortiveOutcome" %chin% vsacname,
         .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)][
         V1==TRUE] -> abortiveepisodes
abortiveepisodes[, V1:=NULL]

setkey(longform, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")
setkey(abortiveepisodes, "MyLua_Index_PatientID", "MyLua_OBEpisode_ID")

longform[!abortiveepisodes] -> longform


# 2022-09-04
longform[, uniqueN(MyLua_Index_PatientID)]                              # 6,448
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 7,276
# 2022-08-03
longform[, uniqueN(MyLua_Index_PatientID)]                              # 6,433
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 7,261
# 2022-07-25
longform[, uniqueN(MyLua_Index_PatientID)]                              # 6,332
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 7,139
# 2022-07-18
longform[, uniqueN(MyLua_Index_PatientID)]                              # 5,155
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 5,753

# 2022-06-30
longform[, uniqueN(MyLua_Index_PatientID)]                              # 3,368
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 3,707

# 2022-06-27
longform[, uniqueN(MyLua_Index_PatientID)]                              # 3,694
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 4,148


## join with demographic data
demo <- fread("./data/demo.csv") %>%
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID)

setnames(demo, "MRTL_STS", "marital_status")
demo[, marital_status:=tolower(marital_status)]

racexwalk <- fread("./support/race-xwalk.csv")
ethxwalk <- fread("./support/ethnicity-xwalk.csv")

demo %>%
  merge(racexwalk, by="p_race") %>%
  merge(ethxwalk, by="p_ethcty") -> demo

demo %>% dt_keep_cols(c("MyLua_Index_PatientID", "marital_status",
                        "race", "hisp_latino_p"))

longform <- longform %>% merge(demo, all.x=TRUE, by="MyLua_Index_PatientID")

longform


longform[vsacname=="Cesarean", vsacname:="CesareanBirth"]

# --------------------------------------------------------------- #

# adding delivery data

delivery <- fread("./data/delivery.csv")
delivery[, MyLua_Index_PatientID:=as.integer(str_replace(MyLua_TrimesterData_v2.MyLua_OBEpisode_ID, "\\-\\d+$", ""))]
delivery[, MyLua_OBEpisode_ID:=as.integer(str_replace(MyLua_TrimesterData_v2.MyLua_OBEpisode_ID, "^\\d+\\-", ""))]
delivery[, MyLua_TrimesterData_v2.MyLua_OBEpisode_ID:=NULL]
setcolorder(delivery, c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID"))
delivery %>% head
longform %>% names


delivery %>% dt_counts_and_percents("DLVRY_DT_YEAR")
delivery %>% dt_counts_and_percents("RH_STS")

# TODO: add more cleaning here


longform %>% merge(delivery, by=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID")) -> longform


fwrite(longform, "target/fe-longform.csv")

