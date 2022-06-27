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

trimesters <- fread("./data/trimester.csv", na.strings=c("", "NULL"))


# --------------------------------------------------------------- #

trimesters %<>%
  ### just the necessary columns, for demonstration
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID,
         Age=Age_at_Trimester) %>%
  select(MyLua_Index_PatientID, MyLua_OBEpisode_ID, Trimester, Age, DX_Codes,
         Meds_RXNORM, CPT_Codes, ObsrvtnData.Obsrvtn_Array) %>%
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
  melt(id.vars=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester", "Age"),
       variable.name="codesystem",
       value.name="code") %>%
  # separate the array into rows
  separate_rows(code, sep="; ") %>%
  filter(!is.na(code)) %>%
  # extract the LOINC value
  mutate(loincvalue = ifelse(codesystem=="LOINC",
                             str_replace(code, "^.+:(.+)$", "\\1"),
                             NA_character_)) %>%
  # conditionally fix the codes for LOINCs using regular expressions
  mutate(code = case_when(codesystem=="LOINC" ~ str_replace(code, ":.+$", ""),
                          TRUE                ~ code))

trimesters %>%
  # do the join
  inner_join(valueset, by=c("codesystem", "code")) %>%
  # left_join(valueset, by=c("codesystem", "code")) %>%
  # re-order variables
  select(MyLua_Index_PatientID,
         MyLua_OBEpisode_ID,
         Trimester,
         Age,
         vsacname,
         codesystem,
         code,
         loincvalue,
         description,
         # everything()
  ) -> longform

setDT(longform)

longform


## exclude abortive outcomes
longform <- longform[vsacname!="AbortiveOutcome",]

# 2022-06-27
longform[, uniqueN(MyLua_Index_PatientID)]                              # 3,694
longform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN    # 4,148


## join with demographic data
demo <- fread("./data/demo.csv") %>%
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID)

longform <- longform %>% merge(demo, all.x=TRUE, by="MyLua_Index_PatientID")

longform


fwrite(longform, "target/fe-longform.csv")


# --------------------------------------------------------------- #


### now wide form

## first get the ages
longform[, .(Age=min(Age)), .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] -> ages
setkey(ages,"MyLua_Index_PatientID", "MyLua_OBEpisode_ID")


## columns to limit to trimesters 0-3
dcast(longform[Trimester<=3, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part1

part1 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AntidepressantMedication",
                         "Anxiety",
                         "BetaBlockers",
                         "CesareanBirth",
                         "Depression",
                         "Diarrhea",
                         "Hypertension",
                         "Hypothyroidism",
                         "Migraine",
                         "Mood",
                         "Preeclampsia",
                         "Pharyngitis",
                         "Sleep"))

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
                         "AntidepressantMedication",
                         "Depression"))
# part2[, DepressionTarget:=Depression+AntidepressantMedication]
# part2[, Depression:=NULL]
# part2[, AntidepressantMedication:=NULL]
## NOTE: I don't think (for POST delivery) we should consider antidepressant
## use as a sign of depression
setnames(part2, "Sleep", "PostnatalSleep")


race.xwalk <- fread("./support/race-xwalk.csv")
eth.xwalk <- fread("./support/ethnicity-xwalk.csv", na.strings=c("NA", ""))

## putting together pieces for the wide form
ages %>%
  merge(part1) %>%
  merge(part2) %>%
  merge(demo, all.x=TRUE, by="MyLua_Index_PatientID") %>%
  merge(race.xwalk, all.x=TRUE, by="p_race") %>%
  merge(eth.xwalk, all.x=TRUE, by="p_ethcty") -> wideform


setnames(wideform, "MRTL_STS", "marital_status")
wideform[, marital_status:=tolower(marital_status)]

setcolorder(wideform, c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID"))

wideform %<>% dt_del_cols("CDC_ETHCTY_CD", "CDC_RACE_CD", "S_ZIP",
                          "p_ethcty", "p_race")


fwrite(wideform, "target/fe-wideform.csv")

# --------------------------------------------------------------- #

## multiple birth

wideform[,.N]
wideform[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID)] %>% uniqueN
wideform[, .(MyLua_Index_PatientID)] %>% uniqueN

wideform[MyLua_OBEpisode_ID>1, .(MyLua_Index_PatientID)] -> morethanone

wideform[MyLua_OBEpisode_ID==1] %>%
  merge(morethanone, by="MyLua_Index_PatientID") -> tmp

tmp[, .N, Depression>0]

# 34% of patients with more than one birth had postpartum depression
# in their first pregnancy!

tmp[Depression>0, .(MyLua_Index_PatientID)] -> morethanonewithppdinfirst

morethanonewithppdinfirst

morethanonewithppdinfirst %>%
  merge(wideform[MyLua_OBEpisode_ID>1], by="MyLua_Index_PatientID") -> tmp

tmp[, .N, Depression>0]

# 62% of those with ppd in the first birth had it in subsequent birth (kindof)

wideform[, .N, Depression>0]

# compared to only 27% percent for everyone

wideform[!(MyLua_Index_PatientID %in% morethanone[, MyLua_Index_PatientID]), .N, Depression>0]

# and 25% of those with only one birth

## TODO: so we have to add an indicator for this

# --------------------------------------------------------------- #

## odds ratio

# wideform[, .N, .(hisp_latino_p, DepressionTarget>0)] %>%




