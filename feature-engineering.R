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

valueset <- fread("./data/valuesets.csv", na.strings=c("", "NULL")) %>%
  select(vsacname, codesystem, code, description)

valueset

trimesters <- fread("./data/trimester.csv")


trimesters %<>%
  ### just the necessary columns, for demonstration
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID) %>%
  select(MyLua_Index_PatientID, MyLua_OBEpisode_ID, Trimester, DX_Codes,
         Meds_RXNORM, CPT_Codes, ObsrvtnData.Obsrvtn_Array) %>%
  # rename weird column name
  rename(LOINC=ObsrvtnData.Obsrvtn_Array,     # observations (TODO: describe)
         RXNORM=Meds_RXNORM,                  # medicines
         CPT=CPT_Codes,                       # procedures
         ICD10CM=DX_Codes) %>%                # diagnoses
  # we want to use data.table's implementation of "melt"
  as.data.table

trimesters %<>%
  # make long (not wide)
  melt(id.vars=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester"),
       variable.name="codesystem",
       value.name="code") %>%
  # separate the array into rows
  separate_rows(code, sep="; ") %>%
  # unnecessary, as we'll do an inner join
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
  # inner_join(valueset, by=c("codesystem", "code")) %>%
  left_join(valueset, by=c("codesystem", "code")) %>%
  # re-order variables
  select(MyLua_Index_PatientID,
         MyLua_OBEpisode_ID,
         Trimester,
         vsacname,
         codesystem,
         code,
         loincvalue,
         description,
         # everything()
  ) -> longform

setDT(longform)

longform

# inner join is 17,271 (SO FAR)
# 3,875 unique patients
# 4,394 unique pregnencies
## new valueset
# 20,409 inner join

# left join is 1,135,144
# 7,208 patients
# 8,334 pregnancies


## join with demographic data
demo <- fread("./data/demo.csv") %>%
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID)

longform <- longform %>% merge(demo, all.x=TRUE, by="MyLua_Index_PatientID")

longform


fwrite(longform, "target/fe-longform.csv")




# mad abortive outcomes. what's going on?
# oh, I guess it's due to the inner join



## DO WE WANT TO EXCLUDE ABORTIVE OUTCOMES?!




longform %>% dt_counts_and_percents("code")




### now wide form

# columns to limit to trimesters 0-3
dcast(longform[Trimester<=3, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part1

options(warn=1)
part1 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AbortiveOutcome",
                         # "AntidepressantMedication",
                         "Anxiety",
                         "BetaBlockers",
                         "CesareanBirth",
                         # "Depression",
                         "Diarrhea",
                         "Hypertension",
                         "Hypothyroidism",
                         "Migraine",
                         "Mood",
                         "Preeclampsia",
                         "Pharyngitis",
                         "Sleep",
                         "Vomiting"
                         ))




# columns to limit to trimester 4 and after
dcast(longform[Trimester>=4, ],
      MyLua_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part2
part2 %>% dt_keep_cols(c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AntidepressantMedication",
                         "Depression"))





## putting together pieces for the wide form
part1 %>%
  merge(part2, all=TRUE) %>%
  merge(demo, all.x=TRUE, by="MyLua_Index_PatientID") -> wideform
wideform[is.na(wideform)] <- 0


fwrite(wideform, "target/fe-wideform.csv")




## preparing model matrix for model building, etc...

modmat <- copy(wideform)


race.xwalk <- fread("./support/race-xwalk.csv")
eth.xwalk <- fread("./support/ethnicity-xwalk.csv", na.strings=c("NA", ""))

modmat %<>%
  merge(race.xwalk, all.x=TRUE, by="p_race") %>%
  merge(eth.xwalk, all.x=TRUE, by="p_ethcty")


setnames(modmat, "MRTL_STS", "marital_status")
modmat[, marital_status:=tolower(marital_status)]


# no AbortiveOutcome?
modmat <- modmat[AbortiveOutcome<1]

setcolorder(modmat, c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID"))

modmat %<>% dt_del_cols("CDC_ETHCTY_CD", "CDC_RACE_CD", "S_ZIP")


modmat %>% fwrite("./target/model-matrix.csv", sep=",")


