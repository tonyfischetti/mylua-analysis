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

valueset <- fread("./data/valuesets.csv") %>%
  select(vsacname, codesystem, code, description)

trimesters <- fread("./data/trimester.csv")

trimesters %>%
  ### just the necessary columns, for demonstration
  select(MyLUA_Index_PatientID, MyLua_OBEpisode_ID, Trimester, DX_Codes,
         Meds_RXNORM, CPT_Codes, ObsrvtnData.Obsrvtn_Array) %>%
  # rename weird column name
  rename(LOINC=ObsrvtnData.Obsrvtn_Array,     # observations (TODO: describe)
         RXNORM=Meds_RXNORM,                  # medicines
         CPT=CPT_Codes,                       # procedures
         ICD10CM=DX_Codes) %>%                # diagnoses
  # we want to use data.table's implementation of "melt"
  as.data.table %>%
  # make long (not wide)
  melt(id.vars=c("MyLUA_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester"),
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
                          TRUE                ~ code)) %>%
  # do the join
  inner_join(valueset, by=c("codesystem", "code")) %>%
  # re-order variables
  select(MyLUA_Index_PatientID,
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

fwrite(longform, "target/fe-longform.csv")


### now wide form

# columns to limit to trimesters 0-3
dcast(longform[Trimester<=3, ],
      MyLUA_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part1

options(warn=1)
part1 %>% dt_keep_cols(c("MyLUA_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AbortiveOutcome",
                         "CesareanBirth",  # we have none right now
                         "Diarrhea",
                         "Hypertension", # we have none right now
                         "Migraine",
                         "Preeclampsia",
                         "Pharyngitis",
                         "Sleep Disorders",
                         "Vomiting",
                         # added valuesets for LOINCS - CB220522
                         "MYLUA-ANXIETY",
                         "MYLUA-LOINCTEST",
                         "MYLUA-MOOD",
                         "MYLUA-PAIN",
                         "MYLUA-SDOH",
                         "MYLUA-SLEEP"
                         ))


# columns to limit to trimester 4 and after
dcast(longform[Trimester>=4, ],
      MyLUA_Index_PatientID + MyLua_OBEpisode_ID ~ vsacname,
      value.var="code",
      fun.aggregate=uniqueN) -> part2
part2 %>% dt_keep_cols(c("MyLUA_Index_PatientID", "MyLua_OBEpisode_ID",
                         "AntidepressantMedication",
                         "Depression"))

part1 %>% merge(part2, all=TRUE) -> wideform
wideform[is.na(wideform)] <- 0

fwrite(wideform, "target/fe-wideform.csv")

