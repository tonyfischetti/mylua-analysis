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

setwd(here("using-ccsr"))




# --------------------------------------------------------------- #

ccsr <- fread("../data/dx-ccsr.csv", na.strings=c("", "NULL", "' '"))
setnames(ccsr, str_replace_all(names(ccsr), "'", ""))
dt_set_clean_names(ccsr)

ccsr <- ccsr[, .(ICD10CM=icd_1__cm_code,
                 ccsr_cat_1_ip=default_ccsr_category_description_ip,
                 ccsr_cat_1_op=default_ccsr_category_description_op,
                 ccsr_cat_1=ccsr_category_1_description,
                 ccsr_cat_2=ccsr_category_2_description,
                 ccsr_cat_3=ccsr_category_3_description,
                 ccsr_cat_4=ccsr_category_4_description,
                 ccsr_cat_5=ccsr_category_5_description,
                 ccsr_cat_6=ccsr_category_6_description)]

ccsr[, ICD10CM:=str_replace_all(ICD10CM, "'", "")]
ccsr %<>% melt(id.var="ICD10CM")
setorder(ccsr, "ICD10CM")
ccsr <- ccsr[value!=" "][, .(ICD10CM, CCSR_cat=value)]
ccsr <- unique(ccsr)



trimesters <- fread("../data/trimester.csv", na.strings=c("", "NULL"))

# --------------------------------------------------------------- #


trimesters[, .(MyLua_Index_PatientID=MyLUA_Index_PatientID,
               MyLua_OBEpisode_ID,
               Trimester,
               Age=Age_at_Trimester,
               ICD10CM=DX_Codes,
               PHQ=PHQ_Scrs,
               EPDS=EPDS_Scrs)] -> trimesters


trimesters %<>%
  # make long (not wide)
  melt(id.vars=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID", "Trimester",
                 "Age", "PHQ", "EPDS"),
       variable.name="codesystem",
       value.name="code") %>%
  # separate the array into rows
  separate_rows(code, sep="; ") %>%
  # separate_rows(code, sep="; ?") %>%
  filter(!is.na(code))

setDT(trimesters)

tphq <- trimesters[, PHQ]
tphq <- sapply(sapply(sapply(tphq, function(x) str_split(x, "; ")), as.integer), max)
trimesters[, PHQ:=tphq]

tepds <- trimesters[, EPDS]
tepds <- sapply(sapply(sapply(tepds, function(x) str_split(x, "; ")), as.integer), max)
trimesters[, EPDS:=tepds]

trimesters[codesystem=="ICD10CM", ICD10CM:=str_replace_all(code, "\\.", "")]
trimesters %>% dt_del_cols("codesystem", "code")

trimesters[, .(ICD10CM)]
ccsr[, .(ICD10CM)]

setkey(trimesters, "ICD10CM")
setkey(ccsr, "ICD10CM")





trimesters %>% merge(ccsr, allow.cartesian=TRUE) -> comb

trimesters[, .N]  # [1] 273,627
ccsr[, .N]        # [1] 102,130
comb[, .N]        # [1] 442,914
unique(comb)[,.N] # [1] 232,888


comb[, .(MyLua_Index_PatientID, MyLua_OBEpisode_ID,
         Trimester, CCSR_cat)] -> comb
# comb <- unique(comb)


## SHOULD I INCLUDE ALL!!!


# NOTE: let's try incidents 500 and above

comb %>% dt_counts_and_percents("CCSR_cat") -> tmp
keepers <- tmp[count>=500 &
               CCSR_cat!="TOTAL" &
               !str_detect(CCSR_cat, "Unacceptable"),
             CCSR_cat]

comb <- comb[CCSR_cat %chin% keepers, ]
comb[, CCSR_cat:=str_trim(str_sub(CCSR_cat, 1, 40))]


comb
longform <- copy(comb)









## join with demographic data
demo <- fread("../data/demo.csv") %>%
  rename(MyLua_Index_PatientID=MyLUA_Index_PatientID)

setnames(demo, "MRTL_STS", "marital_status")
demo[, marital_status:=tolower(marital_status)]

racexwalk <- fread("../support/race-xwalk.csv")
ethxwalk <- fread("../support/ethnicity-xwalk.csv")

demo %>%
  merge(racexwalk, by="p_race") %>%
  merge(ethxwalk, by="p_ethcty") -> demo

demo %>% dt_keep_cols(c("MyLua_Index_PatientID", "marital_status",
                        "race", "hisp_latino_p"))

longform <- longform %>% merge(demo, all.x=TRUE, by="MyLua_Index_PatientID")

longform



# --------------------------------------------------------------- #

# adding delivery data

delivery <- fread("../data/delivery.csv")
delivery[, MyLua_Index_PatientID:=as.integer(str_replace(MyLua_TrimesterData_v2.MyLua_OBEpisode_ID, "\\-\\d+$", ""))]
delivery[, MyLua_OBEpisode_ID:=as.integer(str_replace(MyLua_TrimesterData_v2.MyLua_OBEpisode_ID, "^\\d+\\-", ""))]
delivery[, MyLua_TrimesterData_v2.MyLua_OBEpisode_ID:=NULL]
setcolorder(delivery, c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID"))
delivery %>% head
longform %>% names


delivery %>% dt_counts_and_percents("DLVRY_DT_YEAR")
delivery %>% dt_counts_and_percents("RH_STS")

# TODO: add more cleaning here


longform[, MyLua_OBEpisode_ID:=as.integer(str_replace(MyLua_OBEpisode_ID, "^.+-", ""))]

longform %>% merge(delivery, by=c("MyLua_Index_PatientID", "MyLua_OBEpisode_ID")) -> longform


fwrite(longform, "target/fe-longform.csv")

