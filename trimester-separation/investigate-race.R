#!/usr/local/bin/Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(width=80)
options(warn=2)
options(scipen=10)
options(datatable.prettyprint.char=50)
options(datatable.print.class=TRUE)
options(datatable.print.keys=TRUE)
options(datatable.fwrite.sep='\t')
options(datatable.na.strings="")

args <- commandArgs(trailingOnly=TRUE)

library(colorout)
library(data.table)
library(magrittr)
library(stringr)
library(libbib)

source("~/.rix/tony-utils.R")
# ------------------------------ #


dat <- fread("./results/individual_predictions.csv")

dat <- dat[trimester==3, ]

race_tb <- fread("../target/fe-wideform.csv", select=c("MyLua_Index_PatientID",
                                                    "race", "hisp_latino_p"))
race_tb <- race_tb[race %chin% c("black", "white"), .(patient_id=MyLua_Index_PatientID,
                                                      race)]

race_tb

dat %>% merge(race_tb, by="patient_id") %>% { .[, .(race,
                                                    predicted=predicted_boolean,
                                                    actual=actual_boolean.deptarget)] } -> comb


comb[type %chin% c("false_negatives", "true_negatives"), .N, .(race, type)] %>%
  { .[, xtabs(N ~ race + type)] } %>%
  plot
  prop.table

comb[type %chin% c("false_negatives", "false_positives"), .N, .(race, type)] %>%
  { .[, xtabs(N ~ race + type)] } %>%
  plot


  chisq.test
  plot






# comb[predicted==0 & actual==0, type:="true_negatives"]
# comb[predicted==0 & actual==1, type:="false_negatives"]
# comb[predicted==1 & actual==0, type:="false_positives"]
# comb[predicted==1 & actual==1, type:="true_positives"]
#
# library(ggmosaic)
#
# options(warn=1)
#
# ggplot(comb) +
#   geom_mosaic(aes(x=product(race, type), fill=race),
#        divider=c("vspine", "hbar")) 
#
