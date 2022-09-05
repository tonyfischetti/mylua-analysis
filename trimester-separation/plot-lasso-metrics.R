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


library(ggplot2)


dat -1<- fread("./results/lasso.csv")

ggplot(dat, aes(x=trimester, y=PCC)) +
  geom_line() +
  geom_hline(yintercept=88) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("base rate compared to PCC of lasso model throughout trimesters")
ggsave("./results/PCC-plot.png")



