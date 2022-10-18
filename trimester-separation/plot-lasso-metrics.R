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


dat <- fread("./results/lasso.csv")

ggplot(dat, aes(x=trimester, y=PCC)) +
  geom_line() +
  geom_hline(yintercept=88) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("base rate compared to PCC of lasso model throughout trimesters")
ggsave("./results/PCC-plot.png")



read.single.coef <- function(trime) {
  fn <- sprintf("results/coeffs/lasso-coeffs-%d.csv", trime)
  # print(fn)
  tmp <- fread(fn)
  tmp[, up_to_trimester:=trime]
  setcolorder(tmp, "up_to_trimester")
  tmp[]
}


0:8 %>%
  lapply(read.single.coef) %>%
  rbindlist -> comb

comb[is.na(coefficient), coefficient:=0]

# comb <- comb[str_detect(feature, "TRUE")]
comb[up_to_trimester==8 & coefficient >= .15, feature] -> these
comb <- comb[feature %chin% these]

ggplot(comb, aes(x=up_to_trimester, y=coefficient, color=feature)) +
  geom_line()

ggsave("./results/coeffs-through-the-trimesters.png")


