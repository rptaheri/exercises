records = read.csv(records_loc) #read csv file

library(foreign)
library(gmodels)
library(vcd)
library(epiR)
library(NSM3)

occupation <- factor(records.education_level_)