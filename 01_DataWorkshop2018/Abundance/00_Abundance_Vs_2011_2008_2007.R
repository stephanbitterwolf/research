#This Code takes the raw point intercept data, converts it into a Database friendly style, and then calculates proportions of substrate types for the year 2011
setwd("~/School/UVI/Science/Research/MARC/MARC Hawaii/Excel Files/01_DataWorkshop2018/Abundance")
library(reshape2)
library(dplyr)
####Load File####
Abundance<-read.csv("01_Abundance_Vs_2011_2008_2007.csv")
###Rename Columns#####
colnames(Abundance)[colnames(Abundance)=="ï..Author"]<-"Author"###rename
colnames(Abundance)[colnames(Abundance)=="X..5.cm"]<-">5"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X6.10.cm"]<-"6-10"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X11.20.cm"]<-"10-20"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X21.40.cm"]<-"20-40"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X41.80.cm"]<-"40-80"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X81.160.cm"]<-"80-160"###rename (units are CM)
colnames(Abundance)[colnames(Abundance)=="X..160.cm"]<-">160"###rename (units are CM)
####Make Data DataBase Friendly####
Abundance_melt<-melt(Abundance,id=(c("Author",	"Site",	"Year",	"GenusSpecies",	"Genus",	"Species",	"Area",	"Zone")))
##Rename New Columns###
colnames(Abundance_melt)[colnames(Abundance_melt)=="variable"]<-"SizeClass"###rename (UNits are CM)
colnames(Abundance_melt)[colnames(Abundance_melt)=="value"]<-"Count"###rename (Total for Each Site and size class)
write.csv(Abundance_melt, file="02_Abundance_Vs_2011_2008_2007_DB.csv",row.names = FALSE)###Write a Database Friendly File
####Data Analysis#####

