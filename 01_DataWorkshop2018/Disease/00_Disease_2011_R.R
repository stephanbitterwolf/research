#This Code takes the raw disease occurence data, converts it into a Database friendly style for the year 2011
setwd("~/School/UVI/Science/Research/MARC/MARC Hawaii/Excel Files/01_DataWorkshop2018/Disease/")
library(reshape2)
library(dplyr)
####Load File####
Disease<-read.csv("01_Disease_2011.csv")
###Rename Columns#####
colnames(Disease)[colnames(Disease)=="?..Author"]<-"Author"###rename
colnames(Disease)[colnames(Disease)=="X.5cm"]<-">5"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X5.10cm"]<-"5-10"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X10.20cm"]<-"10-20"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X20.40cm"]<-"20-40"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X40.80cm"]<-"40-80"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X80.160cm"]<-"80-160"###rename (units are CM)
colnames(Disease)[colnames(Disease)=="X.160"]<-">160"###rename (units are CM)
####Make Data DataBase Friendly####
Disease_melt<-melt(Disease,id=(c("Author","Year", "Site",	"Area",	"Zone","Quadrat","Disease")))
##Rename New Columns###
colnames(Disease_melt)[colnames(Disease_melt)=="variable"]<-"SizeClass"###rename (UNits are CM)
colnames(Disease_melt)[colnames(Disease_melt)=="value"]<-"Count"###rename (Total for Each quadrat and size class)
write.csv(Disease_melt, file="02_Disease_2011_DB.csv",row.names = FALSE)###Write a Database Friendly File
####Data Analysis#####
site_group<-group_by(Disease_melt,Site)
Disease_sum<-as.data.frame.matrix(dcast(site_group, Site~Disease, value.var='Count', sum))## Calculates the sum of disease counts by site
