#This Code takes the raw point intercept data, converts it into a Database friendly style, and then calculates proportions of substrate types for the year 2011
p_int<-read.csv("01_PointIntercept_HIMB_2011.csv")
library(reshape2)
p_melt<-melt(p_int,id=(c("Site","Date")))
colnames(p_melt)[colnames(p_melt)=="variable"]<-"Distance"###rename
colnames(p_melt)[colnames(p_melt)=="value"]<-"Substrate"###rename
p_melt$Distance<-gsub("X", "", paste(p_melt$Distance))###Remove X's from the Distance variable
p_melt$Omitted<-ifelse(p_melt$Substrate=="PCMC"|p_melt$Date=="7/7/2011",1,0)#Create a column that flags data for ommision that are from 2011-07-07 or were labelled as MCPC for substrate
write.csv(p_melt, file="02_PointIntercept_HIMB_2011_DB.csv",row.names = FALSE)###Write a Database Friendly File
####Data Analysis#####
p_sbst<-subset(p_melt,Omitted=="0")#select all data that are NOT supposed to be ommitted
site_sum<-as.data.frame.matrix(table(p_sbst$Site, p_sbst$Substrate))#sum all substrates by site
p_sbst<-group_by(p_sbst,Site)
site_counts<-summarise(p_sbst,length(Site))###Check that the counts are the same for each site (siteB should have 1 less)
site_counts<-site_counts[,2]#take only the count data
substrate_proportion<-site_sum/matrix(site_counts$`length(Site)`,8,9)#make a matrix that replicates counts by site
substrate_proportion<-cbind(Site =c("A","B","C","D","G","H","I","J"),substrate_proportion) #calculate proportions of substrates
prop_melt<-melt(substrate_proportion,id=(c("Site")))###put proportion data into Database friendly format
colnames(prop_melt)[colnames(prop_melt)=="variable"]<-"Substrate"
colnames(prop_melt)[colnames(prop_melt)=="value"]<-"Proportion"
write.csv(prop_melt, file="03_PointIntecept_Proportion_2011.csv") #write proportion file
