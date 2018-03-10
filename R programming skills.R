# Read Data from ONLINE SOURCE!! (HOW COULD I USE THIS TO LOG INTO GOOGLE DRIVE FOR EXAMPLE?)
georoc <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
#Reads the first row (essentially, to get a understanding of the data present)
head(georoc)
#Call out a specific column in my data frame
georoc$SIO2
#Call out a specific ROW in my DF
georoc[3,]
#Call out a specific column in my DF (the second column)
georoc[,2]
#Call out specific value in ROW 5 and Column 7
georoc[5,7]
#Call out value on the 100th row in the SIO2 column from my DF
georoc$SIO2[100]
#plot histogra
hist(georoc$AL2O3)
#assign AL2o3 its own varible
al203<-georoc$AL2O3
al203
#get mean of SIO2 and REMOVE NAs
mean(al203, na.rm=TRUE)
####### END OF FIRST QUIZ #####
#READ NEW RANDOM DATA FROM ONLINE and call the data set RANDOM#####
random <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/rand_data.csv")
##Look at columns
head(random)
#question 1 central tendency of v14
hist(random$V14)
###mean is most appropriate
mean(random$V14,na.rm = TRUE)
#question 2 central tendency of v42
hist(random$V42)
###median is most appropriate
median(random$V42,na.rm = TRUE)
#question 3 central tendency of v10
hist(random$V10)
###median is most appropriate (HOW COULD I TELL IT TO AUTOMATICALLY COPY THE ANSWER TO MY CLIPBOARD?)
median(random$V10,na.rm = TRUE)
#question 4 central tendency of v5
hist(random$V5)
###mean is most appropriate (HOW COULD I TELL IT TO AUTOMATICALLY COPY THE ANSWER TO MY CLIPBOARD?)
mean(random$V5,na.rm = TRUE)
#question 5 central tendency of v36
hist(random$V36)
###median is most appropriate ()
median(random$V36,na.rm = TRUE)
#question 6 central tendency of v50
hist(random$V50)
###median is most appropriate ()
median(random$V50,na.rm = TRUE)
#question 7 central tendency of v9
hist(random$V9)
###mean is most appropriate ()
mean(random$V9,na.rm = TRUE)
#question 8 central tendency of v15
hist(random$V15)
###mean is most appropriate ()
mean(random$V15,na.rm = TRUE)
#question 9 central tendency of v20
hist(random$V20)
###mean is most appropriate ()
mean(random$V20,na.rm = TRUE)
#question 10 central tendency of v4
hist(random$V4)
###mean is most appropriate ()
mean(random$V4,na.rm = TRUE)
#######################Dispersion in R#################
#load random again
random <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/rand_data.csv")
hist(random$V42)
IQR(random$V42, na.rm=TRUE)
#q2
hist(random$V47)
IQR(random$V47, na.rm=TRUE)
#q3
hist(random$V49)
IQR (random$V49, na.rm=TRUE)
#q4
hist(random$V16)
sd(random$V16, na.rm=TRUE)
#q5
hist(random$V4)
sd(random$V4, na.rm=TRUE)
#q6
IQR(random$V48, na.rm=TRUE)
#q7
IQR(random$V8, na.rm=TRUE)
#q8
var(random$V13, na.rm=TRUE)
#q9
hist(random$V7)
IQR(random$V7, na.rm=TRUE)
#q10
var(random$V24, na.rm=TRUE)
##############################
#calculating standard error in R
#Omit all na values from entire file
#####na.omit(random) ACTUALLY THIS REMOVES ENTIRE COLUMNS IF THEY HAVE NA values in them so dont use it on a matrix or dataframe!
### you have to omit them from vector files It's also possible to nest functions inside other functions. In that case, the standard mathematical order of operations applies, and commands are executed from inside to out. For example length(na.omit(x)) would execute the na.omit function first, and then calculate the length of the resulting vector. You can read this as "length of na.omit(x)." Sometimes multiple nested functions makes for difficult-to-read code, if those functions are complex, and it's sometimes difficult to make sure all of the parentheses are in the right place.
sd(random$V4, na.rm=TRUE)/sqrt(length(na.omit(random$V4)))
#q1
sd(random$V1, na.rm=TRUE)/sqrt(length(na.omit(random$V1)))
#q2 lower bound of the 95% confidence interval
sdv35<-
  sd(random$V35, na.rm=TRUE)
sdv35
sdv35*1.96
meanv35<-
  mean(random$V35, na.rm=TRUE)
meanv35
meanv35-(1.96*sdv35)
#q3
sdv35/sqrt(length(na.omit(random$V35)))
#q4
sdv16<-
  sd(random$V16, na.rm=TRUE)
sdv16
sdv16*1.96
meanv16<-
  mean(random$V16, na.rm=TRUE)
meanv16
meanv16-(1.96*sdv16)
#q5
## gonna try a for loop to do the math of everything

library(plyr)
library(reshape2)
melted<-melt(random)
summarized<-ddply(melted, c("variable"), summarise,mean = mean(na.omit(value)),IQR = IQR(na.omit(value))  ,sd = sd(na.omit(value)),
      sem = sd(na.omit(value))/(sqrt((length(na.omit(value))))), lower95CI = mean(na.omit(value))-(sd(na.omit(value))/(sqrt((length(na.omit(value)))))*1.96))
### t tests in R####
basalt_Al <- georoc$AL2O3[georoc$rock.type == "Basalt"] #selects Al2O3 data where rock type is basalt

andesite_Al <- georoc$AL2O3[georoc$rock.type == "Andesite"] #selects Al2O3 data where rock type is andesite

t.test(basalt_Al, andesite_Al) #performs t test on the two samples
### t test quiz###
#load new file
gwater <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/gwater_nitrogen.csv")
#Reads the first row (essentially, to get a understanding of the data present)
##head(gwater)
#t test to compare between biochar or non biochar
control<-gwater$nitrate[gwater$treatment=="control"]#selects all nitrate data where treatment is control
experimental<-gwater$nitrate[gwater$treatment=="biochar"]#selects all nitrate data where treatment is biochar
#I am going to do a one tailed t test because my alternate hypothesis is that the biochar will have less nitrate than the control
t.test(control,experimental,alternative="g")
#ONE TAILED TEST WAS WRONG BECAUSE QUESTION STATES THAT I AM SEEING IF THERE IS A DIFFERENCE
#REDO with two tailed test
t.test(control,experimental)
#load new coral growth file
coralg <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/coral_growth.csv")
lagoon<- coralg$growth_rate[coralg$site=="lagoon"]
ph<- coralg$growth_rate[coralg$site=="low_pH"]
t.test(lagoon, ph)
#load new rift file
rift <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/rift_subsidence.csv")
volcanic<- rift$subsidence[rift$margin_type=="volcanic"]
nonvolcanic<- rift$subsidence[rift$margin_type=="non-volcanic"]
t.test(volcanic, nonvolcanic)
#load new uranium file#### WILL REQUIRE PAIRED TTEST
uranium <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/upb_comparison.csv")
shrimp<- uranium$shrimp_PbU
tims<- uranium$tims_PbU
t.test(shrimp, tims,paired = TRUE)
#load new antarctic file
ant <- read.csv("https://people.ucsc.edu/~mclapham/eart125/data/antarctic.csv")
direct<- ant$ice_motion[ant$glacier_type=="direct"]
shelf<- ant$ice_motion[ant$glacier_type=="ice_shelf"]
t.test(direct, shelf)
############################## THE F TEST##########################
#load new leafwax file
lw<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/antarctic.csv")
## I did this but didnt save the data...
##var.test(basalt_Al, andesite_Al) #performs F test on the two samples
## ADD ONS###
install.packages("dplyr")
library(dplyr)
georoc_gp <- group_by(georoc, tectonic.setting, rock.type) #groups the georoc data frame by values in the tectonic setting column and the rock.type column
summarize(georoc_gp, mean(SIO2)) #calculates the mean SIO2 value in each group
summarize(georoc_gp, mean(SIO2), n()) #calculates the mean SIO2 value in each group and the number of observations in each group
#using pipes to join (control shift m)
georoc %>%
  
  group_by(tectonic.setting, rock.type) %>%
  
  summarize(mean(SIO2))
### ANOVA ####
#aov_results <- aov(measurements ~ categories, data=object)
AL2O3_aov <- aov(AL2O3 ~ rock.type, data = georoc)
summary(AL2O3_aov)
TukeyHSD(AL2O3_aov)#perform post hoc
#q1-2
incision<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/incision.csv")
incision_gp<-group_by(incision,rock_type)
summarize(incision_gp,mean(incision))
incision_anova<-aov(incision~rock_type, data=incision)
summary(incision_anova)
#Q3-4
mercury<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/mercury.csv")
mercury_gp<-group_by(mercury, region)
summarize(mercury_gp, mean(hg_flux))
mercury_aov<-aov(hg_flux~region, data=mercury)
summary(mercury_aov)
TukeyHSD(mercury_aov)
#q5-6
pluto<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/pluto.csv")
pluto_gp<-group_by(pluto,ice_type)
summarize(pluto_gp,mean(aspect_ratio))
pluto_aov<-aov(aspect_ratio~ice_type,data=pluto)
summary(pluto_aov)
TukeyHSD(pluto_aov)
#q7-8
coyote<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/coyote.csv")
coyote_gp<-group_by(coyote,site)
summarize(coyote_gp,mean(d15N))
coyote_aov<-aov(d15N~site, data=coyote)
summary(coyote_aov)
TukeyHSD(coyote_aov)
#q9-10
schist<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/schist.csv")
summary(schist)
schist_gp<-group_by(schist,unit)
summarize(schist_gp,mean(age))
schist_aov<-aov(age~unit,data=schist)
summary(schist_aov)
######################### Shapiro Wilk TEST############################
##shapiro.test(x) #where x is a vector
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
shapiro.test(georoc$AL2O3)
hist(georoc$AL2O3)
#q1-2
andesite_Al<-georoc$AL2O3[georoc$rock.type=="Andesite"]
shapiro.test(andesite_Al)
#q3
rift_al<-georoc$AL2O3[georoc$tectonic.setting=="Rift"]
shapiro.test(rift_al)
#q4
rhyolite_si<-georoc$SIO2[georoc$rock.type=="Rhyolite"]
shapiro.test(rhyolite_si)
hist(rhyolite_si)
#q5
archean_si<-georoc$SIO2[georoc$tectonic.setting=="Archean craton"]
shapiro.test(archean_si)
#q6
basalt_sr<-georoc$SR[georoc$rock.type=="Basalt"]
shapiro.test(basalt_sr)
hist(basalt_sr)
#q7
rhyolite_co<-georoc$CO[georoc$rock.type=="Rhyolite"]
shapiro.test(rhyolite_co)
#q8
conv_cao<-georoc$CAO[georoc$tectonic.setting=="Convergent margin"]
shapiro.test(conv_cao)
#q9
backarc_zr<-georoc$ZR[georoc$tectonic.setting=="Backarc basin"]
shapiro.test(backarc_zr)
#q10
ocean_mgo<-georoc$MGO[georoc$tectonic.setting=="Ocean island"]
shapiro.test(ocean_mgo)
############# qq plot #################
qqnorm(georoc$AL2O3)
qqline(georoc$AL2O3)
hist(georoc$AL2O3)
qqnorm(georoc$SR)
qqline(georoc$SR)
hist(georoc$SR)
###ggplot2#####
#install.packages("ggplot2")
library(ggplot2)
ggplot(georoc,aes(AL2O3))+geom_histogram()+theme_classic()
ggplot(georoc, aes(AL2O3)) + stat_ecdf() + theme_classic() + ylab("Probability AL2O3 value is less than/equal to X")
### some stuff about picking data by multiple criteria
#### KS tests####
basalt_Al<-georoc$AL2O3[georoc$rock.type=="Basalt"]
andesite_Al
basalt_Al
ks.test(basalt_Al,andesite_Al)
georoc %>% 
  filter(rock.type=="Basalt"|rock.type=="Dacite") %>% 
  ggplot(aes(AL2O3))+stat_ecdf(aes(color=rock.type))+theme_classic()
##q1-2
venus<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/venuscrater.csv")
psize<-venus$diameter[venus$modification.state=="p"]
f1size<-venus$diameter[venus$modification.state=="f1"]
ks.test(psize,f1size)
##q3
zircon<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/zircon.csv")
zircon %>% 
  filter(Region=="BC"|Region=="NT"|Region=="NU"|Region=="SC"|Region=="TN"|Region=="UT") %>% 
  ggplot(aes(Age))+stat_ecdf(aes(color=Region))+theme_classic()
BC<-zircon$Age[zircon$Region=="BC"]
NU<-zircon$Age[zircon$Region=="NU"]
NT<-zircon$Age[zircon$Region=="NT"]
UT<-zircon$Age[zircon$Region=="UT"]
SC<-zircon$Age[zircon$Region=="SC"]
TN<-zircon$Age[zircon$Region=="TN"]
#q3
ks.test(BC,NU)
#q4
ks.test(NT,NU)
#q5
ks.test(NT,UT)
#q6
ks.test(SC,TN)
#q7-10
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
basalt_mgo_act<-georoc$MGO[georoc$rock.type=="Basalt"&georoc$tectonic.setting=="Archean craton"]
basalt_mgo_cmt<-georoc$MGO[georoc$rock.type=="Basalt"&georoc$tectonic.setting=="Convergent margin"]
ks.test(basalt_mgo_act,basalt_mgo_cmt)
#q8
rhyolite_k2o_i<-georoc$K2O[georoc$rock.type=="Rhyolite" & georoc$tectonic.setting=="Intraplate"]
rhyolite_k2o_c<-georoc$K2O[georoc$rock.type=="Rhyolite" & georoc$tectonic.setting=="Convergent margin"]
ks.test(rhyolite_k2o_i,rhyolite_k2o_c)
#q9
basalt_ni_bb<-georoc$NI[georoc$rock.type=="Basalt" & georoc$tectonic.setting=="Backarc basin"]
basalt_ni_s<-georoc$NI[georoc$rock.type=="Basalt" & georoc$tectonic.setting=="Seamount"]
ks.test(basalt_ni_bb,basalt_ni_s)
hist(basalt_ni_bb)
hist(basalt_ni_s)
ks.test(basalt_ni_s,basalt_ni_bb)

georoc %>% 
  filter(rock.type=="Basalt") %>%
  filter(tectonic.setting=="Backarc basin"|tectonic.setting=="Seamount") %>% 
  ggplot(aes(NI))+stat_ecdf(aes(color=tectonic.setting))+theme_classic()
###mannwhitney########
slide<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/landslide.csv")
thick<-slide$volume[slide$site=="thick"]
thin<-slide$volume[slide$site=="thin"]
hist(thick)
hist(thin)
wilcox.test(thick,thin)
median(thin)
median(thick)
##q3-4
erosion<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/coastalerosion.csv")
nw<-erosion$erosion[erosion$coast_type=="NW"]
sw<-erosion$erosion[erosion$coast_type=="SW"]
hist(nw)
hist(sw)
wilcox.test(nw,sw)
median(nw)
median(sw)
####q5-6
sed<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/sedOC.csv")
a<-sed$organic_carbon[sed$oxygenation=="anoxic"]
o<-sed$organic_carbon[sed$oxygenation=="oxic"]
hist(a)
hist(o)
wilcox.test(a,o)
median(a)
median(o)
###q7-8
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
r_c_z<-georoc$ZR[georoc$rock.type=="Rhyolite" &georoc$tectonic.setting=="Convergent margin"]
r_i_z<-georoc$ZR[georoc$rock.type=="Rhyolite" &georoc$tectonic.setting=="Intraplate"]
hist(r_c_z)
hist(r_i_z)
wilcox.test(r_c_z,r_i_z)
median(r_c_z)
median(r_i_z)
####Q9-10
estuary<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/estuary.csv")
estuary$ratio<-estuary$organic_carbon/estuary$sulfur
pre<-estuary$ratio[estuary$timeint=="preindustrial"]
modern<-estuary$ratio[estuary$timeint=="modern"]
hist(modern)
hist(pre)
wilcox.test(pre, modern)
############ Kruskal-Wallis test####
branch<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/brachsize.csv")
library(ggplot2)
ggplot(branch,aes(size))+stat_density()+theme_classic()+aes(color=lithology)
kruskal.test(size~lithology,data=branch)
pairwise.wilcox.test(branch$size, branch$lithology)
###q3-4
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
georoc$SRdivCR<-georoc$SR/georoc$CR
georoc_sbst<-subset(georoc,rock.type=="Basalt")
ggplot(georoc_sbst,aes(SRdivCR))+stat_density()+aes(color=tectonic.setting)+theme_classic()
kruskal.test(SRdivCR~tectonic.setting,data=georoc_sbst)
pairwise.wilcox.test(georoc_sbst$SRdivCR,georoc_sbst$tectonic.setting)
georoc_gp<-group_by(georoc_sbst,tectonic.setting)
summarize(georoc_gp,median(SRdivCR))
###q5-6
venus<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/venuscrater.csv")

diameter<-venus$diameter[venus$modification.state=="f1"|venus$modification.state=="f2"|venus$modification.state=="p"]
venus_sbst<-subset(venus,modification.state=="f1"|modification.state=="f2"|modification.state=="p",select = c("name","lat","long","diameter","modification.state","crater.type"))
ggplot(venus_sbst,aes(diameter))+stat_density()+aes(color=modification.state)+theme_classic()
kruskal.test(diameter~modification.state,data=venus_sbst)
##q7-8
beach<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/beachwidth.csv")
ggplot(beach,aes(width))+stat_density()+aes(color=type)+theme_classic()
kruskal.test(width~type,data=beach)
pairwise.wilcox.test(beach$width,beach$type)
##q9-10
recharge<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/recharge.csv")
ggplot(recharge,aes(K))+stat_density()+aes(color=site)+theme_classic()
kruskal.test(K~site,data=recharge)
###### Levenes Test#####
##install.packages("car")
library(car)
###q1-2
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
georoc_sbst<-subset(georoc,rock.type=="Dacite")
georoc_sbst2<-subset(georoc_sbst,tectonic.setting=="Convergent margin"|tectonic.setting=="Intraplate"|tectonic.setting=="Seamount")
ggplot(georoc_sbst2,aes(SIO2))+stat_density()+aes(color=tectonic.setting)+theme_classic()
leveneTest(SIO2~tectonic.setting,data=georoc_sbst2,center=mean)
#Q3-4
georoc_th_sbst<-subset(georoc,rock.type=="Basalt")
georoc_th_sbst2<-subset(georoc_th_sbst, tectonic.setting=="Cont flood Basalt"|tectonic.setting=="Intraplate")
ggplot(georoc_th_sbst2,aes(TH))+stat_density()+aes(color=tectonic.setting)+theme_classic()
leveneTest(TH~tectonic.setting,data=georoc_th_sbst2,center=median)
##############q5-6
georoc_cr_sbst<-subset(georoc,rock.type=="Andesite")
georoc_cr_sbst2<-subset(georoc_cr_sbst, tectonic.setting=="Archean craton"|tectonic.setting=="Convergent margin")
ggplot(georoc_cr_sbst2,aes(CR))+stat_density()+aes(color=tectonic.setting)+theme_classic()
leveneTest(CR~tectonic.setting,data=georoc_cr_sbst2,center=median)
#############q7-8
foram<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/foram.csv")
ggplot(foram,aes(boron_bc))+stat_density()+aes(color=species)+theme_classic()
leveneTest(boron_bc~species,data=foram,center=mean)
#### maybe oceans are contributing to the variability
foram<-group_by(foram,species)
summarize(foram,var(boron_bc))
#####q9-10
ggplot(foram,aes(pH))+stat_density()+aes(color=ocean)+theme_classic()
leveneTest(pH~ocean,data=foram, center=mean)
foram_pH<-group_by(foram,ocean)
summarize(foram_pH,var(pH))
############### Counts and COntingency Tables#####
#Practice using nRow and lenght and table
nrow(georoc[georoc$rock.type=="Basalt",])
length(georoc$rock.type[georoc$rock.type=="Basalt"])
table(georoc$rock.type)
no_basalt <- georoc[georoc$rock.type != "Basalt", ] #create a new data frame where rock type is not equal to basalt
table(no_basalt$rock.type)
table(as.character(no_basalt$rock.type))
####Binomial Tests#####
##install.packages("EMT")
library(EMT)
#q1-2
venus<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/venuscrater.csv")
venus_sbst<-subset(venus,crater.type=="M")
venus_sbst$hemisphere<-ifelse(venus_sbst$lat>0,"N","S")
table(venus_sbst$hemisphere)
binom.test(length(venus_sbst$hemisphere[venus_sbst$hemisphere=="N"]),length(venus_sbst$hemisphere),.5)
####q3-4
binom.test(52,120,0.72)
#####q5-6
obs_counts<-c(38,25,19)
exp_p<-c(.3,.4,.3)
multinomial.test(obs_counts,exp_p)
expected_counts<-exp_p*sum(obs_counts)
count_difference<-obs_counts-expected_counts
count_difference
####q7-8
binom.test(16,23,.5)
###q9-10
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
length(georoc$tectonic.setting)
length(georoc$tectonic.setting[georoc$tectonic.setting=="Archean craton"])
binom.test(189,1000,.15)
####Contingency Table####
#practice
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
table(georoc$tectonic.setting, georoc$rock.type)
#q1-2
obs_cts<-matrix(c(5,3, 1,7),ncol=2)
fisher.test(obs_cts)
#q3-4
obs_cts<-matrix(c(5,22,8,7,21,7,6,28,11,11,14,10),ncol = 4)
row_names<-c("Apatite","Rutile","Zircon")
rownames(obs_cts, do.NULL = TRUE, prefix = "row")
rownames(obs_cts) <- row_names
chisq.test(obs_cts)
#q5-6
obs_cts<-matrix(c(32,24,17,19),ncol = 2)
row_names<-c("America","Europe")
rownames(obs_cts, do.NULL = TRUE, prefix = "row")
rownames(obs_cts) <- row_names
fisher.test(obs_cts)
#q7-8
obs_cts<-matrix(c(2,7,12,3),ncol = 2)
row_names<-c("Tropical Forest","Desert")
rownames(obs_cts, do.NULL = TRUE, prefix = "row")
rownames(obs_cts) <- row_names
fisher.test(obs_cts)
#q9-10
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
georoc_sbst<-subset(georoc, tectonic.setting=="Convergent margin"|tectonic.setting=="Intraplate")
georoc_sbst<-subset(georoc_sbst, rock.type=="Rhyolite")
georoc_sbst$SIO2high<-ifelse(georoc_sbst$SIO2>75,1,0)
georoc_sbst_gp<-group_by(georoc_sbst,tectonic.setting,SIO2high)
summarise(georoc_sbst_gp,length(georoc_sbst_gp$SIO2high))
table(as.character(georoc_sbst$tectonic.setting),georoc_sbst$SIO2high)
obs_cts<-as.matrix(table(as.character(georoc_sbst$tectonic.setting),georoc_sbst$SIO2high))
fisher.test(obs_cts)
#####Correlations####
#q1-2
bay<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/chesapeake.csv")
bay_deep<-subset(bay,Water_depth>10)
ggplot(bay_deep,aes(Organic_carbon,Sulfur))+geom_point()+geom_smooth(method="lm")+theme_classic()
qqnorm(bay_deep$Organic_carbon)
qqline(bay_deep$Organic_carbon)
hist(bay_deep$Organic_carbon)
qqnorm(bay_deep$Sulfur)
qqline(bay_deep$Sulfur)
hist(bay_deep$Sulfur)#both seem normal
cor.test(~Organic_carbon  + Sulfur, data = bay_deep)
#q3-4
foram<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/foram.csv")
ggplot(foram,aes(d13C,d18O))+geom_point()+geom_smooth(method="lm")+theme_classic()
qqnorm(foram$d18O)
qqline(foram$d18O)
hist(foram$d13C)
qqnorm(foram$d13C)
qqline(foram$d13C)
hist(foram$d18O)#NOT NORMAL so 
cor.test(~d13C  + d18O, data = foram, method="kendall")
#q5-6
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
georoc_sbst<-subset(georoc,rock.type=="Andesite")
hist(georoc_sbst$NA2O)
hist(georoc_sbst$AL2O3)
#both are normal PARAMETRIC
ggplot(georoc_sbst,aes(NA2O,AL2O3))+geom_point()+geom_smooth(method="lm")+theme_classic()
cor.test(~NA2O+AL2O3,data=georoc_sbst)
cor.test(~NA2O+AL2O3,data=georoc_sbst, method="spearman")
cor.test(~NA2O+AL2O3,data=georoc_sbst, method="kendall")
#q7-8
georoc<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/georoc.csv")
hist(georoc$CE)
hist(georoc$EU)
#both are non-normal non-PARAMETRIC
ggplot(georoc,aes(CE,EU))+geom_point()+geom_smooth(method="lm")+theme_classic()#outliers appear to be present
ggplot(georoc,aes(rock.type,CE))+geom_boxplot()+theme_classic()#many outliers
boxplot(georoc$EU)
boxplot(georoc$CE)
ggplot(georoc,aes(rock.type, EU))+geom_boxplot()+theme_classic()#many outliers
cor.test(~CE+EU,data=georoc, method="spearman")
cor.test(~CE+EU,data=georoc, method="kendall")
#q9-10
luna<-read.csv("https://people.ucsc.edu/~mclapham/eart125/data/lunarswirl.csv")
hist(luna$mag_field)
hist(luna$albedo)
Boxplot(luna$mag_field)
Boxplot(luna$albedo)
ggplot(luna,aes(mag_field, albedo))+geom_point()+theme_classic()
cor.test(~mag_field+albedo,data=luna,method="kendall")
cor.test(~mag_field+albedo,data=luna,method="spearman")
