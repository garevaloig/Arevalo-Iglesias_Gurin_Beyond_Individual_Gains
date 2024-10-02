##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

####
## SCRIPT 1: DATA PREPARATION
####


library(dplyr)
library(foreign)
library(readxl)
library(forcats)

setwd("D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Data")

ESS18<-read.csv2("ESS9e03_2.csv",sep=",")

## Preparing key variables

# Country
ESS18$cntry<-as.factor(ESS18$cntry)

# Remove countries from analysis
ESS18<-ESS18[c(which(ESS18$cntry=="AT"),which(ESS18$cntry=="BE"),which(ESS18$cntry=="BG"),
               which(ESS18$cntry=="HR"),#which(ESS18$cntry=="CY"), # I eliminate Cyprus cause it lacks key data for the salaries
               which(ESS18$cntry=="CZ"),
               which(ESS18$cntry=="DK"),which(ESS18$cntry=="EE"),which(ESS18$cntry=="FI"),
               which(ESS18$cntry=="FR"),which(ESS18$cntry=="DE"),which(ESS18$cntry=="HU"),
               which(ESS18$cntry=="IE"),which(ESS18$cntry=="IT"),which(ESS18$cntry=="LV"),
               which(ESS18$cntry=="LT"),which(ESS18$cntry=="NL"),which(ESS18$cntry=="PL"),
               which(ESS18$cntry=="PT"),which(ESS18$cntry=="SK"),which(ESS18$cntry=="SI"),
               which(ESS18$cntry=="ES"),which(ESS18$cntry=="SE")),]

ESS18$cntry<-droplevels(ESS18$cntry)


# Age
ESS18$agea<-as.numeric(ESS18$agea)

ESS18$agea_grp<-factor(nrow(ESS18),levels=c("15-24","25-49","50-64","65-79","80 or more"))
ESS18$agea_grp[which(ESS18$agea<25)]="15-24"
ESS18$agea_grp[which(ESS18$agea>=25 & ESS18$agea<=49)]="25-49"
ESS18$agea_grp[which(ESS18$agea>=50 & ESS18$agea<=64)]="50-64"
ESS18$agea_grp[which(ESS18$agea>=65 & ESS18$agea<=79)]="65-79"
ESS18$agea_grp[which(ESS18$agea>=80)]="80 or more"

# Gender
ESS18$gndr<-as.factor(ESS18$gndr)
ESS18$gndr<-recode_factor(ESS18$gndr,
                     "1"="male",
                     "2"="female")

# Preferences
ESS18$gincdif[which(ESS18$gincdif>5)]=NA
ESS18$gincdif_num<-abs(ESS18$gincdif-6) # Turn the variable around so that higher values express stronger support for redistribution
ESS18$gincdif<-as.ordered(ESS18$gincdif_num)

# Marital status
ESS18$maritalb[which(ESS18$maritalb>6)]=NA
ESS18$maritalstatus<-as.factor(ESS18$maritalb)
ESS18$maritalstatus<-recode_factor(ESS18$maritalstatus,
                                   "1"="Legally married",
                                   "2"="In a legally registered civil union",
                                   "3"="Legally separated",
                                   "4"="Legally divorced/Civil union dissolved",
                                   "5"="Widowed/Civil partner died",
                                   "6"="Never married or in civil union")

ESS18$maritalstatus_grp<-recode_factor(ESS18$maritalstatus,
                                   "Legally married"="Legally married or in civil union",
                                   "In a legally registered civil union"="Legally married or in civil union",
                                   "Legally separated"="Legally separated or divorced",
                                   "Legally divorced/Civil union dissolved"="Legally separated or divorced",
                                   "Widowed/Civil partner died"="Widowed/Civil partner died",
                                   "Never married or in civil union"="Never married or in civil union")

ESS18$maritalstatus_bin<-recode_factor(ESS18$maritalstatus,
                                    "Legally separated"="Not married or in civil union",
                                    "Legally divorced/Civil union dissolved"="Not married or in civil union",
                                    "Widowed/Civil partner died"="Not married or in civil union",
                                    "Never married or in civil union"="Not married or in civil union",
                                    "Legally married"="Married or in civil union",
                                    "In a legally registered civil union"="Married or in civil union")

ESS18$divorced_married<-numeric(nrow(ESS18)) # Dummy to compare divorced with married, other categories are NAs
ESS18$divorced_married[which(ESS18$maritalstatus_grp=="Legally married or in civil union")]=0
ESS18$divorced_married[which(ESS18$maritalstatus_grp=="Legally separated or divorced")]=1
ESS18$divorced_married[which(ESS18$maritalstatus_grp=="Widowed/Civil partner died")]=NA
ESS18$divorced_married[which(ESS18$maritalstatus_grp=="Never married or in civil union")]=NA

ESS18$widowed_married<-numeric(nrow(ESS18)) # Dummy to compare widowed with married, other categories are NAs
ESS18$widowed_married[which(ESS18$maritalstatus_grp=="Legally married or in civil union")]=0
ESS18$widowed_married[which(ESS18$maritalstatus_grp=="Legally separated or divorced")]=NA
ESS18$widowed_married[which(ESS18$maritalstatus_grp=="Widowed/Civil partner died")]=1
ESS18$widowed_married[which(ESS18$maritalstatus_grp=="Never married or in civil union")]=NA

ESS18$single_married<-numeric(nrow(ESS18)) # Dummy to compare never married with married, other categories are NAs
ESS18$single_married[which(ESS18$maritalstatus_grp=="Legally married or in civil union")]=0
ESS18$single_married[which(ESS18$maritalstatus_grp=="Legally separated or divorced")]=NA
ESS18$single_married[which(ESS18$maritalstatus_grp=="Widowed/Civil partner died")]=NA
ESS18$single_married[which(ESS18$maritalstatus_grp=="Never married or in civil union")]=1

# Cohabiting partner
ESS18$lwpartner<-factor(nrow(ESS18),levels=c("no","yes"))

for (i in 1:length(ESS18$lwpartner)){
  if (1 %in% c(ESS18$rshipa2[i],ESS18$rshipa3[i],ESS18$rshipa4[i],
               ESS18$rshipa5[i],ESS18$rshipa6[i],ESS18$rshipa7[i],
               ESS18$rshipa8[i],ESS18$rshipa9[i],ESS18$rshipa10[i],
               ESS18$rshipa11[i],ESS18$rshipa12[i],ESS18$rshipa13[i],
               ESS18$rshipa14[i],ESS18$rshipa15[i])){
    ESS18$lwpartner[i]="yes"
  }
  else{
    ESS18$lwpartner[i]="no"
  }
}                       

# Cohabiting (0) vs married (1) couples
ESS18$married_cohabiting<-numeric(nrow(ESS18))
ESS18$married_cohabiting[which(ESS18$lwpartner=="yes" & ESS18$maritalstatus_bin=="Married or in civil union")]=1
ESS18$married_cohabiting[which(ESS18$lwpartner=="yes" & ESS18$maritalstatus_bin=="Not married or in civil union")]=0
ESS18$married_cohabiting[which(ESS18$lwpartner=="no")]=NA
ESS18$married_cohabiting[which(is.na(ESS18$maritalstatus_bin))]=NA

# Gender of partner
rshipaind<-which(colnames(ESS18)=="rshipa2"):which(colnames(ESS18)=="rshipa15")
gndrind<-which(colnames(ESS18)=="gndr2"):which(colnames(ESS18)=="gndr15")

gndrpartner<-numeric(nrow(ESS18))

for (i in 1:length(gndrpartner)){
  if (1 %in% ESS18[i,rshipaind[1:6]]){
  gndrpartner[i]<-ESS18[i,gndrind[1:6]][which(ESS18[i,rshipaind[1:6]]==1)][1,1]
  }
  else{gndrpartner[i]=NA}
}

gndrpartner[which(gndrpartner>2)]=NA

ESS18$gndrpartner<-as.factor(gndrpartner)
ESS18$gndrpartner<-recode_factor(ESS18$gndrpartner,
                          "1"="male",
                          "2"="female")

# Different or same-sex partnership
ESS18$partnership_type<-factor(nrow(ESS18),levels=c("Different-sex","Same-sex"))

ESS18$partnership_type[which(ESS18$gndr=="male" & 
                               ESS18$gndrpartner=="female")]="Different-sex"
ESS18$partnership_type[which(ESS18$gndr=="female" & 
                               ESS18$gndrpartner=="male")]="Different-sex"
ESS18$partnership_type[which(ESS18$gndr=="male" & 
                               ESS18$gndrpartner=="male")]="Same-sex"
ESS18$partnership_type[which(ESS18$gndr=="female" & 
                               ESS18$gndrpartner=="female")]="Same-sex"
ESS18$partnership_type[which(is.na(ESS18$gndrpartner))]=NA

# Number of children (in HH)

ESS18$nchildren<-numeric(nrow(ESS18))

for (i in 1:length(ESS18$nchildren)){
  ESS18$nchildren[i]=length(which(ESS18[i,c("rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7","rshipa8",
                                     "rshipa9","rshipa10","rshipa11","rshipa12","rshipa13","rshipa14","rshipa15")]==2))
}  

ESS18$nchildren_grp<-factor(nrow(ESS18),levels=c("no children","1 child","2 children","3 children or more"),ordered=T)

ESS18$nchildren_grp[which(ESS18$nchildren==0)]="no children"
ESS18$nchildren_grp[which(ESS18$nchildren==1)]="1 child"
ESS18$nchildren_grp[which(ESS18$nchildren==2)]="2 children"
ESS18$nchildren_grp[which(ESS18$nchildren>=3)]="3 children or more"

ESS18$nchildren_grp2<-recode_factor(ESS18$nchildren_grp,"1 child"="1-2 children","2 children"="1-2 children")

# Ages of minor children in HH
ESS18$hhchildage2<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
if (ESS18$rshipa2[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn2[i])<18){
  ESS18$hhchildage2[i]=(ESS18$inwyys[i]-ESS18$yrbrn2[i])}
else{
  ESS18$hhchildage2[i]=9999
}
}
ESS18$hhchildage2[which(ESS18$hhchildage2<0 | ESS18$hhchildage2>=18)]=NA

ESS18$hhchildage3<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa3[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn3[i])<18){
    ESS18$hhchildage3[i]=(ESS18$inwyys[i]-ESS18$yrbrn3[i])}
  else{
    ESS18$hhchildage3[i]=9999
  }
}
ESS18$hhchildage3[which(ESS18$hhchildage3<0 | ESS18$hhchildage3>=18)]=NA

ESS18$hhchildage4<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa4[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn4[i])<18){
    ESS18$hhchildage4[i]=(ESS18$inwyys[i]-ESS18$yrbrn4[i])}
  else{
    ESS18$hhchildage4[i]=9999
  }
}
ESS18$hhchildage4[which(ESS18$hhchildage4<0 | ESS18$hhchildage4>=18)]=NA

ESS18$hhchildage5<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa5[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn5[i])<18){
    ESS18$hhchildage5[i]=(ESS18$inwyys[i]-ESS18$yrbrn5[i])}
  else{
    ESS18$hhchildage5[i]=9999
  }
}
ESS18$hhchildage5[which(ESS18$hhchildage5<0 | ESS18$hhchildage5>=18)]=NA

ESS18$hhchildage6<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa6[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn6[i])<18){
    ESS18$hhchildage6[i]=(ESS18$inwyys[i]-ESS18$yrbrn6[i])}
  else{
    ESS18$hhchildage6[i]=9999
  }
}
ESS18$hhchildage6[which(ESS18$hhchildage6<0 | ESS18$hhchildage6>=18)]=NA

ESS18$rshipa7[which(is.na(ESS18$rshipa7))]=9999
ESS18$yrbrn7[which(is.na(ESS18$yrbrn7))]=9999
ESS18$hhchildage7<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa7[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn7[i])<18){
    ESS18$hhchildage7[i]=(ESS18$inwyys[i]-ESS18$yrbrn7[i])}
  else{
    ESS18$hhchildage7[i]=9999
  }
}
ESS18$hhchildage7[which(ESS18$hhchildage7<0 | ESS18$hhchildage7>=18)]=NA

ESS18$rshipa8[which(is.na(ESS18$rshipa8))]=9999
ESS18$yrbrn8[which(is.na(ESS18$yrbrn8))]=9999
ESS18$hhchildage8<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa8[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn8[i])<18){
    ESS18$hhchildage8[i]=(ESS18$inwyys[i]-ESS18$yrbrn8[i])}
  else{
    ESS18$hhchildage8[i]=9999
  }
}
ESS18$hhchildage8[which(ESS18$hhchildage8<0 | ESS18$hhchildage8>=18)]=NA

ESS18$rshipa9[which(is.na(ESS18$rshipa9))]=9999
ESS18$yrbrn9[which(is.na(ESS18$yrbrn9))]=9999
ESS18$hhchildage9<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa9[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn9[i])<18){
    ESS18$hhchildage9[i]=(ESS18$inwyys[i]-ESS18$yrbrn9[i])}
  else{
    ESS18$hhchildage9[i]=9999
  }
}
ESS18$hhchildage9[which(ESS18$hhchildage9<0 | ESS18$hhchildage9>=18)]=NA

ESS18$rshipa10[which(is.na(ESS18$rshipa10))]=9999
ESS18$yrbrn10[which(is.na(ESS18$yrbrn10))]=9999
ESS18$hhchildage10<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa10[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn10[i])<18){
    ESS18$hhchildage10[i]=(ESS18$inwyys[i]-ESS18$yrbrn10[i])}
  else{
    ESS18$hhchildage10[i]=9999
  }
}
ESS18$hhchildage10[which(ESS18$hhchildage10<0 | ESS18$hhchildage10>=18)]=NA

ESS18$rshipa11[which(is.na(ESS18$rshipa11))]=9999
ESS18$yrbrn11[which(is.na(ESS18$yrbrn11))]=9999
ESS18$hhchildage11<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa11[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn11[i])<18){
    ESS18$hhchildage11[i]=(ESS18$inwyys[i]-ESS18$yrbrn11[i])}
  else{
    ESS18$hhchildage11[i]=9999
  }
}
ESS18$hhchildage11[which(ESS18$hhchildage11<0 | ESS18$hhchildage11>=18)]=NA

ESS18$rshipa12[which(is.na(ESS18$rshipa12))]=9999
ESS18$yrbrn12[which(is.na(ESS18$yrbrn12))]=9999
ESS18$hhchildage12<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  if (ESS18$rshipa12[i]==2 & (ESS18$inwyys[i]-ESS18$yrbrn12[i])<18){
    ESS18$hhchildage12[i]=(ESS18$inwyys[i]-ESS18$yrbrn12[i])}
  else{
    ESS18$hhchildage12[i]=9999
  }
}
ESS18$hhchildage12[which(ESS18$hhchildage12<0 | ESS18$hhchildage12>=18)]=NA

# Number of minor children in HH
ESS18$nmchildren<-numeric(nrow(ESS18))

for (i in 1:nrow(ESS18)){
  ESS18$nmchildren[i]<-11-length(which(is.na(c(ESS18$hhchildage2[i],ESS18$hhchildage3[i],ESS18$hhchildage4[i],
                                               ESS18$hhchildage5[i],ESS18$hhchildage6[i],ESS18$hhchildage7[i],
                                               ESS18$hhchildage8[i],ESS18$hhchildage9[i],ESS18$hhchildage10[i],
                                               ESS18$hhchildage11[i],ESS18$hhchildage12[i]))))
}

ESS18$nmchildren_grp<-factor(nrow(ESS18),levels=c("no children","1 child","2 children","3 children or more"),ordered=T)

ESS18$nmchildren_grp[which(ESS18$nmchildren==0)]="no children"
ESS18$nmchildren_grp[which(ESS18$nmchildren==1)]="1 child"
ESS18$nmchildren_grp[which(ESS18$nmchildren==2)]="2 children"
ESS18$nmchildren_grp[which(ESS18$nmchildren>=3)]="3 children or more"

ESS18$nmchildren_grp2<-recode_factor(ESS18$nmchildren_grp,"1 child"="1-2 children","2 children"="1-2 children")
ESS18$nmchildren_grp2<-factor(ESS18$nmchildren_grp2,levels=c("no children","1-2 children","3 children or more"))

ESS18$hasmkids_bin<-recode_factor(ESS18$nmchildren_grp,"no children"="no","1 child"="yes","2 children"="yes","3 children or more"="yes")

# Combinations of parenthood and partnership
ESS18$parentpartner<-factor(nrow(ESS18),levels=c("Single no children","Single parent","Couple no children","Couple parents"))
ESS18$parentpartner[which(ESS18$lwpartner=="no" & ESS18$nmchildren==0)]="Single no children"
ESS18$parentpartner[which(ESS18$lwpartner=="no" & ESS18$nmchildren>0)]="Single parent"
ESS18$parentpartner[which(ESS18$lwpartner=="yes" & ESS18$nmchildren==0)]="Couple no children"
ESS18$parentpartner[which(ESS18$lwpartner=="yes" & ESS18$nmchildren>0)]="Couple parents"

# A dummy to compare single parents with parents in partnership (NAs for respondents without children)
ESS18$singleparent<-numeric(nrow(ESS18))
ESS18$singleparent[which(ESS18$lwpartner=="no" & ESS18$nmchildren==0)]=NA
ESS18$singleparent[which(ESS18$lwpartner=="no" & ESS18$nmchildren>0)]=1
ESS18$singleparent[which(ESS18$lwpartner=="yes" & ESS18$nmchildren==0)]=NA
ESS18$singleparent[which(ESS18$lwpartner=="yes" & ESS18$nmchildren>0)]=0


# HH income
ESS18$hinctnta[which(ESS18$hinctnta>=77)]=NA

ESS18$hinctnta_factor<-as.factor(ESS18$hinctnta)

ESS18$hinctnta_grp<-factor(nrow(ESS18),levels=c("Low","Middle","High"))
ESS18$hinctnta_grp[which(ESS18$hinctnta<=3)]="Low"
ESS18$hinctnta_grp[which(ESS18$hinctnta>=4 & ESS18$hinctnta<=7)]="Middle"
ESS18$hinctnta_grp[which(ESS18$hinctnta>=8)]="High"

# HH category (distribution of paid work among partners -> single-earner, main-earner, double-earner, no-earner)
# Not needed (or should we model it?)

# Occupational status
ESS18$ocustat<-factor(nrow(ESS18),levels=c("Paid employment","NEET","In education"))

ESS18$ocustat[which(ESS18$mnactic==1)]="Paid employment"
ESS18$ocustat[which(ESS18$mnactic>=3 & ESS18$mnactic<=9)]="NEET"
ESS18$ocustat[which(ESS18$mnactic==2)]="In education"
ESS18$ocustat[which(ESS18$mnactic>9)]=NA

# Single individual (0) vs individuals with family (1)
ESS18$single_family<-numeric(nrow(ESS18))
ESS18$single_family[which(ESS18$lwpartner=="yes")]=1
ESS18$single_family[which(ESS18$nchildren>=1)]=1
ESS18$single_family[which(ESS18$lwpartner=="no" & ESS18$nchildren==0)]=0

############################################

#
# ESTIMATING INCOME FOR EACH PARTNER: NECESSARY VARIABLES
#

## EUROSTAT data for the imputation of average earnings

Earnings<-read.csv2("Eurostat_EarningsData2.csv",sep=",")

Earnings$isco08<-as.factor(Earnings$isco08)

Earnings$worktime<-as.factor(Earnings$worktime)

Earnings$age<-as.factor(Earnings$age)

Earnings$sex<-as.factor(Earnings$sex)
Earnings$sex=recode_factor(Earnings$sex,"F"="female","M"="male")
Earnings<-Earnings[-which(Earnings$sex=="T"),]
Earnings$sex<-droplevels(Earnings$sex)

Earnings$geo<-as.factor(Earnings$geo)

Earnings$OBS_VALUE[which(Earnings$OBS_VALUE=="")]=NA
Earnings$OBS_VALUE<-as.numeric(Earnings$OBS_VALUE)

### Obtaining the same factor in the ESS data

# 1) for respondent:

# ISCO08
ESS18$isco08_postcoded<-factor(nrow(ESS18),levels=levels(Earnings$isco08))

ESS18$isco08_postcoded[which(ESS18$isco08<=999)]="OC0"
ESS18$isco08_postcoded[which(ESS18$isco08>=1000 & ESS18$isco08<2000)]="OC1"
ESS18$isco08_postcoded[which(ESS18$isco08>=2000 & ESS18$isco08<3000)]="OC2"
ESS18$isco08_postcoded[which(ESS18$isco08>=3000 & ESS18$isco08<4000)]="OC3"
ESS18$isco08_postcoded[which(ESS18$isco08>=4000 & ESS18$isco08<5000)]="OC4"
ESS18$isco08_postcoded[which(ESS18$isco08>=5000 & ESS18$isco08<6000)]="OC5"
ESS18$isco08_postcoded[which(ESS18$isco08>=6000 & ESS18$isco08<7000)]="OC6"
ESS18$isco08_postcoded[which(ESS18$isco08>=7000 & ESS18$isco08<8000)]="OC7"
ESS18$isco08_postcoded[which(ESS18$isco08>=8000 & ESS18$isco08<9000)]="OC8"
ESS18$isco08_postcoded[which(ESS18$isco08>=9000 & ESS18$isco08<9999)]="OC9"
ESS18$isco08_postcoded[which(ESS18$isco08>9999)]=NA

ESS18$isco08_postcoded[which(ESS18$isco08>=6000 & ESS18$isco08<7000 & ESS18$cntry=="AT")]="OC6-8" #No OC6 in Austria, use instead OC6-8
ESS18$isco08_postcoded[which(ESS18$isco08>=6000 & ESS18$isco08<7000 & ESS18$cntry=="BE")]="OC6-8" # Same for Belgium

# Full-time or part-time
ESS18$wkhtot[which(ESS18$wktot>168)]=NA # Note that 168 is the highest acceptable value according to the codebook
ESS18$wkhtot_grp<-factor(nrow(ESS18),levels=c("FT","PT","TOTAL"))
ESS18$wkhtot_grp[which(ESS18$wkhtot<35)]="PT"
ESS18$wkhtot_grp[which(ESS18$wkhtot>=35)]="FT"
ESS18$wkhtot_grp[which(is.na(ESS18$wkhtot))]="TOTAL" # When there is no info I am just using the total (for analyses code as NA)

Earnings[1686,]=Earnings[967,];Earnings[1686,"worktime"]="PT" # There is no value for PT women in OC6 in LT so I am using the value for total WH

Earnings=Earnings[-which(Earnings$geo=="CY"),];Earnings$geo=droplevels(Earnings$geo) # Get rid of Cyprus

ESS18$salary_est<-numeric(nrow(ESS18))

for (i in 1:nrow(ESS18)){
  if (ESS18$pdwrk[i]==0){
    ESS18$salary_est[i]=0} 
  if (ESS18$pdwrk[i]==1 & is.na(ESS18$isco08_postcoded[i])){
      ESS18$salary_est[i]=NA}
    else{
    if (ESS18$pdwrk[i]==1 & ESS18$isco08_postcoded[i]=="OC0"){
    ESS18$salary_est[i]=NA}
    if (ESS18$pdwrk[i]==1 & ESS18$isco08_postcoded[i]!="OC0"){  
    ESS18$salary_est[i]=Earnings$OBS_VALUE[which(Earnings$isco08==ESS18$isco08_postcoded[i] & Earnings$worktime==ESS18$wkhtot_grp[i] &
                                                   Earnings$sex==ESS18$gndr[i] & Earnings$geo==ESS18$cntry[i])]}
  }}

# Monthly salary:
ESS18$msalary_est<-ESS18$salary_est*ESS18$wkhtot*52/12

# Monthly salary in categories: average, half the average or double the average
ESS18$averagesalary<-numeric(nrow(ESS18))
for (i in 1:nrow(ESS18)){
  ESS18$averagesalary[i]=mean(ESS18$msalary_est[which(ESS18$cntry==ESS18$cntry[i])],na.rm=T)
}

ESS18$msalary_grp<-factor(nrow(ESS18),levels=c("No income","Half the average","Average","Double the average"))
for (i in 1:nrow(ESS18)){
  if (is.na(ESS18$msalary_est[i])){
    ESS18$msalary_grp[i]=NA
  }
  else{
    if (ESS18$msalary_est[i]<=(ESS18$averagesalary[i]*0.5)){
      ESS18$msalary_grp[i]="Half the average"
    }
    if (ESS18$msalary_est[i]>(ESS18$averagesalary[i]*0.5) & ESS18$msalary_est[i]<(ESS18$averagesalary[i]*2)){
      ESS18$msalary_grp[i]="Average"
    }
    if (ESS18$msalary_est[i]>=(ESS18$averagesalary[i]*2)){
      ESS18$msalary_grp[i]="Double the average"
    }
    if (ESS18$msalary_est[i]==0){
      ESS18$msalary_grp[i]="No income"
    }
  }
}

# Do the same for the partner:

# ISCO08
ESS18$isco08p_postcoded<-factor(nrow(ESS18),levels=levels(Earnings$isco08))

ESS18$isco08p_postcoded[which(ESS18$isco08p<=999)]="OC0"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=1000 & ESS18$isco08p<2000)]="OC1"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=2000 & ESS18$isco08p<3000)]="OC2"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=3000 & ESS18$isco08p<4000)]="OC3"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=4000 & ESS18$isco08p<5000)]="OC4"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=5000 & ESS18$isco08p<6000)]="OC5"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=6000 & ESS18$isco08p<7000)]="OC6"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=7000 & ESS18$isco08p<8000)]="OC7"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=8000 & ESS18$isco08p<9000)]="OC8"
ESS18$isco08p_postcoded[which(ESS18$isco08p>=9000 & ESS18$isco08p<9999)]="OC9"
ESS18$isco08p_postcoded[which(ESS18$isco08p>9999)]=NA

ESS18$isco08p_postcoded[which(ESS18$isco08p>=6000 & ESS18$isco08p<7000 & ESS18$cntry=="AT")]="OC6-8" #No OC6 in Austria, use instead OC6-8
ESS18$isco08p_postcoded[which(ESS18$isco08p>=6000 & ESS18$isco08p<7000 & ESS18$cntry=="BE")]="OC6-8" # Same for Belgium

# Full-time or part-time
ESS18$wkhtotp[which(ESS18$wktotp>168)]=NA # Note that 168 is the highest acceptable value according to the codebook
ESS18$wkhtotp_grp<-factor(nrow(ESS18),levels=c("FT","PT","TOTAL"))
ESS18$wkhtotp_grp[which(ESS18$wkhtotp<35)]="PT"
ESS18$wkhtotp_grp[which(ESS18$wkhtotp>=35)]="FT"
ESS18$wkhtotp_grp[which(is.na(ESS18$wkhtotp))]="TOTAL" # When there is no info I am just using the total (for analyses code as NA)

ESS18<-ESS18[-which(ESS18$lwpartner=="yes" & is.na(ESS18$gndrpartner)),]

ESS18$salaryp_est<-numeric(nrow(ESS18))

for (i in 1:nrow(ESS18)){
  if (ESS18$lwpartner[i]=="no"){
    ESS18$salaryp_est[i]=NA}
      else{
        if (ESS18$pdwrkp[i]==0){
            ESS18$salaryp_est[i]=0} 
        if (ESS18$pdwrkp[i]==1 & is.na(ESS18$isco08p_postcoded[i])){
          ESS18$salaryp_est[i]=NA}
        else{
          if (ESS18$pdwrkp[i]==1 & ESS18$isco08p_postcoded[i]=="OC0"){
              ESS18$salaryp_est[i]=NA}
          if (ESS18$pdwrkp[i]==1 & ESS18$isco08p_postcoded[i]!="OC0"){  
              ESS18$salaryp_est[i]=Earnings$OBS_VALUE[which(Earnings$isco08==ESS18$isco08p_postcoded[i] & 
                                                          Earnings$worktime==ESS18$wkhtotp_grp[i] &
                                                          Earnings$sex==ESS18$gndrpartner[i] & Earnings$geo==ESS18$cntry[i])]}
    }}}

# Monthly salary:
ESS18$msalaryp_est<-ESS18$salaryp_est*ESS18$wkhtotp*52/12

# Monthly salary in categories: average, half the average or double the average
ESS18$msalaryp_grp<-factor(nrow(ESS18),levels=c("No income","Half the average","Average","Double the average"))
for (i in 1:nrow(ESS18)){
  if (is.na(ESS18$msalaryp_est[i])){
    ESS18$msalaryp_grp[i]=NA
  }
  else{
    if (ESS18$msalaryp_est[i]<=(ESS18$averagesalary[i]*0.5)){
      ESS18$msalaryp_grp[i]="Half the average"
    }
    if (ESS18$msalaryp_est[i]>(ESS18$averagesalary[i]*0.5) & ESS18$msalaryp_est[i]<(ESS18$averagesalary[i]*2)){
      ESS18$msalaryp_grp[i]="Average"
    }
    if (ESS18$msalaryp_est[i]>=(ESS18$averagesalary[i]*2)){
      ESS18$msalaryp_grp[i]="Double the average"
    }
    if (ESS18$msalaryp_est[i]==0){
      ESS18$msalaryp_grp[i]="No income"
    }
  }
}

# Distribution of market earnings among partners
ESS18$earnings_dist<-factor(nrow(ESS18),levels=c("No earner","Single earner","Supplementary earner","Double earner"))

ESS18$earnings_dist[which(ESS18$lwpartner=="no" & is.na(ESS18$msalary_est))]=NA
ESS18$earnings_dist[which(ESS18$lwpartner=="no" & ESS18$msalary_est==0)]="No earner"
ESS18$earnings_dist[which(ESS18$lwpartner=="no" & ESS18$msalary_est>0)]="Single earner"

ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & is.na(ESS18$msalary_est))]=NA
ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & is.na(ESS18$msalaryp_est))]=NA

ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & ESS18$msalary_grp=="No income" & ESS18$msalaryp_grp=="No income")]="No earner"

ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & ESS18$msalary_grp!="No income" & ESS18$msalaryp_grp=="No income")]="Single earner"
ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & ESS18$msalary_grp=="No income" & ESS18$msalaryp_grp!="No income")]="Single earner"

ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & ESS18$msalary_grp!="No income" & !is.na(ESS18$msalary_grp) & 
                            ESS18$msalaryp_grp!="No income" & !is.na(ESS18$msalaryp_grp) & 
                            ESS18$msalary_grp!=ESS18$msalaryp_grp)]="Supplementary earner"

ESS18$earnings_dist[which(ESS18$lwpartner=="yes" & ESS18$msalary_grp!="No income" & !is.na(ESS18$msalary_grp) & 
                          ESS18$msalary_grp==ESS18$msalaryp_grp)]="Double earner"


# Double-earner (0) vs single-earner (1) couples
ESS18$double_single<-numeric(nrow(ESS18))
ESS18$double_single[which(ESS18$earnings_dist=="Single-earner")]=1
ESS18$double_single[which(ESS18$earnings_dist=="Supplementary-earner")]=NA
ESS18$double_single[which(ESS18$earnings_dist=="No earner")]=NA
ESS18$double_single[which(ESS18$earnings_dist=="Double-earner")]=0
ESS18$double_single[which(is.na(ESS18$earnings_dist))]=NA

# Double-earner (0) vs Supplementary-earner (1) couples
ESS18$double_sup<-numeric(nrow(ESS18))
ESS18$double_sup[which(ESS18$earnings_dist=="Single-earner")]=NA
ESS18$double_sup[which(ESS18$earnings_dist=="No earner")]=NA
ESS18$double_sup[which(ESS18$earnings_dist=="Supplementary-earner")]=1
ESS18$double_sup[which(ESS18$earnings_dist=="Double-earner")]=0
ESS18$double_sup[which(is.na(ESS18$earnings_dist))]=NA

############################################

## Other relevant variables:

# Education
ESS18$high_second_edu<-factor(nrow(ESS18),levels=c("No","Yes"))

ESS18$high_second_edu[which(ESS18$eisced<=3)]="No"
ESS18$high_second_edu[which(ESS18$eisced>=4 & ESS18$eisced<=7)]="Yes"
ESS18$high_second_edu[which(ESS18$eisced>=77)]=NA

# Religiosity
ESS18$rlgdgr[which(ESS18$rlgdgr>=77)]=NA

# Support for gay rights
ESS18$freehms[which(ESS18$freehms>5)]<-NA
ESS18$freehms<-abs(ESS18$freehms-6)

# Weights
ESS18$dweight<-as.numeric(ESS18$dweight)
ESS18$anweight<-as.numeric(ESS18$anweight)

############################################

#
# HHoT DATA
#

HHoT_list<-lapply(file.path("D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Data/Country data EUROMOD",
                          c("AT data.xlsx","BE data.xlsx","BG data.xlsx",#"CY data.xlsx",
                            "CZ data.xlsx",
                            "DE data.xlsx","DK data.xlsx","EE data.xlsx","ES data.xlsx","FI data.xlsx","FR data.xlsx",
                            "HR data.xlsx","HU data.xlsx","IE data.xlsx","IT data.xlsx","LT data.xlsx","LV data.xlsx",
                            "NL data.xlsx","PL data.xlsx","PT data.xlsx","SE data.xlsx","SI data.xlsx","SK data.xlsx")),read_xlsx)

HHoT_list<-setNames(HHoT_list,c("AT","BE","BG",#"CY", # I eliminated Cyprus due to the lack of labour market variables
                                "CZ","DE","DK","EE","ES","FI","FR","HR","HU","IE","IT","LT","LV","NL","PL","PT",
                "SE","SI","SK"))

# Correct difference in names for the last column 
for (i in 1:length(HHoT_list)){
  colnames(HHoT_list[[i]])[ncol(HHoT_list[[i]])]="NDI partner"
}

# Adding rows for the reference points (Single Individuals)
for (i in 1:length(HHoT_list)){
HHoT_list[[i]]<-rbind(HHoT_list[[i]],c("SI","Single","0 children","Low income (50% average)","Single earner",
                                          HHoT_list[[i]]$"NDI Reference point"[1],HHoT_list[[i]]$"NDI Reference point"[1],
                                          0,"0%",HHoT_list[[i]]$"NDI Reference point"[1],0),
                      c("SI","Single","0 children","Middle income (100% average)","Single earner",
                        HHoT_list[[i]]$"NDI Reference point"[2],HHoT_list[[i]]$"NDI Reference point"[2],
                        0,"0%",HHoT_list[[i]]$"NDI Reference point"[2],0),
                      c("SI","Single","0 children","High income (200% average)","Single earner",
                        HHoT_list[[i]]$"NDI Reference point"[3],HHoT_list[[i]]$"NDI Reference point"[3],
                        0,"0%",HHoT_list[[i]]$"NDI Reference point"[3],0)
                      )}

HHoT<-as.data.frame(do.call(rbind,HHoT_list))
rown<-nrow(HHoT_list[[1]])
HHoT$Country<-c(rep("AT",rown),rep("BE",rown),rep("BG",rown),#rep("CY",rown),
                rep("CZ",rown),rep("DE",rown),rep("DK",rown),rep("EE",rown),
                rep("ES",rown),rep("FI",rown),rep("FR",rown),rep("HR",rown),rep("HU",rown),rep("IE",rown),rep("IT",rown),rep("LT",rown),
                rep("LV",rown),rep("NL",rown),rep("PL",rown),rep("PT",rown),rep("SE",rown),rep("SI",rown),rep("SK",rown))

HHoT$`NDI First adult`<-as.numeric(HHoT$`NDI First adult`)
HHoT$`NDI Reference point`<-as.numeric(HHoT$`NDI Reference point`)
HHoT$`NDI family form`<-as.numeric(HHoT$`NDI family form`)
HHoT$`NDI partner`<-as.numeric(HHoT$`NDI partner`)
HHoT$`Diff`<-as.numeric(HHoT$`Diff`)

HHoT$Diff_Eur<-HHoT$Diff
HHoT$Diff_Eur[which(HHoT$Country=="BG")]=HHoT$Diff_Eur[which(HHoT$Country=="BG")]*0.51
HHoT$Diff_Eur[which(HHoT$Country=="CZ")]=HHoT$Diff_Eur[which(HHoT$Country=="CZ")]*0.04
HHoT$Diff_Eur[which(HHoT$Country=="DK")]=HHoT$Diff_Eur[which(HHoT$Country=="DK")]*0.13
HHoT$Diff_Eur[which(HHoT$Country=="HU")]=HHoT$Diff_Eur[which(HHoT$Country=="HU")]*0.003
HHoT$Diff_Eur[which(HHoT$Country=="PL")]=HHoT$Diff_Eur[which(HHoT$Country=="PL")]*0.23
HHoT$Diff_Eur[which(HHoT$Country=="SE")]=HHoT$Diff_Eur[which(HHoT$Country=="SE")]*0.10

HHoT$NDI_RE_Eur<-HHoT$"NDI First adult"
HHoT$NDI_RE_Eur[which(HHoT$Country=="BG")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="BG")]*0.51
HHoT$NDI_RE_Eur[which(HHoT$Country=="CZ")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="CZ")]*0.04
HHoT$NDI_RE_Eur[which(HHoT$Country=="DK")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="DK")]*0.13
HHoT$NDI_RE_Eur[which(HHoT$Country=="HU")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="HU")]*0.003
HHoT$NDI_RE_Eur[which(HHoT$Country=="PL")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="PL")]*0.23
HHoT$NDI_RE_Eur[which(HHoT$Country=="SE")]=HHoT$NDI_RE_Eur[which(HHoT$Country=="SE")]*0.10

HHoT$NDI_RP_Eur<-HHoT$"NDI Reference point"
HHoT$NDI_RP_Eur[which(HHoT$Country=="BG")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="BG")]*0.51
HHoT$NDI_RP_Eur[which(HHoT$Country=="CZ")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="CZ")]*0.04
HHoT$NDI_RP_Eur[which(HHoT$Country=="DK")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="DK")]*0.13
HHoT$NDI_RP_Eur[which(HHoT$Country=="HU")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="HU")]*0.003
HHoT$NDI_RP_Eur[which(HHoT$Country=="PL")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="PL")]*0.23
HHoT$NDI_RP_Eur[which(HHoT$Country=="SE")]=HHoT$NDI_RP_Eur[which(HHoT$Country=="SE")]*0.10

HHoT$NDI_FF_Eur<-HHoT$"NDI family form"
HHoT$NDI_FF_Eur[which(HHoT$Country=="BG")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="BG")]*0.51
HHoT$NDI_FF_Eur[which(HHoT$Country=="CZ")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="CZ")]*0.04
HHoT$NDI_FF_Eur[which(HHoT$Country=="DK")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="DK")]*0.13
HHoT$NDI_FF_Eur[which(HHoT$Country=="HU")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="HU")]*0.003
HHoT$NDI_FF_Eur[which(HHoT$Country=="PL")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="PL")]*0.23
HHoT$NDI_FF_Eur[which(HHoT$Country=="SE")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="SE")]*0.10

HHoT$NDI_Partner_Eur<-HHoT$"NDI partner"
HHoT$NDI_Partner_Eur[which(HHoT$Country=="BG")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="BG")]*0.51
HHoT$NDI_Partner_Eur[which(HHoT$Country=="CZ")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="CZ")]*0.04
HHoT$NDI_Partner_Eur[which(HHoT$Country=="DK")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="DK")]*0.13
HHoT$NDI_Partner_Eur[which(HHoT$Country=="HU")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="HU")]*0.003
HHoT$NDI_Partner_Eur[which(HHoT$Country=="PL")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="PL")]*0.23
HHoT$NDI_Partner_Eur[which(HHoT$Country=="SE")]=HHoT$NDI_Partner_Eur[which(HHoT$Country=="SE")]*0.10

HHoT$PPP<-numeric(nrow(HHoT))
HHoT$PPP[which(HHoT$Country=="AT")]=1.122
HHoT$PPP[which(HHoT$Country=="BE")]=1.147
HHoT$PPP[which(HHoT$Country=="BG")]=0.519
HHoT$PPP[which(HHoT$Country=="CZ")]=0.74
HHoT$PPP[which(HHoT$Country=="DK")]=1.414
HHoT$PPP[which(HHoT$Country=="DE")]=1.058
HHoT$PPP[which(HHoT$Country=="EE")]=0.83
HHoT$PPP[which(HHoT$Country=="IE")]=1.342
HHoT$PPP[which(HHoT$Country=="ES")]=0.964
HHoT$PPP[which(HHoT$Country=="FR")]=1.139
HHoT$PPP[which(HHoT$Country=="HR")]=0.706
HHoT$PPP[which(HHoT$Country=="IT")]=1.033
HHoT$PPP[which(HHoT$Country=="LV")]=0.767
HHoT$PPP[which(HHoT$Country=="LT")]=0.675
HHoT$PPP[which(HHoT$Country=="HU")]=0.66
HHoT$PPP[which(HHoT$Country=="NL")]=1.144
HHoT$PPP[which(HHoT$Country=="PL")]=0.596
HHoT$PPP[which(HHoT$Country=="PT")]=0.882
HHoT$PPP[which(HHoT$Country=="SI")]=0.871
HHoT$PPP[which(HHoT$Country=="SK")]=0.846
HHoT$PPP[which(HHoT$Country=="FI")]=1.257
HHoT$PPP[which(HHoT$Country=="SE")]=1.251

HHoT$Diff_PPS<-HHoT$Diff_Eur/HHoT$PPP
HHoT$NDI_RE_PPS<-HHoT$NDI_RE_Eur/HHoT$PPP
HHoT$NDI_RP_PPS<-HHoT$NDI_RP_Eur/HHoT$PPP
HHoT$NDI_FF_PPS<-HHoT$NDI_FF_Eur/HHoT$PPP
HHoT$NDI_Partner_PPS<-HHoT$NDI_Partner_Eur/HHoT$PPP

####
# Imputing HHoT data in the ESS dataset 
####

HHoT$Country<-as.factor(HHoT$Country)

HHoT$`PARTNERSHIP STATUS`<-factor(HHoT$`PARTNERSHIP STATUS`,levels=c("Single Parent","Couples","SI"))
ESS18$PARTNERSHIP_STATUS<-recode_factor(ESS18$lwpartner,"no"="Single Parent","yes"="Couples")
levels(ESS18$PARTNERSHIP_STATUS)=c("Single Parent","Couples","SI")
ESS18$PARTNERSHIP_STATUS[which(ESS18$lwpartner=="no" & ESS18$nmchildren_grp2=="no children")]="SI"

HHoT$`MARITAL STATUS`<-factor(HHoT$`MARITAL STATUS`,levels=c("Single","Divorced","Widowed","Cohabiting","Married/legal partnership"))
ESS18$MARITAL_STATUS<-factor(nrow(ESS18),levels=c("Single","Divorced","Widowed","Cohabiting","Married/legal partnership"))
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="Single Parent" & ESS18$maritalstatus_grp=="Never married or in civil union")]="Single"
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="Single Parent" & ESS18$maritalstatus_grp=="Legally separated or divorced")]="Divorced"
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="Single Parent" & ESS18$maritalstatus_grp=="Widowed/Civil partner died")]="Widowed"
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="Couples" & 
                     ESS18$maritalstatus_grp=="Legally married or in civil union")]="Married/legal partnership"
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="Couples" & 
                             ESS18$maritalstatus_grp!="Legally married or in civil union")]="Cohabiting"
ESS18$MARITAL_STATUS[is.na(ESS18$maritalstatus_grp)]=NA
ESS18$MARITAL_STATUS[which(ESS18$PARTNERSHIP_STATUS=="SI")]="Single"

HHoT$`NUMBER OF (MINOR) CHILDREN`<-as.factor(HHoT$`NUMBER OF (MINOR) CHILDREN`)
ESS18$NUMBER_CHILDREN<-recode_factor(ESS18$nmchildren_grp2,"no children"="0 children","1-2 children"="2 children","3 children or more"="3 children")

HHoT$`HH INCOME`<-factor(HHoT$`HH INCOME`,levels=c("Low income (50% average)","Middle income (100% average)","High income (200% average)"))
HHoT$`HH INCOME`<-recode_factor(HHoT$`HH INCOME`,"Low income (50% average)"="Low","Middle income (100% average)"="Middle",
                                "High income (200% average)"="High")                       
ESS18$HH_INCOME<-ESS18$hinctnta_grp

HHoT$`DISTRIBUTION OF PAID WORK`<-factor(HHoT$`DISTRIBUTION OF PAID WORK`,levels=c("Single earner","Supplementary earner","Dual earner"))
ESS18$D_PAID_WORK<-recode_factor(ESS18$earnings_dist,"Single-earner"="Single earner","Supplementary-earner"="Supplementary earner",
                                 "Double-earner"="Dual earner")

ESS18$GROUPS<-fct_cross(ESS18$cntry,ESS18$PARTNERSHIP_STATUS,ESS18$MARITAL_STATUS,ESS18$NUMBER_CHILDREN,ESS18$HH_INCOME,
                        ESS18$D_PAID_WORK,keep_empty=T) 
HHoT$GROUPS<-fct_cross(HHoT$Country,HHoT$`PARTNERSHIP STATUS`,HHoT$`MARITAL STATUS`,HHoT$`NUMBER OF (MINOR) CHILDREN`,
                       HHoT$`HH INCOME`,HHoT$`DISTRIBUTION OF PAID WORK`,keep_empty=T)

data_p3<-merge(ESS18,HHoT[,c("GROUPS","NDI First adult","NDI_RE_Eur","NDI_RE_PPS","NDI Reference point","NDI_RP_Eur","NDI_RP_PPS",
                             "Diff","Diff_Eur","Diff_PPS","Diff in %","NDI family form","NDI_FF_Eur","NDI_FF_PPS",
                             "NDI partner","NDI_Partner_Eur","NDI_Partner_PPS")],by="GROUPS",all.x=T)


data_p3$Diff_pc<-as.numeric(paste(data_p3$`Diff in %`))
data_p3$Diff_pc[which(data_p3$`Diff in %`=="0%")]=0

data_p3$Diff_ths<-data_p3$Diff/1000
data_p3$Diff_Eur_ths<-data_p3$Diff_Eur/1000
data_p3$Diff_PPS_ths<-data_p3$Diff_PPS/1000



############################################

save(ESS18,file="ESS18_Analysis.RData")
save(list=ls(),file="DataStudy3.RData")

load("DataStudy3.RData")


