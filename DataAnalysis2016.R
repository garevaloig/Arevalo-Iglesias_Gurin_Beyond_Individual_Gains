##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

####
## SCRIPT 3: REGRESSION MODELS
####

library(lme4)
library(lmerTest)
library(ggplot2)
library(ordinal)
library(texreg)
library(sjPlot)
library(ragg)
library(mice)
library(viridis)
library(broom.mixed)
library(lmerTest)
library(dplyr)
library(MASS)
library(brant)

setwd("D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Scripts/2016Data")
load("DataStudy3_2016.RData")

#############################################################

# Creating a function to get the matrixes to plot each model
plot_matrix<-function(x){
  fixed_effect<-fixef(x)[2]
  coef_table<-coef(x)[[1]]
  var_slopes<-attr(ranef(x)[[1]],"postVar")[2,2,]
  se_slopes<-sqrt(var_slopes)
  ci_lower<-coef_table[,2]-1.96*se_slopes
  ci_upper<-coef_table[,2]+1.96*se_slopes
  plot_data <- data.frame(
    country = rownames(ranef(x)[[1]]),
    total_effect = coef_table[,2],
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  return(plot_data)
}

#############################################################

###
# CREATE A SAMPLE FOR ANALYSIS (excludes all missings in variables except Household Income and Redistributive Outputs)
###

data_p3$workingparents<-numeric(nrow(data_p3))
data_p3$workingparents[which(data_p3$hasmkids_bin=="yes" & data_p3$earn_dist2!="No earner")]=1

# We explore all variables in the analysis
summary(data_p3$gincdif) # Preferences for redistribution: 435NAs
summary(data_p3$wrkprbf) # Preferences for work-family balance: 1903 NAs
summary(data_p3$Diff_PPS_ths) # Family-related redistribution: 9537 NAs (add also full version and NA indicator)
summary(data_p3$NDI_RP1_PPS_ths) # Individual-related redistribution: 9537 NAs (add also full version and NA indicator)
summary(data_p3$agea) # Age: 0 NAs
summary(data_p3$gndr) # Gender: 0 NAs
summary(data_p3$hinctnta_num) # Household income: 5106 NAs (add also full version and NA indicator)
summary(data_p3$cohort) # Cohort: 0 NAs
summary(data_p3$lms) # Labour market status: 51 NAs
summary(data_p3$high_second_edu) # Education: 102 NAs
summary(data_p3$rlgdgr_num) # Religiosity: 190 NAs
summary(data_p3$freehms_num) # Attitudes queer rights: 556 NAs
summary(data_p3$single_family) # Single individual or with family: 0 NAs
summary(data_p3$parentpartner) # Family type (children + partnership): 0 NAs
summary(data_p3$relstatus) # Relationship status (single/cohabitating/married): 0 NAs
summary(data_p3$nchild) # Number of children: 0 NAs
summary(data_p3$workingparents) # Working parents indicator: 0 NAs
summary(data_p3$earnings_dist) # Distribution of earnings: 0 NAs
summary(data_p3$regime) # Welfare regime: 0 NAs
summary(data_p3$GDP_pc) # GDP pc: 0 NAs
summary(data_p3$Gini) # Gini Index: 0 NAs
summary(data_p3$cntry) # Country: 0 NAs
summary(data_p3$average_FRR) # Average FRR in country: 0 NAs
summary(data_p3$average_FRR_sp) # Average FRR for single parents in country: 0 NAs
summary(data_p3$average_FRR_cwc) # Average FRR for couples with children in country: 0 NAs
summary(data_p3$average_FRR_cnc) # Average FRR for couples without children in country: 0 NAs
summary(data_p3$average_FRR_lwp) # Average FRR for those who live with their partner in country: 0 NAs
summary(data_p3$average_FRR_married) # Average FRR for married couples in country: 0 NAs
summary(data_p3$average_FRR_hk) # Average FRR for those who have children in country: 0 NAs
summary(data_p3$average_FRR_workp) # Average FRR for working parents in country: 0 NAs

data_full<-data_p3[,c("gincdif","gincdif_num","wrkprbf","wrkprbf_num",
                               "Diff_PPS_ths","Diff_PPS_ths_na","Diff_missing",
                               "NDI_RP1_PPS_ths","NDI_RP1_PPS_ths_na","NDI_RP1_missing",
                               "hinctnta_num","hinctnta_num_na","hinctnta_missing",
                               "agea","gndr","cohort","lms","high_second_edu",
                               "rlgdgr","rlgdgr_num","freehms","freehms_num",
                               "single_family","single_family_num",
                               "parentpartner","relstatus","nchild","workingparents","earnings_dist","earnings_dist2",
                               "cntry","regime","GDP_pc","Gini",
                               "average_FRR","average_FRR_sp","average_FRR_cwc",
                               "average_FRR_cnc","average_FRR_lwp","average_FRR_married",
                               "average_FRR_hk","average_FRR_workp","average_FRR_cohabitating",
                               "average_FRR_child12","average_FRR_child3",
                               "average_FRR_singleearner","average_FRR_supearner","average_FRR_doubearner", 
                               "anweight")]


# Remove the missings from all variables except household income, FRR and IRR
data_full<-data_full[-which(is.na(data_full$gincdif) | is.na(data_full$wrkprbf) | is.na(data_full$lms) |
                 is.na(data_full$high_second_edu) | is.na(data_full$rlgdgr) | is.na(data_full$freehms)),] 

# We lose 2694 cases
# The remaining missings correspond to missing cases for the household income, FRR and IRR variables. These will be handled
# In three ways: complete case analysis, missing variable indicator and multiple imputation.

save(ESS16,file="ESS16_Analysis.RData")
save(list=ls(),file="DataStudy3_2016.RData")

################################################################################
################################################################################

#####
# OPTION 1: COMPLETE CASE ANALYSIS
#####

##
# 1.1) INDIVIDUALS WITH VS WITHOUT FAMILY
##

# M0: y~family type+controls
cca_M0_1<-clmm(gincdif~single_family+hinctnta_num+agea+gndr+lms+
                 high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M0_1) # Having a family is associated with stronger support for redistribution

# M1: y~family type+controls
cca_M1_1<-clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
              high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
            data=data_full,weights=anweight,link="logit")
summary(cca_M1_1) # Having a family is associated with stronger support for redistribution

# M2: y~family type+FRR+IRR+controls
cca_M2_1<-clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M2_1)

# M3: y~family type*averageFRR+FRR+IRR+controls
cca_M3_1<-clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M3_1)

# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
cca_M4_1<-clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M4_1)

wordreg(list(cca_M1_1,cca_M2_1,cca_M3_1,cca_M4_1),
        custom.coef.names=c("Has family","Individual NDI",
                            "Household NDI","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4","Intercept 4|5",
                            "Family-Related Redistribution","Country-Average FRR","Has family:Country-Average FRR",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="cca1_Models.doc")

######

# Plot random slopes model to see the effect of having a family in each country

rsl_M2_1<-clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1+single_family|cntry),
               data=data_full,weights=anweight,link="logit")

# Plot the effect and random slopes (cant access confidence intervals)
plot_H1<-data.frame(cbind(levels(data_full$cntry),ranef(rsl_M2_1)$cntry[,2]))
colnames(plot_H1)<-c("country","total_effect")
plot_H1$total_effect<-as.numeric(plot_H1$total_effect)+coef(rsl_M2_1)[4]

agg_png("H1_ranef.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_H1, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = coef(rsl_M2_1)[4], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is positive in all countries and significant in most of them.
dev.off()

plot_H1$average_FRR<-numeric(nrow(plot_H1))
for (i in 1:nrow(plot_H1)){
  plot_H1$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_H1$country[i])[1]]
}

# Plot the relationship between effect of having a family and FRR
agg_png("H1_interaction.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H1, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line
  geom_text(vjust = -1, size = 3) +  # Add country labels
  scale_color_viridis(option = "magma", direction = -1) +  # Use "magma" viridis color palette
  labs(title = "",
       x = "Average FRR",
       y = "Effect of having a family on preferences") +
  theme_minimal()
dev.off()

#########################################################

##
# 1.2) FAMILY TYPES
##

# M1: y~family type+controls
cca_M1_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M1_2) #

# M2: y~family type+FRR+IRR+controls
cca_M2_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M2_2)

# M3: y~family type+FRR+IRR+controls+robustness
cca_M3_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                      NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                      high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M3_2)

wordreg(list(cca_M1_2,cca_M2_2,cca_M3_2),
        custom.coef.names=c("Cohabitating","Married","1-2 children",">= 3 children",
                            "Single earner","Supplementary earner","Double earner",
                            "Individual NDI",
                            "Household NDI","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4","Intercept 4|5",
                            "Family-Related Redistribution","Country-Average FRR",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="cca2_Models.doc")

#########################################################

##
# 1.3) PREFERENCES FOR WORK-FAMILY BALANCE
##

# M1: y~family type+controls
cca_M1_3<-clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M1_3) # Having children is positively associated with support for wfb

# M2: y~family type+FRR+IRR+controls
cca_M2_3<-clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M2_3)

# M3: y~family type*averageFRR+FRR+IRR+controls
cca_M3_3<-clmm(wrkprbf~workingparents*average_FRR_workp+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M3_3)


# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
cca_M4_3<-clmm(wrkprbf~workingparents+average_FRR_workp+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit") # I need to remove the interaction or model does not converge
summary(cca_M4_3)

wordreg(list(cca_M1_3,cca_M2_3,cca_M3_3,cca_M4_3),
        custom.coef.names=c("Working parents","Individual NDI",
                            "Household NDI","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4",
                            "Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="cca3_Models.doc")

##############

# Plot the interaction effect in M3:
plot_model(cca_M3_3, type = "int", terms = c("single_family", "average_FRR"))

# Plot random slopes model to see the effect of having a family in each country

rsl_M2_3<-clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1+workingparents|cntry),
               data=data_full,weights=anweight,link="logit")

# Plot the effect and random slopes (cant access confidence intervals)
plot_H3<-data.frame(cbind(levels(data_full$cntry),ranef(rsl_M2_3)$cntry[,2]))
colnames(plot_H3)<-c("country","total_effect")
plot_H3$total_effect<-as.numeric(plot_H3$total_effect)+coef(rsl_M2_3)[4]

agg_png("H3_ranef.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_H3, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = coef(rsl_M2_3)[4], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is positive in all countries and significant in most of them.
dev.off()

plot_H3$average_FRR<-numeric(nrow(plot_H3))
for (i in 1:nrow(plot_H3)){
  plot_H3$average_FRR[i]=data_full$average_FRR_workp[which(data_full$cntry==plot_H3$country[i])[1]]
}

# Plot the relationship between effect of having a family and FRR
agg_png("H3_interaction.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H3, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line
  geom_text(vjust = -1, size = 3) +  # Add country labels
  scale_color_viridis(option = "magma", direction = -1) +  # Use "magma" viridis color palette
  labs(title = "",
       x = "Average FRR for working parents",
       y = "Effect of having a family on preferences") +
  theme_minimal()
dev.off()

###############################################################################
###############################################################################

#####
# OPTION 2: MISSING VARIABLE INDICATOR
#####

##
# 2.1) INDIVIDUALS WITH VS WITHOUT FAMILY
##


# M1: y~family type+controls
mvi_M1_1<-clmm(gincdif~single_family+NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M1_1) 

# M2: y~family type+FRR+IRR+controls
mvi_M2_1<-clmm(gincdif~single_family+NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+
                 Diff_PPS_ths_na+average_FRR+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M2_1)

# M3: y~family type*averageFRR+FRR+IRR+controls
mvi_M3_1<-clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+
                 Diff_PPS_ths_na+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M3_1)

# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
mvi_M4_1<-clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+
                 Diff_PPS_ths_na+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M4_1)

wordreg(list(mvi_M1_1,mvi_M2_1,mvi_M3_1,mvi_M4_1),
        custom.coef.names=c("Has family","Individual NDI","Individual NDI: missing",
                            "Household NDI","Household NDI: missing","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4","Intercept 4|5",
                            "Family-Related Redistribution","Country-Average FRR","Has family:Country-Average FRR",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="mvi1_Models.doc")

######

# Plot the effect and random slopes (cant access confidence intervals)
plot_H1a_2<-data.frame(cbind(levels(data_p3$cntry),ranef(H1a_2)$cntry[,2]))
colnames(plot_H1a_2)<-c("country","total_effect")
plot_H1a_2$total_effect<-as.numeric(plot_H1a_2$total_effect)+coef(H1a_2)[4]

agg_png("H1a_ranef.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_H1a_2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = coef(H1a_2)[4], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is positive in all countries and significant in most of them.
dev.off()

# H1b
H1b_1<-clmm(gincdif~single_family*average_FRR+agea+gndr+hinctnta_num_na+hinctnta_missing+
              high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
            data=data_p3,weights=anweight,link="logit")
summary(H1b_1) # When adding the interaction effect the overall effect disappears. The interaction
# is positive and almost significant at 0.9 (p=0.107), suggesting that 

plot_H1b_1<-sjPlot::plot_model(H1b_1, type="int",pred.type="re", ci.lvl=NA,title="")+
  scale_color_manual(values=c("#377cb6","#4cad4b","#e58526","#e21c1b"))+
  labs(title="Mediterranean", y= "Probability", x = "Lives with parent(s)?")+
  labs(color = "Activity")+
  guides(color = guide_legend(reverse=FALSE,
                              override.aes=list(shape = c(19,17,15,4))))+
  bgcolor("#BFD5E3") +
  border("#BFD5E3")+
  scale_y_continuous(limits=c(2.3,3))+
  theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14),
        plot.title = element_text(family="Times New Roman",size=15,hjust=0.5))+
  theme(legend.position = "none")
Layers_plotMed<-ggplot_build(plotMed)
Layers_plotMed$data[[1]]$shape=rep(c(19,17,15,4),2)
plotMed<-ggplot_gtable(Layers_plotMed)

#########################################################

##
# 1.2) FAMILY TYPES
##

# M1: y~family type+controls
mvi_M1_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M1_2) 

# M2: y~family type+FRR+IRR+controls
mvi_M2_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths_na+average_FRR+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M2_2)

# M3: y~family type+FRR+IRR+controls+robustness
mvi_M3_2<-clmm(gincdif~relstatus+nchild+earnings_dist+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths_na+average_FRR+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M3_2)

wordreg(list(mvi_M1_2,mvi_M2_2,mvi_M3_2),
        custom.coef.names=c("Cohabitating","Married","1-2 children",">= 3 children",
                            "Single earner","Supplementary earner","Double earner",
                            "Individual NDI","Individual NDI: missing",
                            "Household NDI","Household NDI: missing","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4","Intercept 4|5",
                            "Family-Related Redistribution","Country-Average FRR",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="mvi2_Models.doc")

#########################################################

##
# 1.3) PREFERENCES FOR WORK-FAMILY BALANCE
##

# M1: y~family type+controls
mvi_M1_3<-clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M1_3) # Having children is positively associated with support for wfb

# M2: y~family type+FRR+IRR+controls
mvi_M2_3<-clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths_na+average_FRR_workp+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M2_3)

# M3: y~family type*averageFRR+FRR+IRR+controls
mvi_M3_3<-clmm(wrkprbf~workingparents*average_FRR_workp+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths_na+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(mvi_M3_3)


# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
mvi_M4_3<-clmm(wrkprbf~workingparents*average_FRR_workp+
                 NDI_RP1_PPS_ths_na+NDI_RP1_missing+hinctnta_num_na+hinctnta_missing+
                 agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths_na+
                 cohort+GDP_pc+Gini+(1|cntry),
               data=data_full,weights=anweight,link="logit") # I need to remove the interaction or model does not converge
summary(mvi_M4_3)

wordreg(list(mvi_M1_3,mvi_M2_3,mvi_M3_3,mvi_M4_3),
        custom.coef.names=c("Working parents","Individual NDI","Individual NDI: missing",
                            "Household NDI","Household NDI: missing","Age","Gender: Male","NEET","In education",
                            "Higher secondary education",
                            "Religiosity","Support queer rights",
                            "Intercept 1|2","Intercept 2|3","Intercept 3|4",
                            "Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
                            "Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="mvi3_Models.doc")

###############################################################################
###############################################################################

#####
# OPTION 3: MULTIPLE IMPUTATION
#####

set.seed(1234) # Seed for replicability

md.pattern(data_full) # We only have missings in the household income and imputed variables

# Specify the predictor matrix
pred_matrix<-matrix(rep(0,ncol(data_full)^2),nrow=ncol(data_full),ncol=ncol(data_full))
colnames(pred_matrix)=colnames(data_full);rownames(pred_matrix)=colnames(data_full)

imp_ind<-which(rownames(pred_matrix) %in% c("hinctnta_num","NDI_RP1_PPS_ths","Diff_PPS_ths"))
pred_ind<-which(colnames(pred_matrix) %in% c("gincdif_num","wrkprbf_num","agea","gndr","lms","high_second_edu","rlgdgr_num",
                                             "freehms_num","relstatus","nchild","earnings_dist","cntry","GDP_pc","Gini",
                                             "average_FRR"))

pred_matrix[imp_ind,pred_ind]=1

# Generate multiple imputations:
imputation<-mice(data_full,pred=pred_matrix,m=5,method=c(rep("",4),"pmm",
                                             "","","pmm","","","pmm",rep("",38)))


xyplot(imputation,Diff_PPS_ths~gincdif_num,pch=20,cex=1.4)

#########################################################

##
# 1.1) INDIVIDUALS WITH VS WITHOUT FAMILY
##

# M1: y~family type+controls

mimp_M1_1<-with(imputation,clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                  weights=anweight,link="logit"))
summary(pool(mimp_M1_1)) 

mean(sapply(mimp_M1_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M1_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M2: y~family type+FRR+IRR+controls
mimp_M2_1<-with(imputation,clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                 weights=anweight,link="logit"))
summary(pool(mimp_M2_1))

mean(sapply(mimp_M2_1$analyses,AIC))
mean(sapply(mimp_M2_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M3: y~family type*averageFRR+FRR+IRR+controls
mimp_M3_1<-with(imputation,clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
                 weights=anweight,link="logit"))
summary(pool(mimp_M3_1))

mean(sapply(mimp_M3_1$analyses,AIC))
mean(sapply(mimp_M3_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
mimp_M4_1<-with(imputation,clmm(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+
                 cohort+GDP_pc+Gini+(1|cntry),
               weights=anweight,link="logit")) # This model does not converge, ignore the extra controls for the simulations
summary(pool(mimp_M4_1))

#########

# Plot random slopes model to see the effect of having a family in each country

rsl_M2_1<-with(imputation,clmm(gincdif~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1+single_family|cntry),
                               weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2_1))

fixed_effect <- pooled_results$estimate[which(pooled_results$term == "average_FRR")]

random_slopes_list <- lapply(rsl_M2_1$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect<-pooled_results$estimate[5]

plot_H1<-data.frame(cbind(levels(data_full$cntry),numeric(14)))
colnames(plot_H1)<-c("country","total_effect")

for (i in 1:nrow(plot_H1)){
  plot_H1$total_effect[i]=fixed_effect+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                            random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                            random_slopes_list[[5]][i,2])
}
plot_H1$total_effect<-as.numeric(plot_H1$total_effect)

plot_H1$average_FRR<-numeric(nrow(plot_H1))
for (i in 1:nrow(plot_H1)){
  plot_H1$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_H1$country[i])[1]]
}

# Plot the relationship between effect of having a family and FRR
agg_png("H1_interaction.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H1, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line
  geom_text(vjust = -1, size = 3) +  # Add country labels
  scale_color_viridis(option = "magma", direction = -1) +  # Use "magma" viridis color palette
  labs(title = "",
       x = "Average FRR",
       y = "Effect of having a family on preferences") +
  theme_minimal()
dev.off()

## The same with CIs

# Extract random slopes per country from each imputed model
random_slopes_list <- lapply(rsl_M2_1$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes
})

# Extract fixed effect
pooled_results <- summary(pool(rsl_M2_1))
fixed_effect <- pooled_results$estimate[5]  # Adjust index if needed

# Create a data frame for plotting
plot_H1 <- data.frame(country = levels(data_full$cntry))

# Compute mean random slopes and standard error
random_slopes_matrix <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes)
plot_H1$random_slope_mean <- rowMeans(random_slopes_matrix)
plot_H1$random_slope_se <- apply(random_slopes_matrix, 1, function(x) sd(x) / sqrt(length(x)))

# Compute total effect and confidence intervals
plot_H1$total_effect <- fixed_effect + plot_H1$random_slope_mean
plot_H1$lower_CI <- plot_H1$total_effect - 1.96 * plot_H1$random_slope_se
plot_H1$upper_CI <- plot_H1$total_effect + 1.96 * plot_H1$random_slope_se

# Add average_FRR_workp for each country
plot_H1$average_FRR <- sapply(plot_H1$country, function(cntry) {
  data_full$average_FRR[which(data_full$cntry == cntry)[1]]
})

# Plot with confidence intervals
agg_png("H1_interaction_CIs.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H1, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  geom_text(vjust = -1.6, size = 3) +  # Country labels
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, color = "#21908C") +  # Confidence intervals
  scale_color_viridis(option = "magma", direction = -1) +  # Color scheme
  labs(title = "",
       x = "Average FRR",
       y = "Effect of having a family on preferences") +
  theme_minimal()
dev.off()

#########################################################

##
# 1.2) FAMILY TYPES
##

# M1: y~family type+controls
mimp_M1_2<-with(imputation,clmm(gincdif~relstatus+nchild+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               weights=anweight,link="logit"))

summary(pool(mimp_M1_2))

mean(sapply(mimp_M1_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M1_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M2: y~family type+FRR+IRR+controls
mimp_M2_2<-with(imputation,clmm(gincdif~relstatus+nchild+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                 weights=anweight,link="logit"))
summary(pool(mimp_M2_2))

mean(sapply(mimp_M2_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M2_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

#########################################################

##
# 1.3) PREFERENCES FOR WORK-FAMILY BALANCE
##

# M1: y~family type+controls
mimp_M1_3<-with(imputation,clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
               weights=anweight,link="logit"))
summary(pool(mimp_M1_3)) 

mean(sapply(mimp_M1_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M1_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M2: y~family type+FRR+IRR+controls
mimp_M2_3<-with(imputation,clmm(wrkprbf~workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1|cntry),
               weights=anweight,link="logit"))
summary(pool(mimp_M2_3))

mean(sapply(mimp_M2_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M2_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M3: y~family type*averageFRR+FRR+IRR+controls
mimp_M3_3<-with(imputation,clmm(wrkprbf~workingparents*average_FRR_workp+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                 high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
               weights=anweight,link="logit"))
summary(pool(mimp_M3_3))

mean(sapply(mimp_M3_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M3_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M4: y~family type+controls
mimp_M3_4<-with(imputation,clmm(wrkprbf~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                weights=anweight,link="logit"))

summary(pool(mimp_M3_4))

mean(sapply(mimp_M3_4$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M3_4$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

# M5: y~family type+FRR+IRR+controls
mimp_M3_5<-with(imputation,clmm(wrkprbf~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                                weights=anweight,link="logit"))
summary(pool(mimp_M3_5))

mean(sapply(mimp_M3_5$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(mimp_M3_5$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

#########

# Plot random slopes model to see the effect of having a family in each country

rsl_M2_3<-with(imputation,clmm(wrkprbf~workingparents+
                                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+
                                 rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1+workingparents|cntry),
                               weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2_3))

random_slopes_list <- lapply(rsl_M2_3$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect<-pooled_results$estimate[4]

plot_H3<-data.frame(cbind(levels(data_full$cntry),numeric(14)))
colnames(plot_H3)<-c("country","total_effect")

for (i in 1:nrow(plot_H3)){
  plot_H3$total_effect[i]=fixed_effect+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                            random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                            random_slopes_list[[5]][i,2])
}
plot_H3$total_effect<-as.numeric(plot_H3$total_effect)

plot_H3$average_FRR_workp<-numeric(nrow(plot_H1))
for (i in 1:nrow(plot_H1)){
  plot_H3$average_FRR_workp[i]=data_full$average_FRR_workp[which(data_full$cntry==plot_H3$country[i])[1]]
}

# Plot the relationship between effect of having a family and FRR
agg_png("H3_interaction.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H3, aes(x = average_FRR_workp, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Linear regression line
  geom_text(vjust = -1, size = 3) +  # Add country labels
  scale_color_viridis(option = "magma", direction = -1) +  # Use "magma" viridis color palette
  labs(title = "",
       x = "Average FRR for working parents",
       y = "Effect of being a working parent on preferences") +
  theme_minimal()
dev.off()

## The same with CIs

# Extract random slopes per country from each imputed model
random_slopes_list <- lapply(rsl_M2_3$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes
})

# Extract fixed effect
pooled_results <- summary(pool(rsl_M2_3))
fixed_effect <- pooled_results$estimate[4]  # Adjust index if needed

# Create a data frame for plotting
plot_H3 <- data.frame(country = levels(data_full$cntry))

# Compute mean random slopes and standard error
random_slopes_matrix <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes)
plot_H3$random_slope_mean <- rowMeans(random_slopes_matrix)
plot_H3$random_slope_se <- apply(random_slopes_matrix, 1, function(x) sd(x) / sqrt(length(x)))

# Compute total effect and confidence intervals
plot_H3$total_effect <- fixed_effect + plot_H3$random_slope_mean
plot_H3$lower_CI <- plot_H3$total_effect - 1.96 * plot_H3$random_slope_se
plot_H3$upper_CI <- plot_H3$total_effect + 1.96 * plot_H3$random_slope_se

# Add average_FRR_workp for each country
plot_H3$average_FRR_workp <- sapply(plot_H3$country, function(cntry) {
  data_full$average_FRR_workp[which(data_full$cntry == cntry)[1]]
})

# Plot with confidence intervals
agg_png("H3_interaction_CIs.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H3, aes(x = average_FRR_workp, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  geom_text(vjust = -2, size = 3) +  # Country labels
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, color = "#21908C") +  # Confidence intervals
  scale_color_viridis(option = "magma", direction = -1) +  # Color scheme
  labs(title = "",
       x = "Average FRR for working parents",
       y = "Effect of being a working parent on preferences") +
  theme_minimal()
dev.off()

###

# Plot the (linear) effect of FRR on preferences for work-family balance policies

# M2: y~family type+FRR+IRR+controls
lmimp_M2_3<-with(imputation,lmer(wrkprbf_num~workingparents+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1|cntry),
                                  weights=anweight))


# Extract fixed effects for Diff_PPS_ths across imputations
diff_pps_effects <- lapply(lmimp_M2_3$analyses, function(model) {
  tidy(model, effects = "fixed") %>% filter(term == "Diff_PPS_ths")
})

# Convert list to a single data frame
diff_pps_df <- do.call(rbind, diff_pps_effects)

estimate=mean(diff_pps_df$estimate)
se=sqrt(mean(diff_pps_df$std.error^2) + (1 + 1 / length(diff_pps_effects)) * var(diff_pps_df$estimate))
lower_CI=estimate - 1.96*se;upper_CI=estimate+1.96*se

pooled_diff_plot<-data.frame(cbind(estimate,se,lower_CI,upper_CI))

ggplot(pooled_diff_plot, aes(x = "Diff_PPS_ths", y = estimate)) +
  geom_point(size = 4, color = "#21908C") +  # Point for estimate
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2, color = "#21908C") +  # CI bars
  labs(title = "Pooled Effect of Diff_PPS_ths",
       x = "",
       y = "Estimated Effect") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  # Remove x-axis text

#########################################################

## LINEAR MODELS WITH MULTIPLE IMPUTATIONS

##
# 1.1) INDIVIDUALS WITH VS WITHOUT FAMILY
##

# M1: y~family type+controls

lmimp_M1_1<-with(imputation,lmer(gincdif_num~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M1_1)))

# M2: y~family type+FRR+IRR+controls
lmimp_M2_1<-with(imputation,lmer(gincdif_num~single_family+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M2_1)))


# M3: y~family type*averageFRR+FRR+IRR+controls
lmimp_M3_1<-with(imputation,lmer(gincdif_num~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M3_1)))


# M4: y~family type*averageFRR+FRR+IRR+controls+robustness
lmimp_M4_1<-with(imputation,lmer(gincdif_num~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+
                                  cohort+GDP_pc+Gini+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M4_1)))


#########################################################

##
# 1.2) FAMILY TYPES
##

# M1: y~family type+controls
lmimp_M1_2<-with(imputation,lmer(gincdif_num~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M1_2)))

# M2: y~family type+FRR+IRR+controls
lmimp_M2_2<-with(imputation,lmer(gincdif_num~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                                weights=anweight))
summary(mice::pool(as.mira(lmimp_M2_2)))

# M3: y~family type*averageFRR+FRR+IRR+controls+robustness
lmimp_M3_2<-with(imputation,lmer(gincdif_num~relstatus+nchild+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+
                                   cohort+GDP_pc+Gini+(1|cntry),
                                 weights=anweight))

summary(mice::pool(as.mira(lmimp_M3_2)))


#########################################################

##
# 1.3) PREFERENCES FOR WORK-FAMILY BALANCE
##

# M1: y~family type+controls
lmimp_M1_3<-with(imputation,lmer(wrkprbf_num~workingparents+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                weights=anweight))
summary(mice::pool(as.mira(lmimp_M1_3)))

# M2: y~family type+FRR+IRR+controls
lmimp_M2_3<-with(imputation,lmer(wrkprbf_num~workingparents+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR_workp+(1|cntry),
                                weights=anweight))
summary(mice::pool(as.mira(lmimp_M2_3)))

# M3: y~family type*averageFRR+FRR+IRR+controls
lmimp_M3_3<-with(imputation,lmer(wrkprbf_num~workingparents*average_FRR_workp+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+(1|cntry),
                                weights=anweight))
summary(mice::pool(as.mira(lmimp_M3_3)))

# M4: y~family type+controls
lmimp_M4_3<-with(imputation,lmer(wrkprbf_num~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+(1|cntry),
                                weights=anweight))

summary(mice::pool(as.mira(lmimp_M4_3)))

# M5: y~family type+FRR+IRR+controls
lmimp_M5_3<-with(imputation,lmer(wrkprbf_num~relstatus+nchild+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                  high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1|cntry),
                                weights=anweight))
summary(mice::pool(as.mira(lmimp_M5_3)))

# M6: y~family type+FRR+IRR+controls
lmimp_M6_3<-with(imputation,lmer(wrkprbf_num~workingparents*average_FRR_workp+
                                   NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths+
                                   cohort+GDP_pc+Gini++(1|cntry),
                                 weights=anweight))
summary(mice::pool(as.mira(lmimp_M6_3)))

#####################

save(ESS16,file="ESS16_Analysis.RData")
save(list=ls(),file="DataStudy3_2016.RData")

####################

# ROBUSTNESS:

### 
# Parallel slopes assumption:
###

# Gincdif models
ps_gincdif<-polr(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths,
               data=data_full,weights=anweight,Hess=T)
brant(ps_gincdif)
Para
stargazer(brant(ps_gincdif)[,c(1,3)],type="latex")

# Wrkprbf models
ps_wrkprbf<-polr(wrkprbf~workingparents*average_FRR_workp+
                   NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths,
                 data=data_full,weights=anweight,Hess=T)
brant(ps_wrkprbf)

stargazer(brant(ps_wrkprbf)[,c(1,3)],type="latex")


