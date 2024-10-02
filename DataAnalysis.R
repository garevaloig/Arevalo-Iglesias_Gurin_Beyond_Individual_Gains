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

setwd("D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Data")
load("DataStudy3.RData")

#############################

####
# 1) RANDOM-SLOPE LINEAR MODELS FOR FAMILY CHARACTERISTICS
####

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

######################################################

# 1.1 INDIVIDUALS WITH FAMILY (REF: SINGLE INDIVIDUALS)

lm1<-lmer(gincdif_num~single_family+agea+gndr+hinctnta+earnings_dist+
                        high_second_edu+rlgdgr+freehms+(1+single_family|cntry),
                      data=data_p3,weights=anweight)

summary(lm1) # Overall, ind. with family are slightly more redistributive (0.08)

plot_lm1<-plot_matrix(lm1)

# Plot it
ggplot(plot_lm1, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(lm1)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Having a Family (vs. SIs) on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is positive in all countries and significant in most of them.

#######################################################

# 1.2 INDIVIDUALS LIVING WITH A PARTNER (REF: NOT LIVING WITH A PARTNER)

lm2<-lmer(gincdif_num~lwpartner+hasmkids_bin+agea+gndr+hinctnta+earnings_dist+
                         high_second_edu+rlgdgr+freehms+(1+lwpartner+hasmkids_bin|cntry),
                       data=data_p3,weights=anweight)

summary(lm2) # Overall, ind. living with a partner are more redistributive (0.07)

plot_lm2<-plot_matrix(lm2)

# Plot it
ggplot(plot_lm2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(lm2)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Living with a Partner on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is positive in all countries and significant in all except Spain

#######################################################

# 1.3.1 INDIVIDUALS WHO HAVE MINOR CHILDREN IN HH (REF: INDIVIDUALS WITHOUT MINOR CHILDREN IN HH)

# Plot effect for having minor children in the HH
lm3.1<-lmer(gincdif_num~hasmkids_bin+lwpartner+agea+gndr+hinctnta+earnings_dist+
                         high_second_edu+rlgdgr+freehms+(1+hasmkids_bin+lwpartner|cntry),
                       data=data_p3,weights=anweight)

summary(lm3.1) # Overall, having children is not associated with preferences

plot_lm3.1<-plot_matrix(lm3.1)

# Plot it
ggplot(plot_lm3.1, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(lm3.1)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Having Children on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() # The effect is negative and significant in Netherlands and Germany, and positive and significant in Spain

######

# 1.3.2 NUMBER OF MINOR CHILDREN IN HH
lm3.2<-lmer(gincdif_num~nmchildren+lwpartner+agea+gndr+hinctnta+earnings_dist+
                         high_second_edu+rlgdgr+freehms+(1+nmchildren+lwpartner|cntry),
                       data=data_p3,weights=anweight)

summary(lm3.2) # Overall, the number children is not associated with preferences

plot_lm3.2<-plot_matrix(lm3.2)

# Plot it
ggplot(plot_lm3.2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(lm3.2)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Number of Children on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip()  # In Netherlands, Denmark, Czechia and Germany, having one more child makes you less redistributive.
# In Spain, having one more child makes you more redistributive

######################################################

# 1.4 SINGLE PARENTS (REF: COUPLE PARENTS)

data_p3$couple_singleparents<-factor(nrow(data_p3),levels=c("Couple Parents","Single Parent"))
data_p3$couple_singleparents[which(data_p3$parentpartner=="Single no children")]=NA
data_p3$couple_singleparents[which(data_p3$parentpartner=="Couple no children")]=NA
data_p3$couple_singleparents[which(data_p3$parentpartner=="Single parent")]="Single Parent"
data_p3$couple_singleparents[which(data_p3$parentpartner=="Couple parents")]="Couple Parents"

lm4<-lmer(gincdif_num~couple_singleparents+agea+gndr+hinctnta+earnings_dist+
              high_second_edu+rlgdgr+freehms+(1+couple_singleparents|cntry),
            data=data_p3,weights=anweight)

summary(lm4) # Overall, no effect is observed

plot_lm4<-plot_matrix(lm4)

# Plot it
ggplot(plot_lm4, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(lm4)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Being a Single Parent (vs. Couple Parents) on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip()  # No significant effects for any of the countries

############################################################################################################
############################################################################################################

####
# 2) RANDOM-SLOPE LOGIT MODELS FOR FAMILY CHARACTERISTICS
####

######################################################

# 1.1 INDIVIDUALS WITH FAMILY (REF: SINGLE INDIVIDUALS)

omm1<-clmm(gincdif~single_family+agea+gndr+hinctnta+earnings_dist+
            high_second_edu+rlgdgr+freehms+(1+single_family|cntry),
          data=data_p3,weights=anweight,link="logit")

summary(omm1) # Overall, ind. with family are slightly more redistributive (0.08)

############################################################################################################
############################################################################################################

####
# 3) LOGIT MODELS FOR THE DIFFERENCE BETWEEN RESPONDENT NDI AND RP
####

######################################################

data_p3$Diff_dem<-numeric(nrow(data_p3)) # Create a demeaned Difference variable to eliminate country differences
for (i in 1:nrow(data_p3)){
  data_p3$Diff_dem[i]<-(data_p3$Diff_Eur[i]-mean(data_p3$Diff_Eur[which(data_p3$cntry==data_p3$cntry[i])],na.rm=T))/1000}

lm1.1<-lm(gincdif_num~Diff_Eur_ths+agea+gndr+hinctnta+earnings_dist+
            high_second_edu+rlgdgr+freehms,
          data=data_p3,weights=anweight) # The bigger the difference, the lower the support for redistribution

lm1.2<-lm(gincdif_num~Diff_PPS_ths+agea+gndr+hinctnta+earnings_dist+
           high_second_edu+rlgdgr+freehms,
         data=data_p3,weights=anweight)   # The bigger the difference, the lower the support for redistribution

data_p3$NDI_RE_dem<-numeric(nrow(data_p3)) # Create a demeaned variable to eliminate country differences
for (i in 1:nrow(data_p3)){
  data_p3$NDI_RE_dem[i]<-(data_p3$NDI_RE_Eur[i]-mean(data_p3$NDI_RE_Eur[which(data_p3$cntry==data_p3$cntry[i])],na.rm=T))/1000}

lm2.1<-lm(gincdif_num~NDI_RE_Eur+agea+gndr+hinctnta+earnings_dist+
          high_second_edu+rlgdgr+freehms,
        data=data_p3,weights=anweight)   # The higher the NDI of the respondent, the lower the support for redistribution

lm2.2<-lm(gincdif_num~NDI_RE_PPS+agea+gndr+hinctnta+earnings_dist+
            high_second_edu+rlgdgr+freehms,
          data=data_p3,weights=anweight)   # The higher the NDI of the respondent, the lower the support for redistribution

data_p3$NDI_RP_dem<-numeric(nrow(data_p3)) # Create a demeaned variable to eliminate country differences
for (i in 1:nrow(data_p3)){
  data_p3$NDI_RP_dem[i]<-(data_p3$NDI_RP_Eur[i]-mean(data_p3$NDI_RP_Eur[which(data_p3$cntry==data_p3$cntry[i])],na.rm=T))/1000}

lm3.1<-lm(gincdif_num~NDI_RP_Eur+agea+gndr+hinctnta+earnings_dist+
          high_second_edu+rlgdgr+freehms,
        data=data_p3,weights=anweight)   # The higher the NDI of the reference point, the lower the support for redistribution

lm3.2<-lm(gincdif_num~NDI_RP_PPS+agea+gndr+hinctnta+earnings_dist+
          high_second_edu+rlgdgr+freehms, 
        data=data_p3,weights=anweight)

data_p3$NDI_FF_dem<-numeric(nrow(data_p3)) # Create a demeaned variable to eliminate country differences
for (i in 1:nrow(data_p3)){
  data_p3$NDI_FF_dem[i]<-(data_p3$NDI_FF_Eur[i]-mean(data_p3$NDI_FF_Eur[which(data_p3$cntry==data_p3$cntry[i])],na.rm=T))/1000}

lm4.1<-lm(gincdif_num~NDI_FF_Eur+agea+gndr+hinctnta+earnings_dist+
            high_second_edu+rlgdgr+freehms,
          data=data_p3,weights=anweight)   # The higher the NDI of the family form, the lower the support for redistribution

lm4.2<-lm(gincdif_num~NDI_PPS_dem+agea+gndr+hinctnta+earnings_dist+
            high_second_edu+rlgdgr+freehms,
          data=data_p3,weights=anweight) 