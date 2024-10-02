##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

####
## SCRIPT 2: EXPLORATORY ANALYSES
####

library(lme4)
library(lmerTest)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(tidyr)
library(extrafont)
library(patchwork)
library(ie2misc)

#font_import()
#loadfonts()
#fonts()

setwd("D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Data")
load("DataStudy3.RData")

############

##
# 1) AVERAGE PREFERENCES FOR DIFFERENT FAMILY TYPES
##

## Single individuals vs individuals with family

# Average preference for single individuals (without children or partner) (RP)
singleind_av<- data_p3 %>%
  filter(single_family == 0) %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with family (with children and/or partner)
indwfam_av<- data_p3 %>%
  filter(single_family == 1) %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

si_iwm<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_gincdif_num,
                   With_Family=indwfam_av$mean_gincdif_num,Difference=indwfam_av$mean_gincdif_num-singleind_av$mean_gincdif_num)
si_iwm<-si_iwm[order(si_iwm$Difference),]
si_iwm$Country<-factor(si_iwm$Country,levels=si_iwm$Country[order(si_iwm$Difference)])
si_iwm<-melt(si_iwm,id.vars="Country")

p1<-ggplot(si_iwm[1:44,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "blue", "With_Family" = "red"),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  scale_shape_manual(values = c("Single_Individual" = 16, "With_Family" = 17),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(si_iwm[45:nrow(si_iwm),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_SI<-p1/p2+plot_layout(heights=c(2,1));print(cp_SI)

################

## Living with partner vs not living with partner

# Average preference for individuals living with a partner
lwp_av<- data_p3 %>%
  filter(lwpartner == "yes") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals not living with a partner
nlwp_av<- data_p3 %>%
  filter(lwpartner == "no") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

lwp_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Alone=nlwp_av$mean_gincdif_num,
                   With_Partner=lwp_av$mean_gincdif_num,Difference=lwp_av$mean_gincdif_num-nlwp_av$mean_gincdif_num)
lwp_plot<-lwp_plot[order(lwp_plot$Difference),]
lwp_plot$Country<-factor(lwp_plot$Country,levels=lwp_plot$Country[order(lwp_plot$Difference)])
lwp_plot <- melt(lwp_plot, id.vars = "Country")

p1<-ggplot(lwp_plot[1:44,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Alone" = "blue", "With_Partner" = "red"),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  scale_shape_manual(values = c("Alone" = 16, "With_Partner" = 17),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  # Customize labels and theme
  labs(#title="Average preference for redistribution",
    x = "", y = "Average Preference", color = "Group", shape = "Group") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(lwp_plot[45:nrow(lwp_plot),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Difference") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_lwp<-p1/p2+plot_layout(heights=c(2,1));print(cp_lwp)

##################

## Individuals with and without children (and number of children)

# Average preference for individuals without children
nochild_av<- data_p3 %>%
  filter(nmchildren_grp2 == "no children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with 1-2 children
child12_av<- data_p3 %>%
  filter(nmchildren_grp2 == "1-2 children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with 3 or more children
child3_av<- data_p3 %>%
  filter(nmchildren_grp2 == "3 children or more") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

nchild_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), no_child=nochild_av$mean_gincdif_num,
                     one_or_two=child12_av$mean_gincdif_num,three_or_more=child3_av$mean_gincdif_num)
for (i in 1:nrow(nchild_plot)){
  nchild_plot$Difference[i]<-madstat(c(nchild_plot$no_child[i],nchild_plot$one_or_two[i],nchild_plot$three_or_more[i]))
}
nchild_plot<-nchild_plot[order(nchild_plot$Difference),]
nchild_plot$Country<-factor(nchild_plot$Country,levels=nchild_plot$Country[order(nchild_plot$Difference)])
nchild_plot <- melt(nchild_plot, id.vars = "Country")

p1<-ggplot(nchild_plot[1:66,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("no_child" = "blue", "one_or_two" = "red", "three_or_more"="green"),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  scale_shape_manual(values = c("no_child" = 16, "one_or_two" = 17, "three_or_more"=18),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(nchild_plot[67:88,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_child<-p1/p2+plot_layout(heights=c(2,1));print(cp_child)

################

## Individuals with and without children (and number of children) in low income households

# Average preference for low income individuals without children
nochild_low_av<- data_p3 %>%
  filter(nmchildren_grp2 == "no children") %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for low income individuals with 1-2 children
child12_low_av<- data_p3 %>%
  filter(nmchildren_grp2 == "1-2 children") %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for low income individuals with 3 or more children
child3_low_av<- data_p3 %>%
  filter(nmchildren_grp2 == "3 children or more") %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE)) # Interestingly, families with many children are less redistributive even with low incomes

nchild_li_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), no_child=nochild_low_av$mean_gincdif_num,
                        one_or_two=child12_low_av$mean_gincdif_num,three_or_more=child3_low_av$mean_gincdif_num)
for (i in 1:nrow(nchild_li_plot)){
nchild_li_plot$Difference[i]<-madstat(c(nchild_li_plot$no_child[i],nchild_li_plot$one_or_two[i],nchild_li_plot$three_or_more[i]))
}
nchild_li_plot<-nchild_li_plot[order(nchild_li_plot$Difference),]
nchild_li_plot$Country<-factor(nchild_li_plot$Country,levels=nchild_li_plot$Country[order(nchild_li_plot$Difference)])
nchild_li_plot <- melt(nchild_li_plot, id.vars = "Country")

p1<-ggplot(nchild_li_plot[1:66,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("no_child" = "blue", "one_or_two" = "red", "three_or_more"="green"),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  scale_shape_manual(values = c("no_child" = 16, "one_or_two" = 17, "three_or_more"=18),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(nchild_li_plot[67:88,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_li_child<-p1/p2+plot_layout(heights=c(2,1));print(cp_li_child)

#################

## Single parents vs couples with children vs childless couples (single individual already computed as RP)

# Average preference for single parents
sp_av<- data_p3 %>%
  filter(parentpartner == "Single parent") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for couples with children
cwc_av<- data_p3 %>%
  filter(parentpartner == "Couple parents") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for couples without children
cnc_av<- data_p3 %>%
  filter(parentpartner == "Couple no children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

ft_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_gincdif_num,
                    Single_Parent=sp_av$mean_gincdif_num,Couple_w_Children=cwc_av$mean_gincdif_num,Couple_no_Children=cnc_av$mean_gincdif_num)

for (i in 1:nrow(ft_plot)){ # Calculate mean absolute deviation among the 4 groups
ft_plot$Difference[i]<-madstat(c(ft_plot$Single_Individual[i],ft_plot$Single_Parent[i],
                                 ft_plot$Couple_w_Children[i],ft_plot$Couple_no_Children[i]))
}
ft_plot<-ft_plot[order(ft_plot$Difference),]
ft_plot$Country<-factor(ft_plot$Country,levels=ft_plot$Country[order(ft_plot$Difference)])
ft_plot <- melt(ft_plot, id.vars = "Country")

p1<-ggplot(ft_plot[1:88,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "blue", "Single_Parent" = "lightblue", "Couple_w_Children"="orange",
                                "Couple_no_Children"="red"),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  scale_shape_manual(values = c("Single_Individual" = 16, "Single_Parent" = 17, "Couple_w_Children"=15,
                                "Couple_no_Children"=18),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(ft_plot[89:110,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

ftp_plot<-p1/p2+plot_layout(heights=c(2,1));print(ftp_plot)

#################################

## Married vs cohabiting couples

# Average preference for married couples
marriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

nmarriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Not married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

marcoh_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Not_Married=nmarriedcohab_av$mean_gincdif_num,
                     Married=marriedcohab_av$mean_gincdif_num)
marcoh_plot$Difference<-marcoh_plot$Married-marcoh_plot$Not_Married
marcoh_plot<-marcoh_plot[order(marcoh_plot$Difference),]
marcoh_plot$Country<-factor(marcoh_plot$Country,levels=marcoh_plot$Country[order(marcoh_plot$Difference)])
marcoh_plot <- melt(marcoh_plot, id.vars = "Country")

ggplot(marcoh_plot, aes(x = Country, y = value, color = variable)) +
  geom_point(size = 3) +
  labs(x = "Country", y = "Average Preference", color = "Group", title = "Values by Category") +
  scale_color_manual(values = c("Not_Married" = "blue", "Married" = "red")) +
  theme_minimal()

p1<-ggplot(marcoh_plot[1:44,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Married" = "blue", "Not_Married" = "red"),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  scale_shape_manual(values = c("Married" = 16, "Not_Married" = 17),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(marcoh_plot[45:nrow(marcoh_plot),], aes(x = Country, y = value))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_marcoh<-p1/p2+plot_layout(heights=c(2,1));print(cp_marcoh)

######################################################

## No earner vs Single earner vs Dual earner vs Supplementary earner

# Average preference for no earner HHs
noearn_av<- data_p3 %>%
  filter(earnings_dist == "No earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for single earner HHs
singlearn_av<- data_p3 %>%
  filter(earnings_dist == "Single earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for supplementary earner HHs
supearn_av<- data_p3 %>%
  filter(earnings_dist == "Supplementary earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for double earner HHs
doubearn_av<- data_p3 %>%
  filter(earnings_dist == "Double earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

earn_plot<-merge(merge(singlearn_av,supearn_av,by="cntry",all=T),doubearn_av,by="cntry",all=T)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner")
earn_plot<-cbind(earn_plot,noearn_av$mean_gincdif_num)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner","No_Earner")

for (i in 1:nrow(earn_plot)){ # Calculate mean absolute deviation among the 4 groups
  earn_plot$Difference[i]<-madstat(c(earn_plot$Single_Earner[i],earn_plot$Supplementary_Earner[i],
                                     earn_plot$Dual_Earner[i],earn_plot$No_Earner[i]),na.rm=T)
}

earn_plot<-earn_plot[order(earn_plot$Difference),]
earn_plot$Country<-factor(earn_plot$Country,levels=earn_plot$Country[order(earn_plot$Difference)])
earn_plot <- melt(earn_plot, id.vars = "Country")

p1<-ggplot(earn_plot[1:88,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Earner" = "blue", "Supplementary_Earner" = "red", "Dual_Earner"="green","No_Earner"="purple"),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  scale_shape_manual(values = c("Single_Earner" = 16, "Supplementary_Earner" = 17, "Dual_Earner"=18,"No_Earner"=19),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(earn_plot[89:110,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

earnings_plot<-p1/p2+plot_layout(heights=c(2,1));print(earnings_plot)
################################

## HH income

# Average preference for low HH income
lhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for medium HH income
mhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Middle") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for high HH income
hhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "High") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

hh_plot<-merge(merge(lhhi_av,mhhi_av,by="cntry",all=T),hhhi_av,by="cntry",all=T)
colnames(hh_plot)=c("Country","Low","Middle","High")
for (i in 1:nrow(hh_plot)){ # Calculate mean absolute deviation among the 4 groups
  hh_plot$Difference[i]<-madstat(c(hh_plot$Low[i],hh_plot$Middle[i],hh_plot$High[i]),na.rm=T)
}
hh_plot<-hh_plot[order(hh_plot$Difference),]
hh_plot$Country<-factor(hh_plot$Country,levels=hh_plot$Country[order(hh_plot$Difference)])
hh_plot <- melt(hh_plot, id.vars = "Country")

p1<-ggplot(hh_plot[1:66,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Low"="blue","Middle"="red","High"="green"),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  scale_shape_manual(values = c("Low"=16,"Middle"=17,"High"=18),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(hh_plot[67:88,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

hhinc_plot<-p1/p2+plot_layout(heights=c(2,1));print(hhinc_plot)

##############################################################
##############################################################

## All mean preferences by group in one single table

av_table3<-round(rbind(singleind_av$mean_gincdif_num,indwfam_av$mean_gincdif_num,
                       lwp_av$mean_gincdif_num,nlwp_av$mean_gincdif_num,nochild_av$mean_gincdif_num,
                       child12_av$mean_gincdif_num,child3_av$mean_gincdif_num,nochild_low_av$mean_gincdif_num,
                       child12_low_av$mean_gincdif_num,child3_low_av$mean_gincdif_num,sp_av$mean_gincdif_num,
                       cwc_av$mean_gincdif_num,cnc_av$mean_gincdif_num,
                       c(nmarried_av$mean_gincdif_num[1:16],NA,nmarried_av$mean_gincdif_num[17:22]),
                       c(divorced_av$mean_gincdif_num[1:16],NA,divorced_av$mean_gincdif_num[17:22]),
                       c(widowed_av$mean_gincdif_num[1:16],NA,widowed_av$mean_gincdif_num[17:22]),
                       marriedcohab_av$mean_gincdif_num,nmarriedcohab_av$mean_gincdif_num,
                       c(singlearn_av$mean_gincdif_num[1:3],NA,singlearn_av$mean_gincdif_num[4:22]),
                       c(supearn_av$mean_gincdif_num[1:3],NA,supearn_av$mean_gincdif_num[4:22]),
                       c(doubearn_av$mean_gincdif_num[1:3],NA,doubearn_av$mean_gincdif_num[4:22]),
                       lhhi_av$mean_gincdif_num,mhhi_av$mean_gincdif_num,hhhi_av$mean_gincdif_num),2)

rownames(av_table3)=c("Single individual","Individual with family","Lives with partner","Does not live with partner",
                    "No children","1-2 children","3 or more children","No children low income","1-2 children low income",
                    "3 or more children low income","Single parent","Couple with children","Couple without children",
                    "Never married (single parent)","Divorced (single parent)","Widowed (single parent)",
                    "Married couple","Cohabiting couple","Single-earner","Supplementary-earner","Double-earner",
                    "Low HH income","Middle HH income","High HH income")

colnames(av_table3)=levels(data_p3$cntry)

av_table3<-data.frame(av_table3)
av_table3$TOTAL<-round(c(mean(data_p3$gincdif_num[which(data_p3$single_family==0)],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$single_family==1)],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$lwpartner=="yes")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$lwpartner=="no")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$nmchildren_grp2=="no children")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$nmchildren_grp2=="1-2 children")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$nmchildren_grp2=="3 children or more")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="Low" & data_p3$nmchildren_grp2=="no children")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="Low" & data_p3$nmchildren_grp2=="1-2 children")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="Low" & data_p3$nmchildren_grp2=="3 children or more")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Single parent")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Couple parents")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Couple no children")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Single parent" & 
                                                        data_p3$maritalstatus_grp=="Never married or in civil union")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Single parent" & 
                                                        data_p3$maritalstatus_grp=="Legally separated or divorced")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$parentpartner=="Single parent" & 
                                                        data_p3$maritalstatus_grp=="Widowed/Civil partner died")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$lwpartner=="yes" & 
                                                        data_p3$maritalstatus_bin=="Married or in civil union")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$lwpartner=="yes" & 
                                                        data_p3$maritalstatus_bin=="Not married or in civil union")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$earnings_dist=="Single-earner")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$earnings_dist=="Supplementary-earner")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$earnings_dist=="Double-earner")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="Low")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="Middle")],na.rm=T),
                         mean(data_p3$gincdif_num[which(data_p3$hinctnta_grp=="High")],na.rm=T)
                         ),2)

write.csv(av_table3,file="av_table3.csv")
  
# Differences in means for the pre-defined family types
family_type_averages<- data_p3 %>%
  group_by(cntry) %>%
  group_by(family_type_simple) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

####################################################################################################

##
# 2) CORRELATIONS BETWEEN FAMILY TYPE INDICATORS AND PREFERENCES
##

# Single individuals (0) vs individuals with family (1)
singlefamily_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$single_family,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$single_family[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$single_family[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$single_family[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$single_family[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$single_family[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$single_family[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$single_family[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$single_family[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$single_family[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$single_family[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$single_family[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$single_family[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$single_family[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$single_family[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$single_family[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$single_family[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$single_family[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$single_family[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$single_family[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$single_family[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$single_family[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$single_family[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$single_family[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$single_family),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$single_family[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$single_family[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$single_family[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$single_family[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$single_family[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$single_family[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$single_family[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$single_family[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$single_family[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$single_family[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$single_family[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$single_family[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$single_family[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$single_family[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$single_family[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$single_family[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$single_family[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$single_family[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$single_family[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$single_family[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$single_family[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$single_family[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$single_family[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(singlefamily_cor)<-c("Total",levels(data_p3$cntry))
rownames(singlefamily_cor)<-c("With family Estimate","With family p.value")

# Living with partner
LWP_cor<-data.frame(rbind(
c(cor.test(data_p3$gincdif_num,(as.numeric(data_p3$lwpartner)-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="AT")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="BE")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="BG")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="CY")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="CZ")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="DE")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="DK")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="EE")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="ES")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="FI")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="FR")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="HR")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="HU")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="IE")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="IT")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="LT")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="LV")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="NL")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="PL")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="PT")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SE")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SI")])-1),use="complete.obs")$estimate,
cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SK")])-1),use="complete.obs")$estimate),
round(c(cor.test(data_p3$gincdif_num,(as.numeric(data_p3$lwpartner)-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="AT")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="BE")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="BG")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="CY")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="CZ")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="DE")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="DK")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="EE")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="ES")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="FI")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="FR")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="HR")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="HU")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="IE")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="IT")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="LT")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="LV")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="NL")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="PL")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="PT")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SE")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SI")])-1),use="complete.obs")$p.value,
  cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(as.numeric(data_p3$lwpartner[which(data_p3$cntry=="SK")])-1),use="complete.obs")$p.value),3)
))

colnames(LWP_cor)<-c("Total",levels(data_p3$cntry))
rownames(LWP_cor)<-c("LWP Estimate","LWP p.value")

# Married cohabiting couples vs non-married cohabiting couples
cohabmarried_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$married_cohabiting,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$married_cohabiting[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$married_cohabiting[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$married_cohabiting[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$married_cohabiting[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$married_cohabiting[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$married_cohabiting[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$married_cohabiting[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$married_cohabiting[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$married_cohabiting[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$married_cohabiting[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$married_cohabiting[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$married_cohabiting[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$married_cohabiting[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$married_cohabiting[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$married_cohabiting[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$married_cohabiting[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$married_cohabiting[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$married_cohabiting[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$married_cohabiting[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$married_cohabiting[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$married_cohabiting[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$married_cohabiting[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$married_cohabiting[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$married_cohabiting),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$married_cohabiting[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$married_cohabiting[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$married_cohabiting[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$married_cohabiting[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$married_cohabiting[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$married_cohabiting[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$married_cohabiting[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$married_cohabiting[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$married_cohabiting[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$married_cohabiting[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$married_cohabiting[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$married_cohabiting[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$married_cohabiting[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$married_cohabiting[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$married_cohabiting[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$married_cohabiting[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$married_cohabiting[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$married_cohabiting[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$married_cohabiting[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$married_cohabiting[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$married_cohabiting[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$married_cohabiting[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$married_cohabiting[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(cohabmarried_cor)<-c("Total",levels(data_p3$cntry))
rownames(cohabmarried_cor)<-c("Cohabiting Married Estimate","Cohabiting Married p.value")

# Married/all other categories
Married_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,(as.numeric(data_p3$maritalstatus_bin)-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="AT")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="BE")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="BG")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="CY")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="CZ")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="DE")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="DK")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="EE")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="ES")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="FI")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="FR")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="HR")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="HU")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="IE")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="IT")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="LT")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="LV")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="NL")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="PL")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="PT")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SE")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SI")])-1),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SK")])-1),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(as.numeric(data_p3$maritalstatus_bin)-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="AT")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="BE")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="BG")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="CY")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="CZ")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="DE")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="DK")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="EE")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="ES")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="FI")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="FR")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="HR")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="HU")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="IE")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="IT")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="LT")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="LV")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="NL")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="PL")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="PT")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SE")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SI")])-1),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(as.numeric(data_p3$maritalstatus_bin[which(data_p3$cntry=="SK")])-1),use="complete.obs")$p.value),3)
))

colnames(Married_cor)<-c("Total",levels(data_p3$cntry))
rownames(Married_cor)<-c("Married Estimate","Married p.value")

# Divorced compared to married
divorced_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$divorced_married,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$divorced_married[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$divorced_married[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$divorced_married[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$divorced_married[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$divorced_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$divorced_married[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$divorced_married[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$divorced_married[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$divorced_married[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$divorced_married[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$divorced_married[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$divorced_married[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$divorced_married[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$divorced_married[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$divorced_married[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$divorced_married[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$divorced_married[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$divorced_married[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$divorced_married[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$divorced_married[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$divorced_married[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$divorced_married[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$divorced_married[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$divorced_married),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$divorced_married[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$divorced_married[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$divorced_married[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$divorced_married[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$divorced_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$divorced_married[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$divorced_married[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$divorced_married[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$divorced_married[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$divorced_married[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$divorced_married[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$divorced_married[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$divorced_married[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$divorced_married[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$divorced_married[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$divorced_married[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$divorced_married[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$divorced_married[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$divorced_married[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$divorced_married[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$divorced_married[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$divorced_married[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$divorced_married[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(divorced_cor)<-c("Total",levels(data_p3$cntry))
rownames(divorced_cor)<-c("Divorced Estimate","Divorced p.value")

# Widowed compared to married
widowed_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$widowed_married,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$widowed_married[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$widowed_married[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$widowed_married[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$widowed_married[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$widowed_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$widowed_married[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$widowed_married[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$widowed_married[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$widowed_married[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$widowed_married[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$widowed_married[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$widowed_married[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$widowed_married[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$widowed_married[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$widowed_married[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$widowed_married[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$widowed_married[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$widowed_married[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$widowed_married[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$widowed_married[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$widowed_married[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$widowed_married[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$widowed_married[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$widowed_married),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$widowed_married[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$widowed_married[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$widowed_married[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$widowed_married[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$widowed_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$widowed_married[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$widowed_married[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$widowed_married[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$widowed_married[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$widowed_married[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$widowed_married[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$widowed_married[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$widowed_married[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$widowed_married[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$widowed_married[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$widowed_married[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$widowed_married[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$widowed_married[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$widowed_married[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$widowed_married[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$widowed_married[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$widowed_married[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$widowed_married[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(widowed_cor)<-c("Total",levels(data_p3$cntry))
rownames(widowed_cor)<-c("Widowed Estimate","Widowed p.value")

# Single compared to married
single_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$single_married,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$single_married[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$single_married[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$single_married[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$single_married[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$single_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$single_married[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$single_married[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$single_married[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$single_married[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$single_married[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$single_married[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$single_married[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$single_married[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$single_married[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$single_married[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$single_married[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$single_married[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$single_married[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$single_married[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$single_married[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$single_married[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$single_married[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$single_married[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$single_married),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$single_married[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$single_married[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$single_married[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$single_married[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$single_married[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$single_married[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$single_married[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$single_married[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$single_married[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$single_married[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$single_married[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$single_married[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$single_married[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$single_married[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$single_married[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$single_married[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$single_married[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$single_married[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$single_married[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$single_married[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$single_married[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$single_married[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$single_married[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(single_cor)<-c("Total",levels(data_p3$cntry))
rownames(single_cor)<-c("Single Estimate","Single p.value")

# Number of minor children in HH
nchild_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$nmchildren,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$nmchildren[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$nmchildren[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$nmchildren[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$nmchildren[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$nmchildren[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$nmchildren[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$nmchildren[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$nmchildren[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$nmchildren[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$nmchildren[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$nmchildren[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$nmchildren[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$nmchildren[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$nmchildren[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$nmchildren[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$nmchildren[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$nmchildren[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$nmchildren[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$nmchildren[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$nmchildren[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$nmchildren[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$nmchildren[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$nmchildren[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$nmchildren),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$nmchildren[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$nmchildren[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$nmchildren[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$nmchildren[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$nmchildren[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$nmchildren[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$nmchildren[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$nmchildren[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$nmchildren[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$nmchildren[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$nmchildren[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$nmchildren[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$nmchildren[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$nmchildren[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$nmchildren[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$nmchildren[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$nmchildren[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$nmchildren[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$nmchildren[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$nmchildren[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$nmchildren[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$nmchildren[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$nmchildren[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(nchild_cor)<-c("Total",levels(data_p3$cntry))
rownames(nchild_cor)<-c("N children Estimate","N children p.value")

# Single parents (1) vs couple parents (0)
singleparent_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$singleparent,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$singleparent[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$singleparent[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$singleparent[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$singleparent[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$singleparent[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$singleparent[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$singleparent[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$singleparent[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$singleparent[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$singleparent[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$singleparent[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$singleparent[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$singleparent[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$singleparent[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$singleparent[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$singleparent[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$singleparent[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$singleparent[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$singleparent[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$singleparent[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$singleparent[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$singleparent[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$singleparent[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$singleparent),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$singleparent[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$singleparent[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$singleparent[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$singleparent[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$singleparent[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$singleparent[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$singleparent[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$singleparent[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$singleparent[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$singleparent[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$singleparent[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$singleparent[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$singleparent[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$singleparent[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$singleparent[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$singleparent[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$singleparent[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$singleparent[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$singleparent[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$singleparent[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$singleparent[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$singleparent[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$singleparent[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(singleparent_cor)<-c("Total",levels(data_p3$cntry))
rownames(singleparent_cor)<-c("Single parent Estimate","Single parent p.value")

# HH income
hinc_cor<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$hinctnta,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$hinctnta[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$hinctnta[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$hinctnta[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$hinctnta[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$hinctnta[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$hinctnta[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$hinctnta[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$hinctnta[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$hinctnta[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$hinctnta[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$hinctnta[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$hinctnta[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$hinctnta[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$hinctnta[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$hinctnta[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$hinctnta[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$hinctnta[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$hinctnta[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$hinctnta[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$hinctnta[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$hinctnta[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$hinctnta[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$hinctnta[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$hinctnta),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$hinctnta[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$hinctnta[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$hinctnta[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$hinctnta[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$hinctnta[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$hinctnta[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$hinctnta[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$hinctnta[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$hinctnta[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$hinctnta[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$hinctnta[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$hinctnta[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$hinctnta[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$hinctnta[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$hinctnta[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$hinctnta[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$hinctnta[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$hinctnta[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$hinctnta[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$hinctnta[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$hinctnta[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$hinctnta[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$hinctnta[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(hinc_cor)<-c("Total",levels(data_p3$cntry))
rownames(hinc_cor)<-c("HH income Estimate","HH income p.value")

# Distribution of income: double-earner (0) vs single-earner (1)
single_earner<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$double_single,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$double_single[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$double_single[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$double_single[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    # cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$double_single[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    NA,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$double_single[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$double_single[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$double_single[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$double_single[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$double_single[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$double_single[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$double_single[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$double_single[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$double_single[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$double_single[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$double_single[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$double_single[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$double_single[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$double_single[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$double_single[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$double_single[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$double_single[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$double_single[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$double_single[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$double_single),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$double_single[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$double_single[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$double_single[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          # cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$double_single[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          NA,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$double_single[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$double_single[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$double_single[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$double_single[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$double_single[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$double_single[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$double_single[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$double_single[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$double_single[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$double_single[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$double_single[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$double_single[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$double_single[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$double_single[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$double_single[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$double_single[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$double_single[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$double_single[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$double_single[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(single_earner)<-c("Total",levels(data_p3$cntry))
rownames(single_earner)<-c("Single earner Estimate","Single earner p.value")

# Distribution of income: supplementary-earner (0) vs single-earner (1)
supplementary_earner<-data.frame(rbind(
  c(cor.test(data_p3$gincdif_num,data_p3$double_sup,use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$double_sup[which(data_p3$cntry=="AT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$double_sup[which(data_p3$cntry=="BE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$double_sup[which(data_p3$cntry=="BG")]),use="complete.obs")$estimate,
    # cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$double_sup[which(data_p3$cntry=="CY")]),use="complete.obs")$estimate,
    NA,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$double_sup[which(data_p3$cntry=="CZ")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$double_sup[which(data_p3$cntry=="DE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$double_sup[which(data_p3$cntry=="DK")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$double_sup[which(data_p3$cntry=="EE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$double_sup[which(data_p3$cntry=="ES")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$double_sup[which(data_p3$cntry=="FI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$double_sup[which(data_p3$cntry=="FR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$double_sup[which(data_p3$cntry=="HR")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$double_sup[which(data_p3$cntry=="HU")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$double_sup[which(data_p3$cntry=="IE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$double_sup[which(data_p3$cntry=="IT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$double_sup[which(data_p3$cntry=="LT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$double_sup[which(data_p3$cntry=="LV")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$double_sup[which(data_p3$cntry=="NL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$double_sup[which(data_p3$cntry=="PL")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$double_sup[which(data_p3$cntry=="PT")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$double_sup[which(data_p3$cntry=="SE")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$double_sup[which(data_p3$cntry=="SI")]),use="complete.obs")$estimate,
    cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$double_sup[which(data_p3$cntry=="SK")]),use="complete.obs")$estimate),
  round(c(cor.test(data_p3$gincdif_num,(data_p3$double_sup),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="AT")],(data_p3$double_sup[which(data_p3$cntry=="AT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BE")],(data_p3$double_sup[which(data_p3$cntry=="BE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="BG")],(data_p3$double_sup[which(data_p3$cntry=="BG")]),use="complete.obs")$p.value,
          # cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CY")],(data_p3$double_sup[which(data_p3$cntry=="CY")]),use="complete.obs")$p.value,
          NA,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="CZ")],(data_p3$double_sup[which(data_p3$cntry=="CZ")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DE")],(data_p3$double_sup[which(data_p3$cntry=="DE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="DK")],(data_p3$double_sup[which(data_p3$cntry=="DK")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="EE")],(data_p3$double_sup[which(data_p3$cntry=="EE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="ES")],(data_p3$double_sup[which(data_p3$cntry=="ES")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FI")],(data_p3$double_sup[which(data_p3$cntry=="FI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="FR")],(data_p3$double_sup[which(data_p3$cntry=="FR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HR")],(data_p3$double_sup[which(data_p3$cntry=="HR")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="HU")],(data_p3$double_sup[which(data_p3$cntry=="HU")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IE")],(data_p3$double_sup[which(data_p3$cntry=="IE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="IT")],(data_p3$double_sup[which(data_p3$cntry=="IT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LT")],(data_p3$double_sup[which(data_p3$cntry=="LT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="LV")],(data_p3$double_sup[which(data_p3$cntry=="LV")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="NL")],(data_p3$double_sup[which(data_p3$cntry=="NL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PL")],(data_p3$double_sup[which(data_p3$cntry=="PL")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="PT")],(data_p3$double_sup[which(data_p3$cntry=="PT")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SE")],(data_p3$double_sup[which(data_p3$cntry=="SE")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SI")],(data_p3$double_sup[which(data_p3$cntry=="SI")]),use="complete.obs")$p.value,
          cor.test(data_p3$gincdif_num[which(data_p3$cntry=="SK")],(data_p3$double_sup[which(data_p3$cntry=="SK")]),use="complete.obs")$p.value),3)
))

colnames(supplementary_earner)<-c("Total",levels(data_p3$cntry))
rownames(supplementary_earner)<-c("Supplementary earner Estimate","Supplementary earner p.value")


cor_table3<-round(rbind(singlefamily_cor,LWP_cor,cohabmarried_cor,Married_cor,divorced_cor,widowed_cor,single_cor,nchild_cor,
                        singleparent_cor,hinc_cor,single_earner,supplementary_earner),3)

write.csv(cor_table3,file="cor_table3.csv")

####################

# COUNTRY SPECIFIC LINEAR MODELS

#  INDIVIDUALS WITH FAMILY (1) VS SINGLE INDIVIDUALS (0)
coef_sf<-rbind(c(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["single_family"],
           lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["single_family"]),
           c(summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["single_family",4],
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["single_family",4], 
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["single_family",4],
           summary(lm(gincdif_num~single_family+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["single_family",4])) 
rownames(coef_sf)=c("Individuals with family (ref=single individual)","")
colnames(coef_sf)=c("Total",levels(data_p3$cntry))

#  LIVING WITH PARTNER
coef_lwp<-rbind(c(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["lwpartneryes"],
                 lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["lwpartneryes"]),
                 c(summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["lwpartneryes",4],
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["lwpartneryes",4], 
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["lwpartneryes",4],
                 summary(lm(gincdif_num~lwpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["lwpartneryes",4])) 
rownames(coef_lwp)=c("Living with partner","")
colnames(coef_lwp)=c("Total",levels(data_p3$cntry))


#  ALL PARENT-PARTNER COMBINATIONS
coef_singleparent<-rbind(c(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["parentpartnerSingle parent"],
                  lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["parentpartnerSingle parent"]),
                c(summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["parentpartnerSingle parent",4],
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["parentpartnerSingle parent",4], 
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["parentpartnerSingle parent",4],
                  summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["parentpartnerSingle parent",4])) 
rownames(coef_singleparent)=c("Single parent (ref=single no children)","")
colnames(coef_singleparent)=c("Total",levels(data_p3$cntry))

coef_couplenc<-rbind(c(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["parentpartnerCouple no children"],
                           lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["parentpartnerCouple no children"]),
                         c(summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["parentpartnerCouple no children",4],
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["parentpartnerCouple no children",4], 
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["parentpartnerCouple no children",4],
                           summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["parentpartnerCouple no children",4])) 
rownames(coef_couplenc)=c("Couple no children (ref=single no children)","")
colnames(coef_couplenc)=c("Total",levels(data_p3$cntry))

coef_coupleparents<-rbind(c(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["parentpartnerCouple parents"],
                       lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["parentpartnerCouple parents"]),
                     c(summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["parentpartnerCouple parents",4],
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["parentpartnerCouple parents",4], 
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["parentpartnerCouple parents",4],
                       summary(lm(gincdif_num~parentpartner+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["parentpartnerCouple parents",4])) 
rownames(coef_coupleparents)=c("Couple parents (ref=single no children)","")
colnames(coef_coupleparents)=c("Total",levels(data_p3$cntry))

#  DIVORCED VS WIDOWED VS MARRIED VS NEVER MARRIED (ONLY FOR SINGLE PARENTS?)

#  NUMBER OF CHILDREN

coef_nchild<-rbind(c(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["nmchildren"],
                     lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["nmchildren"]),
                   c(summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["nmchildren",4],
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["nmchildren",4], 
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["nmchildren",4],
                     summary(lm(gincdif_num~nmchildren+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["nmchildren",4])) 
rownames(coef_nchild)=c("Number of minor children in HH","")
colnames(coef_nchild)=c("Total",levels(data_p3$cntry))


coef_12child<-rbind(c(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["nmchildren_grp21-2 children"],
                            lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["nmchildren_grp21-2 children"]),
                          c(summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["nmchildren_grp21-2 children",4],
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["nmchildren_grp21-2 children",4], 
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["nmchildren_grp21-2 children",4],
                            summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["nmchildren_grp21-2 children",4])) 
rownames(coef_12child)=c("1-2 children","")
colnames(coef_12child)=c("Total",levels(data_p3$cntry))


coef_child23<-rbind(c(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["nmchildren_grp23 children or more"],
                      lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["nmchildren_grp23 children or more"]),
                    c(summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["nmchildren_grp23 children or more",4],
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["nmchildren_grp23 children or more",4], 
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["nmchildren_grp23 children or more",4],
                      summary(lm(gincdif_num~nmchildren_grp2+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["nmchildren_grp23 children or more",4])) 
rownames(coef_child23)=c("3 children or more","")
colnames(coef_child23)=c("Total",levels(data_p3$cntry))

#  DISTRIBUTION OF EARNINGS BETWEEN PARTNERS

coef_supearner<-rbind(c(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["earnings_distSupplementary-earner"],
                            #lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["earnings_distSupplementary-earner"],
                            NA,
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["earnings_distSupplementary-earner"],
                            lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["earnings_distSupplementary-earner"]),
                          c(summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["earnings_distSupplementary-earner",4],
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            #summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            NA,
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["earnings_distSupplementary-earner",4], 
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["earnings_distSupplementary-earner",4],
                            summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["earnings_distSupplementary-earner",4])) 
rownames(coef_supearner)=c("Supplementary-earner (ref=Single-earner)","")
colnames(coef_supearner)=c("Total",levels(data_p3$cntry))

coef_doubearner<-rbind(c(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3)$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),])$coefficients["earnings_distDouble-earner"],
                        #lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),])$coefficients["earnings_distDouble-earner"],
                        NA,
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),])$coefficients["earnings_distDouble-earner"],
                        lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),])$coefficients["earnings_distDouble-earner"]),
                      c(summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3))$coefficients["earnings_distDouble-earner",4],
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="AT"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BE"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="BG"),]))$coefficients["earnings_distDouble-earner",4], 
                        #summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CY"),]))$coefficients["earnings_distDouble-earner",4], 
                        NA,
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="CZ"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DE"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="DK"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="EE"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="ES"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FI"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="FR"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HR"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="HU"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IE"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="IT"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LT"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="LV"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="NL"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PL"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="PT"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SE"),]))$coefficients["earnings_distDouble-earner",4], 
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SI"),]))$coefficients["earnings_distDouble-earner",4],
                        summary(lm(gincdif_num~earnings_dist+agea+gndr+hinctnta+high_second_edu+rlgdgr+freehms,weights=anweight,data=data_p3[which(data_p3$cntry=="SK"),]))$coefficients["earnings_distDouble-earner",4])) 
rownames(coef_doubearner)=c("Double-earner (ref=Single-earner)","")
colnames(coef_doubearner)=c("Total",levels(data_p3$cntry))

coef.table3<-round(rbind(coef_sf,coef_lwp,coef_singleparent,coef_couplenc,coef_coupleparents,coef_nchild,coef_12child,coef_child23,
                         coef_supearner,coef_doubearner),2)

write.csv(coef.table3,file="coef.table3.csv")


#############################

# RANDOM-SLOPE MODELS

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

# Single individuals (0) vs individuals with family (1)
mm_singlefamily<-lmer(gincdif_num~single_family+agea+gndr+hinctnta+earnings_dist+
                        high_second_edu+rlgdgr+freehms+(1+single_family|cntry),
                data=data_p3,weights=anweight)

summary(mm_singlefamily) # Overall, ind. with family are more redistributive

plot_model(mm_singlefamily,type="re",terms=c("single_family"))

plot_data2<-plot_matrix(mm_singlefamily)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_singlefamily)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Having a Family (vs. SIs) on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

#######################################################

# Adding living with a partner and having children
mm_singlefamily2<-lmer(gincdif_num~lwpartner+hasmkids_bin+agea+gndr+hinctnta+earnings_dist+
                        high_second_edu+rlgdgr+freehms+(1+lwpartner+hasmkids_bin|cntry),
                      data=data_p3,weights=anweight)

summary(mm_singlefamily2) # Overall, ind. with family are more redistributive

plot_model(mm_singlefamily2,type="re",terms=c("lwpartner"))

plot_data2<-plot_matrix(mm_singlefamily2)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_singlefamily2)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Living with a Partner on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

######

# Plot effect for having children
mm_singlefamily3<-lmer(gincdif_num~hasmkids_bin+lwpartner+agea+gndr+hinctnta+earnings_dist+
                         high_second_edu+rlgdgr+freehms+(1+hasmkids_bin+lwpartner|cntry),
                       data=data_p3,weights=anweight)

summary(mm_singlefamily3) # Overall, ind. with family are more redistributive

plot_model(mm_singlefamily3,type="re",terms=c("lwpartner"))

plot_data2<-plot_matrix(mm_singlefamily3)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_singlefamily3)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Having Children on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

######

# Plot effect for number of children
mm_singlefamily4<-lmer(gincdif_num~nmchildren+lwpartner+agea+gndr+hinctnta+earnings_dist+
                         high_second_edu+rlgdgr+freehms+(1+nmchildren+lwpartner|cntry),
                       data=data_p3,weights=anweight)

summary(mm_singlefamily4) # Overall, ind. with family are more redistributive

plot_model(mm_singlefamily4,type="re",terms=c("lwpartner"))

plot_data2<-plot_matrix(mm_singlefamily4)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_singlefamily4)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Number of Children on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip()  # In Netherlands, Denmark, Czechia and Germany, having one more child makes you less redistributive.
                # In Spain, having one more child makes you more redistributive

#######################################################

# Living with partner (1) vs not living with partner (0)
mm_lwp<-lmer(gincdif_num~lwpartner+agea+gndr+hinctnta+(1+lwpartner|cntry),
                      data=data_p3,weights=anweight)

summary(mm_lwp)

plot_model(mm_lwp,type="re",terms=c("lwpartneryes"))

plot_data2<-plot_matrix(mm_lwp)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_lwp)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Living with a Partner on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

######################################################

# Couples with children (1) vs Single parents (0)

data_p3$couple_singleparents<-factor(nrow(data_p3),levels=c("Couple Parents","Single Parent"))
data_p3$couple_singleparents[which(data_p3$parentpartner=="Single no children")]=NA
data_p3$couple_singleparents[which(data_p3$parentpartner=="Couple no children")]=NA
data_p3$couple_singleparents[which(data_p3$parentpartner=="Single parent")]="Single Parent"
data_p3$couple_singleparents[which(data_p3$parentpartner=="Couple parents")]="Couple Parents"

mm_sp<-lmer(gincdif_num~couple_singleparents+agea+gndr+hinctnta+(1+couple_singleparents|cntry),
             data=data_p3,weights=anweight)

summary(mm_sp)

plot_model(mm_sp,type="re",terms=c("parentpartner"))

plot_data2<-plot_matrix(mm_sp)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_sp)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Being a Single Parent (vs. Couple Parents) on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

#####################################################

# Number of children
mm_nchild<-lmer(gincdif_num~nmchildren+agea+gndr+hinctnta+(1+nmchildren|cntry),
                 data=data_p3,weights=anweight)

summary(mm_nchild)

plot_model(mm_nchild,type="re",terms=c("nmchildren"))

plot_data2<-plot_matrix(mm_nchild)

# Plot it
ggplot(plot_data2, aes(x = reorder(country, total_effect), y = total_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = fixef(mm_nchild)[2], linetype = "dashed", color = "red") +  # Add general coefficient line
  labs(title = "Effect of Number of Minor Children in HH on Preferences by Country",
       x = "Country",
       y = "Effect") +
  theme_minimal(base_family="Times New Roman") +
  coord_flip() 

#######################################################

# Distribution of market income in the HH
sup_earn<-lmer(gincdif_num~earnings_dist+agea+gndr+hinctnta+(1+earnings_dist|cntry),
                data=data_p3[which(data_p3$earnings_dist!="Double-earner"),],weights=anweight)

summary(sup_earn)

plot_model(sup_earn,type="re",terms=c("earnings_dist"))

########################################################

dou_earn<-lmer(gincdif_num~earnings_dist+agea+gndr+hinctnta+(1+earnings_dist|cntry),
               data=data_p3[which(data_p3$earnings_dist!="Supplementary-earner"),],weights=anweight)

plot_model(dou_earn,type="re",terms=c("earnings_dist"))

########################################################

diff_model<-lm(gincdif_num~Diff+agea+gndr+hinctnta,
                 data=data_p3,weights=anweight)

summary(diff_model)

library(ordinal)

diff_model2<-clm(gincdif~Diff+agea+gndr+hinctnta,
               data=data_p3,weights=anweight,link="logit")

plot_model(dou_earn,type="re",terms=c("earnings_dist"))