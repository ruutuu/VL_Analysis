
library(readr)

library(readxl)

# meta-analysis packages
library(meta)
library(metafor )
library(metagen)
library(metasens) 

# graphics packages
library(tidyverse)
library(reshape)
library(doBy)
library(stringr)
library(ggpubr)
library(patchwork)
setwd("~/IDDO/Data")
vl_main <- read_csv("vldata_clean.csv")

df<-vl_main[,c('pub_id',
               'st_intv_arms',
               'redcap_repeat_instance',
               'st_followup',
               'eff_arm_icure_num',
               'eff_arm_icure_days',
               'eff_arm_relapse_num',
               'eff_arm_relapse_days',
               'st_status',
               'st_design',
               'st_allocation',
               'st_country_num',	
               'st_country1',
               'st_region',
               'sa_num_tx'	,
               'sa_num_followup',
               'sa_combi',
               'sa_intv1_drug',
               'sa_intv1_dose_mgkg',
               'sa_intv1_freq_discrete',
               'sa_intv1_dura',
               'sa_intv2_drug',
               'sa_intv2_dose_mgkg',
               'sa_intv2_freq_discrete',
               'sa_intv2_dura')]

df$id<-paste0(df$pub_id,"_",df$redcap_repeat_instance)
df$eff_arm_icure_num<-as.numeric(df$eff_arm_icure_num)
df$eff_arm_relapse_num<-as.numeric(df$eff_arm_relapse_num)
df$sa_num_followup<-as.numeric(df$sa_num_followup)

#df<-df[,c(1,24,2:23)]

#Subset unpublished studies
df<-subset(df,df$st_status=="Published")

df$mono<-ifelse(is.na(df$sa_intv2_drug),1,0 )
df$drug<-ifelse(df$mono==1,df$sa_intv1_drug,
                paste0(df$sa_intv1_drug,' & ',df$sa_intv2_drug))
druglist<-unique(df$drug)

df1<-subset(df,df$eff_arm_icure_num>0)
df1<-subset(df1,df1$eff_arm_relapse_num>0)


#Check the proportions in individual studies
arm_prop<-df1$eff_arm_relapse_num/df1$eff_arm_icure_num
hist(arm_prop)

#Skewed distribution of relapse proportions. And the values are very small as well. 
#Logit transformation prefered in such cases.

meta_relapse <- metaprop(
  data = df1,
  event =  eff_arm_relapse_num,
  n= eff_arm_icure_num, 
  sm = "PLOGIT",
  exclude = NULL
)
meta_relapse

#To overcome the limitations of logit transformation, trying Arcsine transformation
meta_relapse <- metaprop(
  data = df1,
  event =  eff_arm_relapse_num,
  n= eff_arm_icure_num, 
  sm = "PAS",
  exclude = NULL
)
meta_relapse
#--------------------------------
# Subgroup analysis by Primary intervention drug
#--------------------------------
update.meta(meta_relapse,comb.random = TRUE, comb.fixed = F,
            byvar=sa_intv1_drug)



df_mil<-subset(df1,df1$sa_intv1_drug=="Miltefosine")

meta_relapse_mil <- metaprop(
  data = df_mil,
  event =  eff_arm_relapse_num,
  n= eff_arm_icure_num, exclude = NULL
)

#--------------------------------
# Subgroup analysis by Primary intervention drug
#--------------------------------
update.meta(meta_relapse_mil,comb.random = TRUE, comb.fixed = F,
            byvar=redcap_repeat_instance)

