#======================================================================================================================
# Title		:	Literature review of gender distribution in VL clinical trials
# Data version	:	23-Dec-2019 (from Sauman Singh)
# Author	:	Prabin Dahal
# Script Date	: 	02-02-2021
#======================================================================================================================
#rm(list=ls())

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


#=================================
# Read data on gender distribution
#=================================
setwd("~/IDDO/Data")
dat2 <- read_excel("vl_data.xlsx", 
                   sheet = "Final data for R analysis"
)

# Phase of the studies
dat2$phase_group <-  ifelse(dat2$phase=="Unclear","Unclear",
                            ifelse(dat2$phase=="Phase 4","Phase IV","Phase II-III"))

#------------------------------------
# summarise by patient characteristics
#------------------------------------
# overall number of patients
dat2  %>% 
  dplyr::summarise(
    n_males 	= sum(n_males),
    males_prop 	= sum(n_males)/sum(n_total),
    n_females 	= sum(n_females),
    females_prop = sum(n_females)/sum(n_total),
    n_patients	= sum(n_total)
  )

# summarise by region
dat2 %>% 
  dplyr::group_by(Study.Region) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    min_year 	= min(pub_year),
    max_year 	= max(pub_year),
    n_males 	= sum(n_males),
    n_females 	= sum(n_females),
    n_patients	= sum(n_total)
  )

# summary by HIV co-infections
dat2  %>% 
  dplyr::group_by(HIV) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summary by HIV co-infections and region
dat2  %>% 
  dplyr::group_by(Study.Region,HIV) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by age-range
dat2  %>% 
  dplyr::group_by(age_range) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by age-range and region
dat2  %>% 
  dplyr::group_by(Study.Region,age_range) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by pregnancy status
dat2  %>% 
  #dplyr::group_by(pregnancy) %>%
  dplyr::group_by(pregnancy) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by pregnancy status and region
dat2  %>% 
  #dplyr::group_by(Study.Region,pregnancy) %>%
  dplyr::group_by(pregnancy) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# detailed summary by contraception use required for child bearing age
dat2  %>% 
  dplyr::group_by(exclusion_child_bearing_age_women) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# detailed summary by phase of the study
dat2  %>% 
  dplyr::group_by(phase_group) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

#-------------------------------------------------------
# summarise by study design and conduct paramaters
#-------------------------------------------------------

# summary by the year the study was conducted
dat2  %>% 
  dplyr::group_by(pub_year1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summary by the year the study was conducted and region
dat2  %>% 
  dplyr::group_by(Study.Region,pub_year1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# Group single arm and non-randomised studies as non-randomised
dat2$randomisation_1 <- dat2$randomisation
dat2$randomisation_1[dat2$randomisation=="Single-armed"]<- "Non-randomised"

# summarise by randomisation status
dat2  %>% 
  dplyr::group_by(randomisation_1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# Detailed summary by miltefosine used or not
dat2  %>% 
  dplyr::group_by(miltefosine) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_patients	= sum(n_total)
  )

# Detailed summary by FDA drug class used or not
dat2  %>% 
  dplyr::group_by(worst_fda_class) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_patients	= sum(n_total)
  )

####################################################
# Carry out meta-analysis of proportion 
####################################################
(meta.prop <- metaprop(
  data = dat2,
  n_males, 
  n_total, 
  prediction=TRUE
)
)
#--------------------------------
# Subgroup analysis by region
#--------------------------------
update.meta(meta.prop,comb.random = TRUE, comb.fixed = F,
            byvar=Study.Region)

#--------------------------------
# Subgroup analysis by age-group
#--------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=age_range) 

#-----------------------------------
# Subgroup analysis by time range
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=pub_year1)

#-----------------------------------
# Subgroup analysis by HIV status
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=HIV)

#-----------------------------------
# Subgroup analysis by pregnancy
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=pregnancy)

#-----------------------------------------------------------
# Subgroup analysis by exclusion_child_bearing_age_women
#-----------------------------------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=exclusion_child_bearing_age_women)

#-----------------------------------
# Subgroup analysis by miltefosine
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=miltefosine) 

#-----------------------------------
# Subgroup analysis by FDA drug class
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=worst_fda_class) 

#-----------------------------------
# Subgroup analysis by randomisation
#-----------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=randomisation_1)

#------------------------------------------
# Subgroup analysis by phase of the study
#------------------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
            byvar=phase_group)

#=================================================
# Adjusted for potential publication biases 
#=================================================
(meta.prop <- metaprop(
  data = dat2,
  n_males, 
  n_total, 
  prediction=TRUE
)
)
# Egger's test for assessing publication bias
# method.bias "rank", "linreg", "mm", or "score"
metabias(meta.prop)
trimfill(meta.prop)

#################################################################
# Carry out meta-analysis of proportion for studies from ISC
#################################################################
dat2_ISC <- dat2[which(dat2$Study.Region=="India Subcontinent"),]
dat2_ISC <- droplevels(dat2_ISC)


# publication year
dat2_ISC  %>% 
  dplyr::group_by(pub_year1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by randomisation status
dat2_ISC  %>% 
  dplyr::group_by(randomisation_1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# Detailed summary by FDA drug class used or not
dat2_ISC  %>% 
  dplyr::group_by(worst_fda_class) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_patients	= sum(n_total)
  )

# summarise by pregnancy status
dat2_ISC  %>% 
  #dplyr::group_by(pregnancy) %>%
  dplyr::group_by(pregnancy) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# detailed summary by contraception use required for child bearing age
dat2_ISC  %>% 
  dplyr::group_by(exclusion_child_bearing_age_women) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# HIV
dat2_ISC  %>% 
  dplyr::group_by(HIV) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )


(meta.prop.isc <- metaprop(
  data = dat2_ISC,
  n_males, 
  n_total, 
  prediction=TRUE
)
)
# Egger's test for assessing publication bias
# method.bias "rank", "linreg", "mm", or "score"
metabias(meta.prop.isc)
trimfill(meta.prop.isc)

#--------------------------------
# Subgroup analysis by age-group
#--------------------------------
update.meta(meta.prop.isc, 
            byvar=age_range, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by randomisation
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=randomisation_1, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by HIV status
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=HIV, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by time range
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=pub_year1, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by FDA drug class
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=worst_fda_class, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by miltefosine
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=miltefosine, 
            comb.random = TRUE, 
            comb.fixed = F
)
#-----------------------------------
# Subgroup analysis by pregnancy
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=pregnancy, 
            comb.random = TRUE, 
            comb.fixed = F
)
#-----------------------------------
# Subgroup analysis by pregnancy
#-----------------------------------
update.meta(meta.prop.isc, 
            byvar=exclusion_child_bearing_age_women, 
            comb.random = TRUE, 
            comb.fixed = F
)

#########################################################################
# Carry out meta-analysis of proportion for studies from Eastern Africa
#########################################################################
dat2_EA<- dat2[which(dat2$Study.Region=="Eastern Africa"),]
dat2_EA<- droplevels(dat2_EA)

# summarise 
dat2_EA %>% 
  #dplyr::group_by(age_range) %>%
  dplyr::summarise(
    n	= sum(n_total),
  )

# publication year
dat2_EA  %>% 
  dplyr::group_by(pub_year1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# summarise by randomisation status
dat2_EA %>% 
  dplyr::group_by(randomisation_1) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# Detailed summary by FDA drug class used or not
dat2_EA  %>% 
  dplyr::group_by(worst_fda_class) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_patients	= sum(n_total)
  )

# summarise by pregnancy status
dat2_EA %>% 
  #dplyr::group_by(pregnancy) %>%
  dplyr::group_by(pregnancy) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# detailed summary by contraception use required for child bearing age
dat2_EA %>% 
  dplyr::group_by(exclusion_child_bearing_age_women) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

# HIV
dat2_EA %>% 
  dplyr::group_by(HIV) %>%
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_prop	= length(unique(Tag))/nrow(dat2)*100,
    n_patients	= sum(n_total)
  )

(meta.prop.ea <- metaprop(
  data = dat2_EA,
  n_males, 
  n_total, 
  prediction=TRUE
)
)

# Egger's test for assessing publication bias
metabias(meta.prop.ea)
trimfill(meta.prop.ea)

#--------------------------------
# Subgroup analysis by age-group
#--------------------------------
update.meta(meta.prop.ea, 
            byvar=age_range, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by randomisation
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=randomisation_1, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by HIV status
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=HIV, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by time range
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=pub_year1, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by drug class
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=drug_class, 
            comb.random = TRUE, 
            comb.fixed = TRUE
)

#-----------------------------------
# Subgroup analysis by FDA drug class
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=worst_fda_class, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by miltefosine
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=miltefosine, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by pregnancy
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=pregnancy, 
            comb.random = TRUE, 
            comb.fixed = F
)
#-----------------------------------
# Subgroup analysis by exclusion_child_bearing_age_women
#-----------------------------------
update.meta(meta.prop.ea, 
            byvar=exclusion_child_bearing_age_women, 
            comb.random = TRUE, 
            comb.fixed = F
)

#==================================================
# Overall for studies which excluded pregnancy
#==================================================
dat2_preg<- dat2[which(dat2$pregnancy=="Excluded"),]
dat2_preg<- droplevels(dat2_preg)

(meta.prop.preg.ex <- metaprop(
  data = dat2_preg,
  n_males, 
  n_total, 
  prediction=TRUE
)
)

update.meta(meta.prop.preg.ex, 
            byvar=worst_fda_class, 
            comb.random = TRUE, 
            comb.fixed = F
)

#########################################################################
# Carry out meta-analysis of proportion for studies on adults 
#########################################################################
dat2_adults<- dat2[which(dat2$age_range=="Adults"),]

(meta.prop.adults <- metaprop(
  data = dat2_adults,
  n_males, 
  n_total, 
  prediction=TRUE
)
)

# Egger's test for assessing publication bias
metabias(meta.prop.adults )
trimfill(meta.prop.adults )

#-----------------------------------
# Subgroup analysis by HIV status
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=HIV, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by time range
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=pub_year1, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by FDA drug class
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=worst_fda_class, 
            comb.random = TRUE, 
            comb.fixed = TRUE
)

#-----------------------------------
# Subgroup analysis by miltefosine
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=miltefosine, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by pregnancy
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=pregnancy, 
            comb.random = TRUE, 
            comb.fixed = F
)
#-----------------------------------
# Subgroup analysis by exclusion_child_bearing_age_women
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=exclusion_child_bearing_age_women, 
            comb.random = TRUE, 
            comb.fixed = F
)

#-----------------------------------
# Subgroup analysis by preg_comp
#-----------------------------------
update.meta(meta.prop.adults , 
            byvar=preg_comp, 
            comb.random = TRUE, 
            comb.fixed = F
)


#==================================================================
# Distribution of mean duration of illness at presentation over time
#==================================================================
fever_duration <- read_excel("Supplemental file 3_data.xlsx", 
                             sheet = "duration_of_illness"
)

# correlatoon between mean duration of illness at presentation and publication year
# NB:Data were available from 132 arms of which for 5 study arm, data was reported as median rather than mean

cor.test(fever_duration$mean_duration_days, fever_duration$year)          

# Number of studies reporting fever duration at baseline
fever_duration %>% 
  dplyr::summarise(
    n_studies 	= length(unique(Tag)),
    n_patients	= sum(n_treated)
  )

ggplot(fever_duration, aes(x=year, y=mean_duration_days,size = n_treated)) + 
  geom_point(pch=1, col="#56B4E9")+
  scale_size_area(max_size = 12) +
  geom_smooth(show_guide = FALSE)+
  ggtitle("")+
  xlab("Publication year") + 
  ylab("Mean duration of illness at baseline (days)")+
  xlim(1980,2020)


# End Script(Not Run)