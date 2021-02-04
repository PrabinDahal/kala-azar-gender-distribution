#======================================================================================================================
# Title		:	Literature review of gender distribution in VL clinical trials
# Data version	:	23-Dec-2019 (from Sauman Singh)
# Author	:	Prabin Dahal
# Script Date	: 	02-02-2021
#======================================================================================================================
#rm(list=ls())

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
library(RColorBrewer)
library(readxl)

setwd("C:/Users/pdahl")

#=====================================
# Risk of bias in randomised studies
#=====================================

dat <- read_excel("Supplemental file 3_data.xlsx", 
		sheet = "Final data for R analysis"
	)

rob <- read_excel("Supplemental file 3_data.xlsx", 
		sheet = "Risk of bias randomised studies"
	)
rob_data <- merge(dat , rob, by="Tag")

# Overall meta-analysis of proportion 
(meta.prop <- metaprop(
			data = rob_data,
			n_males, 
			n_total, 
			prediction=TRUE
			)
		)
#-----------------------------------------------------
# Subgroup analysis by random_sequence_generation
#-----------------------------------------------------
update.meta(meta.prop,comb.random = TRUE, comb.fixed = F,
			byvar=random_sequence_generation)

#-----------------------------------------------------
# Subgroup analysis by allocation_concealment
#-----------------------------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
		byvar=allocation_concealment) 

#------------------------------------------------------------
# Subgroup analysis by blinding_of_participants_and_personnel
#------------------------------------------------------------
update.meta(meta.prop, comb.random = TRUE,comb.fixed = F,
		byvar=blinding_of_participants_and_personnel)


#=========================================
# Risk of bias in non-randomised studies
#=========================================

rob_nr <- read_excel("Supplemental file 3_data.xlsx", 
		sheet = "Risk of bias non_randomised"
	)
rob_nr_data <- merge(dat , rob_nr, by="Tag")


# Overall meta-analysis of proportion 
(meta.prop.nr <- metaprop(
			data = rob_nr_data,
			n_males, 
			n_total, 
			prediction=TRUE
			)
		)
#-----------------------------------------------------
# Subgroup analysis by bias_due_to_confounding
#-----------------------------------------------------
update.meta(meta.prop.nr,comb.random = TRUE, comb.fixed = F,
			byvar=bias_due_to_confounding)

#-----------------------------------------------------
# Subgroup analysis by bias_in_participants_selection
#-----------------------------------------------------
update.meta(meta.prop.nr, comb.random = TRUE,comb.fixed = F,
		byvar=bias_in_participants_selection) 

#------------------------------------------------------------
# Subgroup analysis by blinding_of_participants_and_personnel
#------------------------------------------------------------
update.meta(meta.prop.nr, comb.random = TRUE,comb.fixed = F,
		byvar=blinding_of_participants_and_personnel)

# End (Not Run)
