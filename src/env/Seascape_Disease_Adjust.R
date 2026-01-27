##############################
## Revised seascape disease ##
##############################

## setup
########
# setwd
setwd("Documents/Github/MVP_oyster_GO")

# libraries
library(tidyverse)
library(dplyr)

# read in data
seascapeEnv <- read.csv("data/EnvDat/seascape/seascape_abiotic_biotic_envr.csv")
newDis <- read.csv("data/EnvDat/seascape/population_statistics_Cq30cutoff.csv")
########

## data prep
############
# check data
head(seascapeEnv)
head(newDis)

# disease goes to wide format
dis_reshape <- reshape(newDis, idvar = "Population", timevar = "Pathogen", direction = "wide")
dis27 <- dis_reshape[,c("Population","Prevalence.Dermo","Prevalence.MSX")]
colnames(dis27) <- c("ID_SiteDate", "Dermo_Prevalence", "MSX_Prevalence")

# trim old seascape disease data
seascape_prep <- seascapeEnv %>% select(!c(Dermo_Prevalence, MSX_Prevalence, 
                          dermo_prev_percent, msx_prev_percent, 
                          intensity_msx, Cq_Mean_MSX, SQ_Mean_MSX,
                          intensity_dermo, Cq_Mean_Dermo, SQ_Mean_Dermo))

# combine with new disease
seascape_dis <- merge(seascape_prep, dis27, by = c("ID_SiteDate"))
############

## save new df
##############
write.csv(seascape_dis, "data/EnvDat/seascape/seascapeEnv_biotic_abiotic_30cq.csv", row.names = F)
##############
