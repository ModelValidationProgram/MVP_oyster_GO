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
Dis27 <- read.csv("data/EnvDat/seascape/population_statistics_Cq27cutoff.csv")
########

## data prep
############
# check data
head(seascapeEnv)
head(Dis27)

# disease goes to wide format
dis27_reshape <- reshape(Dis27, idvar = "Population", timevar = "Pathogen", direction = "wide")
dis27 <- dis27_reshape[,c("Population","Prevalence.MSX")]
colnames(dis27) <- c("ID_SiteDate", "MSX_Prevalence")

# trim old seascape disease data
seascape_prep_msx <- seascapeEnv %>% select(!c(MSX_Prevalence, msx_prev_percent, 
                                               intensity_msx, Cq_Mean_MSX, SQ_Mean_MSX))

# combine with new disease
seascape_dis_msx <- merge(seascape_prep_msx, dis27, by = c("ID_SiteDate"))
############

## save new df
##############
write.csv(seascape_dis_msx, "data/EnvDat/seascape/seascapeEnv_biotic_abiotic_adjust.csv", row.names = F)
##############
