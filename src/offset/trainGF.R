# script to train models on cluster

# packages
library(dplyr) # for data cleaning
library(plyr) # for data cleaning
library(parallel) # for parallelizing processes
library(doParallel) # for parallelizing processes 
library(gradientForest) # for running gradient forest

# data
# env
env_all <- as.data.frame(read.csv("data/EnvDat/env_scaled_2025-08-13.csv")[,-1])
env_sea <- as.data.frame(read.csv("data/EnvDat/seascape/sea_scaled_expanded_reord_2025-08-14.csv")[,-1])
env_exp <- as.data.frame(read.csv("data/EnvDat/exp/exp_scaled_expanded_2025-08-14.csv")[,-1])

# sites
env_sea_site <- read.csv("data/EnvDat/seascape/SeascapeSamples_site.csv")[,-1]
env_sea_date <- read.csv("data/EnvDat/seascape/SeascapeSamples - date.csv")[,-1]
env_exp_site <- read.csv("data/EnvDat/exp_sitenames.csv")[,-1]
exp_site_latlon <- read.csv("data/EnvDat/exp/exp_site_info.csv")[,-1]

# gen
genoMat <- readRDS("data/GenDat/genoMatFull.RDS")
genoThinMat <- readRDS("data/GenDat/genoMatThin.RDS")

# individuals
expInds <- readRDS("data/IndDat/20240922_experimental_indsmatrix.rds")
seascapeInds <- readRDS("data/IndDat/20250604_seascape_indsmatrix.rds")
inds_genomat <- read.csv("data/GenDat/inds_genomat.csv")[,-1]

# order this data by genotype info, first by trimming inds to only those in df
seascapeInds_reord <- seascapeInds[seascapeInds$clean_ID %in% inds_genomat,]
seascapeInds_reord$clean_ID <- factor(seascapeInds_reord$clean_ID, levels = inds_genomat)
seascapeInds_reord2 <- seascapeInds_reord[order(seascapeInds_reord$clean_ID),]

# double check
(seascapeInds_reord2$clean_ID == inds_genomat) # looks good!

# get a full list of pops
pops_list_full <- seascapeInds_reord2$ID_SiteDate

# check geno data
dim(genoMat)
dim(genoThinMat)

# rotate df so rows are now columns (we want individuals as columns, alleles as rows)
genoMatT <- as.matrix(t(genoMat))
genoThinMatT <- as.matrix(t(genoThinMat))

# any missing data?
sum((genoMatT == 9)) # 0 - good 
sum((genoThinMatT == 9)) # 0 - good

# add pop data to genomic data
rownames(genoMat) <- rownames(genoThinMat) <- pops_list_full
colnames(genoMatT) <- colnames(genoThinMatT) <- pops_list_full

# function to calculate allele frequency @ single locus
calc_freq <- function(x){
  a <- sum(x, na.rm=TRUE)
  b <- (2*length(na.omit(x)))
  a/b
}

# function extending this to calculate allele frequencies across whole pop
calcfreq <- function(a, pop){
  tapply(a, pop, calc_freq)
}

# allele freqs
freqs <- apply((genoMat), 2, calcfreq, pops_list_full)
str(freqs)

freqs_thin <- apply((genoThinMat), 2, calcfreq, pops_list_full)
str(freqs_thin)

# update colnames for gf
colnames(freqs) <- make.names(as.character(1:ncol(genoMat)))
colnames(freqs_thin) <- make.names(as.character(1:ncol(genoThinMat)))

# do the same for the full geno matrix, just in case
colnames(genoMat) <- make.names(as.character(1:ncol(genoMat)))
colnames(genoThinMat) <- make.names(as.character(1:ncol(genoThinMat)))

# we also need one just per site
env_sea_pop <- env_sea[!duplicated(env_sea$ID_SiteDate),]

# now experimental data
# no need to order this by any geno data
# should get pop level info 
env_exp_pop <- env_exp[!duplicated(env_exp$site_name),]

# reduced variable set
env_sea_red <- env_sea[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled", "Dermo_Prevalence_scaled", "Pea_crab_scaled")]
env_sea_pop_red <- env_sea_pop[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled", "Dermo_Prevalence_scaled", "Pea_crab_scaled")]
env_exp_pop_red <- env_exp_pop[,c("site_name","salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled", "Dermo_Prevalence_scaled", "Pea_crab_scaled")]
env_exp_lew_red <- env_exp_pop[,c("site_name", "lew_salinity_quantile_10_scaled", "lew_salinity_quantile_90_scaled", "lew_temp_quantile_10_scaled", "lew_temp_quantile_90_scaled", "lew_Dermo_Prevalence_scaled", "lew_Pea_crab_scaled")]
env_exp_yrk_red <- env_exp_pop[,c("site_name", "yrk_salinity_quantile_10_scaled", "yrk_salinity_quantile_90_scaled", "yrk_temp_quantile_10_scaled", "yrk_temp_quantile_90_scaled", "yrk_Dermo_Prevalence_scaled", "yrk_Pea_crab_scaled")]

# change exp colnames to match up
colnames(env_exp_lew_red) <- colnames(env_exp_yrk_red) <- colnames(env_exp_pop_red)

# start by defining a maximum number of splits
maxLevel <- log2(0.368*nrow(env_sea_pop_red)/2)

# run gradient forest
# allele freq models
start_time <- Sys.time() # time start
gf_af <- gradientForest(cbind(env_sea_pop_red, freqs_thin), 
                        predictor.vars = colnames(env_sea_pop_red), 
                        response.vars = (colnames(freqs)), 
                        ntree = 500, 
                        maxLevel = maxLevel, 
                        trace = T, 
                        corr.threshold=0.50) # tons of warnings here, "response has five or fewer unique values. Are you sure you want to do regression?" - warning doesn't appear on the cluster
end_time <- Sys.time() # time end
(end_time - start_time) # about 1.5hrs

## COME BACK TO THIS ON CLUSTER, DOESN'T RUN ON PERSONAL COMPUTER
# geno models
# crashes on personal laptop, run on cluster
start_time <- Sys.time() # time start
gf_geno <- gradientForest(cbind(env_sea_red, genoThinMat), 
                          predictor.vars = colnames(env_sea_red), 
                          response.vars=colnames(genoThinMat), 
                          ntree=500, 
                          maxLevel=maxLevel, 
                          trace=T, 
                          corr.threshold=0.50)
end_time <- Sys.time() # time end
(end_time - start_time) # over 24 on cluster

# save trained models
saveRDS(gf_af, paste0("results/lg_results/af_geno_",Sys.Date(),".RDS"))
saveRDS(gf_geno, paste0("results/lg_results/gf_geno_",Sys.Date(),".RDS"))
