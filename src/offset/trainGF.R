# script to train models on cluster

# setup
setwd("/projects/lotterhos/MVP_oyster_GO")

# packages
#install.packages("gradientForest")
library(gradientForest) # for running gradient forest

# data
# env
env_all <- as.data.frame(read.csv("data/EnvDat/env_scaled_cq27_2026-01-27.csv")[,-1])
env_sea <- as.data.frame(read.csv("data/EnvDat/seascape/sea_scaled_expanded_reord_2026-01-27.csv")[,-1])

# sites
env_sea_site <- read.csv("data/EnvDat/seascape/SeascapeSamples_site.csv")[,-1]
env_sea_date <- read.csv("data/EnvDat/seascape/SeascapeSamples - date.csv")[,-1]
exp_site_latlon <- read.csv("data/EnvDat/exp/exp_site_info.csv")[,-1]

# gen
#genoMat <- readRDS("data/GenDat/genoMatFull.RDS")
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
dim(genoThinMat)

# rotate df so rows are now columns (we want individuals as columns, alleles as rows)
genoThinMatT <- as.matrix(t(genoThinMat))

# any missing data?
sum((genoThinMatT == 9)) # 0 - good

# add pop data to genomic data
colnames(genoThinMatT) <- pops_list_full

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
freqs_thin <- apply((genoThinMat), 2, calcfreq, pops_list_full)
str(freqs_thin)

# update colnames for gf
colnames(freqs_thin) <- make.names(as.character(1:ncol(genoThinMat)))

# do the same for the full geno matrix, just in case
colnames(genoThinMat) <- make.names(as.character(1:ncol(genoThinMat)))

# reduced variable set
env_sea_pop <- env_sea[!duplicated(env_sea$ID_SiteDate),]
env_sea_red <- env_sea[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled", "Dermo_Prevalence_scaled", "Pea_crab_scaled", "MSX_Prevalence_scaled")]
env_sea_red_nb <- env_sea[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled")]
env_sea_pop_red <- env_sea_pop[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled", "Dermo_Prevalence_scaled", "Pea_crab_scaled", "MSX_Prevalence_scaled")]
env_sea_pop_red_nb <- env_sea_pop[,c("salinity_quantile_10_scaled", "salinity_quantile_90_scaled", "temp_quantile_10_scaled", "temp_quantile_90_scaled")]

# start by defining a maximum number of splits
maxLevel <- log2(0.368*nrow(env_sea_pop_red)/2)

# run gradient forest
# allele freq models
start_time <- Sys.time() # time start
gf_af <- gradientForest(cbind(env_sea_pop_red, freqs_thin), 
                        predictor.vars = colnames(env_sea_pop_red), 
                        response.vars = (colnames(freqs_thin)), 
                        ntree = 500, 
                        maxLevel = maxLevel, 
                        trace = T, 
                        corr.threshold=0.50) # tons of warnings here, "response has five or fewer unique values. Are you sure you want to do regression?" - warning doesn't appear on the cluster
end_time <- Sys.time() # time end
(end_time - start_time) # about 1.5hrs

# save trained af model
saveRDS(gf_af, paste0("results/lg_results/gf_af_cq27_",Sys.Date(),".RDS"))

# geno models
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

# save trained geno model
saveRDS(gf_geno, paste0("results/lg_results/gf_geno_cq27_",Sys.Date(),".RDS"))

# without biotic variables (nb)
# allele freq models
start_time <- Sys.time() # time start
gf_af_nb <- gradientForest(cbind(env_sea_pop_red_nb, freqs_thin),
                        predictor.vars = colnames(env_sea_pop_red_nb),
                        response.vars = (colnames(freqs_thin)),
                        ntree = 500,
                        maxLevel = maxLevel,
                        trace = T,
                        corr.threshold=0.50) # tons of warnings here, "response has five or fewer unique values. Are you sure you want to do regression?" - warning doesn't appear on the cluster
end_time <- Sys.time() # time end
(end_time - start_time) # about 1.5hrs

# save trained af model
saveRDS(gf_af_nb, paste0("results/lg_results/gf_af_abiotic_",Sys.Date(),".RDS"))

# geno models
start_time <- Sys.time() # time start
gf_geno_nb <- gradientForest(cbind(env_sea_red_nb, genoThinMat),
                          predictor.vars = colnames(env_sea_red_nb),
                          response.vars=colnames(genoThinMat),
                          ntree=500,
                          maxLevel=maxLevel,
                          trace=T,
                          corr.threshold=0.50)
end_time <- Sys.time() # time end
(end_time - start_time) # over 24 on cluster

# save trained geno model
saveRDS(gf_geno_nb, paste0("results/lg_results/gf_geno_abiotic_",Sys.Date(),".RDS"))
