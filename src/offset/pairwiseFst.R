# script for running pairwise fst

# setwd
setwd("/projects/lotterhos/MVP_oyster_GO")

# libraries
library(adegenet)
library(hierfstat)

# read data
geno_mat_hier <- readRDS("data/GenDat/geno_mat_hierfstat.RDS")
geno_mat_hier_exp <- readRDS("data/GenDat/geno_mat_exp_hierfstat.RDS")

# calc basic stats
sea_stats <- hierfstat::basic.stats(geno_mat_hier)
exp_stats <- hierfstat::basic.stats(geno_mat_hier_exp)

# calc pairwise fst
sea_fst <- hierfstat::pairwise.WCfst(geno_mat_hier)
exp_fst <- hierfstat::pairwise.WCfst(geno_mat_hier_exp)

# save
saveRDS(sea_stats, paste0("results/gen/seascape_statistics_", Sys.Date(), ".RDS"))
saveRDS(exp_stats, paste0("results/gen/exp_pops_statistics_", Sys.Date(), ".RDS"))
saveRDS(sea_fst, paste0("results/gen/seascape_pairwise_", Sys.Date(), ".RDS"))
saveRDS(exp_fst, paste0("results/gen/exp_pops_pairwise_", Sys.Date(), ".RDS"))

