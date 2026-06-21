# script for running pairwise fst

# setwd
setwd("/projects/lotterhos/MVP_oyster_GO")

# libraries
library(adegenet)
library(dartR)

# read data
geno_mat_remap <- readRDS("data/GenDat/geno_mat_remapped.RDS")

# extract experimental pops
geno_mat_exp <- geno_mat_remap[geno_mat_remap$pop %in% c("CB_TX_2022-6-9", "Sister_Lk_LA_2022-8-9", "KP_FGR_FL_2022-8-23", "DWS_CB_VA_2022-10-7", "SR_GB_NH_2022-8-12", "DC_DR_ME_2022-10-25"),]
#popmaps <- c("CB_TX_2022-6-9" = 1, "Sister_Lk_LA_2022-8-9" = 2, "KP_FGR_FL_2022-8-23" = 3, "DWS_CB_VA_2022-10-7" = 4, "SR_GB_NH_2022-8-12" = 5, "DC_DR_ME_2022-10-25" = 6)
#geno_mat_exp$pop <- unname(popmaps[trimws(geno_mat_exp$pop)])
#geno_mat_exp$pop <- as.factor(gen_recoded_exp$pop)

# calc basic stats
exp_stats <- hierfstat::basic.stats(gen_recoded_exp)

# calc pairwise fst
exp_fst <- hierfstat::pairwise.WCfst(gen_recoded_exp)

# save
saveRDS(exp_stats, paste0("results/gen/exp_pops_statistics_", Sys.Date(), ".RDS"))
saveRDS(exp_fst, paste0("results/gen/exp_pops_pairwise_", Sys.Date(), ".RDS"))

