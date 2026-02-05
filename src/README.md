## Read Me: MVP_oyster_GO Scripts

This folder contains all scripts used for analysis in "Genomic Offsets Predict Survival with Low Accuracy in a Marine Common Garden"

Scripts in the "env" folder should be run first to process all environmental data used for downstream analysis. This folder contains scripts to process abiotic environmental data at the two common garden sites (1A), determine disease prevalence at these sites (1B), adjust seascape disease data for different detection thresholds (1C), and combine and process all these data and the other seascape/experimental-site-of-origin data into formats ready for modeling.

Scripts in the "exp_fit" folder should be run second to process the fitness-proxy data. This folder contains scripts to summarize length trends in the experimental common gardens (2A), summarize survival trends in the experimental common gardens (2B), and conduct statistical analyses to examine differences between experimental groups in the common gardens (2C).

Scripts in the "offset" folder should be run last to process genetic data and conduct offset modeling and comparisons with fitness-proxies. This folder contains scripts to process genetic data into formats required for different offset methods (3A), conduct offset modeling (3B-D), and compare offset predictions to ground truth fitness-proxies (3E). Note that the "trainGF" scripts must be run prior to "3D_offsetGF."