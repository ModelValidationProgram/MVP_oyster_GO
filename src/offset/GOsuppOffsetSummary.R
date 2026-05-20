lea_go <- read.csv("results/GO_results/offsets_LEA_expsites_2026-04-06.csv")[,-1]
rda_go <- read.csv("results/GO_results/offsets_RDA_expSites_2026-04-07.csv")[,-1]
gf_go <- read.csv("results/GO_results/offsets_GF_expSites_2026-04-23.csv")[,-1]

# seascape
lea_sea <- read.csv("results/GO_results/offsets_LEA_fullseascape_2026-04-06.csv")[,-1]
rda_sea <- read.csv("results/GO_results/offsets_RDA_fullseascape_2026-04-07.csv")[,-1]
gf_sea <- read.csv("results/GO_results/offsets_GF_fullseascape_2026-04-23.csv")[,-1]
envdist <- read.csv("data/EnvDat/exp/distance_matrix_2026-01-30.csv")[,c("X","Lewisetta","YorkRiver")] # this has both the seascape sites and the experimental sites
envdist_abiotic <- read.csv("data/EnvDat/exp/distance_matrix_abiotic_2026-01-30.csv")[,c("X","Lewisetta","YorkRiver")] # this has both the seascape sites and the experimental sites

# experimental go have the wrong latlon for deby
lea_go[lea_go$site_name == "S2_DEBY",]$Latitude_manual <- 37.24728
lea_go[lea_go$site_name == "S2_DEBY",]$Longitude_manual <- -76.49937
rda_go[rda_go$site_name == "S2_DEBY",]$lat <- 37.24728
rda_go[rda_go$site_name == "S2_DEBY",]$lon <- -76.49937
gf_go[gf_go$site_name == "S2_DEBY",]$lat <- 37.24728
gf_go[gf_go$site_name == "S2_DEBY",]$lon <- -76.49937

# latlons
latlons$site_name <- latlons$ID_SiteDate

# rename envdist columns
colnames(envdist) <- colnames(envdist_abiotic) <- c("site_name", "dist_lew", "dist_yrk")

# split into seascape vs exp pops for future plotting
envdist_sea <- envdist[1:27,]
envdist_exp <- envdist[28:35,]

envdist_sea_nb <- envdist_abiotic[1:27,]
envdist_exp_nb <- envdist_abiotic[28:35,]

# add latlons
envdist_sea2 <- merge(latlons[,c("site_name","Latitude_manual","Longitude_manual")], envdist_sea, by = c("site_name"))
envdist_exp2 <- merge(popsdf, envdist_exp, by = c("site_name"))

envdist_sea2_nb <- merge(latlons[,c("site_name","Latitude_manual","Longitude_manual")], envdist_sea_nb, by = c("site_name"))
envdist_exp2_nb <- merge(popsdf, envdist_exp_nb, by = c("site_name"))

# rda also needs latlons
rda_sea2 <- merge(latlons[,c("site_name","Latitude_manual","Longitude_manual")], rda_sea, by = c("site_name"))

# experimental go
colnames(lea_go) <- c("site_name","lat","long","lea_lew","lea_yrk",
                      "lea_lew_nobio","lea_yrk_nobio")
colnames(rda_go) <- c("site_name","lat","long","rda_lew","rda_yrk",
                      "rda_lew_nobio","rda_yrk_nobio")
colnames(gf_go) <- c("site_name","lat","long","gf_lew","gf_yrk",
                      "gf_lew_nobio","gf_yrk_nobio")
envdist_exp_nb_trim <- envdist_exp2_nb %>% select(site_name,dist_lew,dist_yrk) %>% 
  rename(dist_lew_nobio = dist_lew, dist_yrk_nobio = dist_yrk)
go1 <- merge(lea_go, rda_go, by = c("site_name", "lat", "long"))
go2 <- merge(go1, gf_go, by = c("site_name", "lat", "long"))
go3 <- merge(go2, envdist_exp, by = "site_name")
exp_go <- merge(go3, envdist_exp_nb_trim, by = "site_name")
write.csv(exp_go, "Offsets_expSites_2026-05-15.csv", row.names = F)

# seascape go
colnames(lea_sea) <- c("site_name","lat","long","lea_lew","lea_yrk")
colnames(rda_sea) <- c("site_name","rda_lew","rda_yrk",
                       "rda_lew_nobio","rda_yrk_nobio")
rda_sea <- rda_sea %>% select(!contains("nobio"))
colnames(gf_sea) <- c("site_name","lat","long","gf_lew","gf_yrk")
envdist_sea_nb_trim <- envdist_sea2_nb %>% select(site_name,dist_lew,dist_yrk) %>% 
  rename(dist_lew_nobio = dist_lew, dist_yrk_nobio = dist_yrk)
gosea1 <- merge(lea_sea, rda_sea, by = c("site_name"))
gosea2 <- merge(gosea1, gf_sea, by = c("site_name", "lat", "long"))
sea_go <- merge(gosea2, envdist_sea2, by = "site_name")
sea_go <- sea_go %>% select(!contains("manual"))
write.csv(sea_go, "Offsets_fullSeascape_2026-05-15.csv", row.names = F)

