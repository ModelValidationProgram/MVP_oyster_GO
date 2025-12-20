###################################################
## Disease Processing for Lewisetta & York River ##
###################################################

## setup
########
# setwd
setwd("~/Documents/Github/MVP_oyster_GO")

# libraries
library(dplyr) # for data processing
library(stringr) # for data processing
library(lubridate) # for dates
library(plotrix) # for std error

# data
expdis <- read.csv("data/EnvDat/exp/sentinel_RFTM_organized_dermo.csv")[,-(6:10)]
expdis_lew <- read.csv("data/EnvDat/exp/LewisettaSentinels.csv")
expdis_yrk <- read.csv("data/EnvDat/exp/YorkRiverShorelineDisease-24March2025.csv")
peacrab <- read.csv("data/EnvDat/exp/MVP23-FieldBags - finalTissue.csv")
sitedate <- read.csv("data/EnvDat/seascape/SeascapeSamples - date.csv")
########

## disease summary stats
########################
# start with the disease data, need to organize averages across experimental period
# this is coming from sentinel population
lewyrk_dis_avgs <- expdis %>% 
  group_by(SITE) %>% 
  dplyr::summarize(Dermo_Prevalence = mean(P_dermo), 
                   Dermo_Weighted_Prevalence = mean(WP_dermo, na.rm = T),
                   Dermo_Prevalence_se = std.error(P_dermo))
lewyrk_dis_avgs <- as.data.frame(lewyrk_dis_avgs)

# get the peacrab data from finalTissue
peacrab_trim <- peacrab[,c("bagID","peaCrab")]

# figure out which sites each bag comes from
peacrab_trim$bagnum <- as.numeric(str_split_fixed(peacrab_trim$bagID, "-", 3)[,3])
peacrab_trim$site_name <- (str_split_fixed(peacrab_trim$bagID, "-", 3)[,2])
peacrab_trim$site <- ifelse(peacrab_trim$bagnum < 4, "Lewisetta", "YorkRiver")

# now calculate numbers of pea crabs
lewyrk_pea_avgs <- peacrab_trim %>% 
  group_by(site) %>% 
  dplyr::summarize(Pea_crab = sum(peaCrab == T)/(sum(peaCrab == T) + sum(peaCrab == F))) # this is the percent of oysters per pop that had pea crabs
lewyrk_pea_avgs <- as.data.frame(lewyrk_pea_avgs)
lewyrk_pea_avgs$Pea_crab_se <- c(0,0)

# now msx; these data are formatted very differently
summary(expdis_lew)
summary(expdis_yrk)

# expdis_yrk needs serious re-formatting
# msx data only
msx_yrk <- expdis_yrk[,-(4:18)]
colnames(msx_yrk) <- msx_yrk[1,]
msx_yrk_rename <- msx_yrk[-1,]

# spring imports is relevant group
msx_yrk_si <- msx_yrk[msx_yrk$Site == "Spring Imports",]
msx_yrk_si$msx_prev <- as.numeric(msx_yrk_si$`Inf`) / as.numeric(msx_yrk_si$n)

# get experimental period
msx_yrk_si$Date <- mdy(msx_yrk_si$Date)
msx_yrk_exp <- msx_yrk_si[msx_yrk_si$Date > as.Date("2023-01-01"),]

# use full dataset for deby disease, 2023-2024 for york river site
yrk_prev_msx <- sum(as.numeric(msx_yrk_exp$`Inf`)) / sum(as.numeric(msx_yrk_exp$n))
yrk_prev_msx_se <- std.error(as.numeric(msx_yrk_exp$`Inf`) / as.numeric(msx_yrk_exp$n))

# now lewisetta msx - we only have data spanning back to 2023, so lola and lew prev are the same
expdis_lew_nona <- expdis_lew[!is.na(expdis_lew$msx_intensity),]
lew_prev_msx <- 1 - (nrow(expdis_lew_nona[which(expdis_lew$msx_intensity == "N"),]) / nrow(expdis_lew_nona))
lew_prev_msx_se <- 0 # prevelance was zero, so sd also zero

# put all msx data into single df
msx_avgs <- data.frame(site_name = c("Lewisetta","YorkRiver"),
                       MSX_Prevalence = c(lew_prev_msx, yrk_prev_msx),
                       MSX_Prevalence_se = c(lew_prev_msx_se, yrk_prev_msx_se))

# put together all disease data
lewyrk_dis_avgs
msx_avgs
lewyrk_pea_avgs
dis_avgs <- cbind(msx_avgs, 
                  lewyrk_dis_avgs[,c("Dermo_Prevalence","Dermo_Prevalence_se")], 
                  lewyrk_pea_avgs[,c("Pea_crab","Pea_crab_se")])
########################

## disease plotting - msx
#########################
ggplot() + 
  geom_point(data = msx_yrk_exp, aes(x = Date, y = msx_prev)) + 
  geom_line(data = msx_yrk_exp, aes(x = Date, y = msx_prev)) + 
  theme_classic() + 
  labs(x = "Date", y = "MSX Prevalence")

ggplot() +
  geom_point(data = msx_yrk_si, aes(x = Date, y = msx_prev)) + 
  geom_line(data = msx_yrk_si, aes(x = Date, y = msx_prev)) + 
  annotate("rect", 
           xmin = as.Date("2023-05-05"), xmax = as.Date("2024-11-08"), 
           ymin = 0, ymax = 0.55,
           alpha = 0.2) +
  ylim(0, 0.55) +
  theme_classic() + 
  labs(title = "MSX Prevalence at York River, 2021 - 2024", x = "Date", y = "MSX Prevalence")

expdis_lew_nona$msx_prev <- 0
expdis_lew_nona$Date <- as_date(parse_date_time(expdis_lew_nona$date, c("mdy")))

ggplot() +
  geom_point(data = expdis_lew_nona, aes(x = Date, y = msx_prev)) + 
  geom_line(data = expdis_lew_nona, aes(x = Date, y = msx_prev)) + 
  # annotate("rect", 
  #          xmin = as.Date("2023-05-05"), xmax = as.Date("2024-11-08"), 
  #          ymin = 0, ymax = 0.55,
  #          alpha = 0.2) +
  ylim(0, 0.55) +
  theme_classic() + 
  labs(title = "MSX Prevalence at Lewisetta, 2023 - 2024", x = "Date", y = "MSX Prevalence")
#########################

## plotting - dermo
###################
months_map <- list("June" = "06", "July" = "07", "August" = "08", "Sept" = "09", "Oct" = "10", "Nov" = "11")
expdis_mut <- expdis %>% mutate(MONTH = months_map[MONTH], MONTH) %>% as.data.frame()
expdis_mut$date <- as.Date(paste0(expdis_mut$YEAR, "-", expdis_mut$MONTH, "-01")) # 01 is just the first day of the month so we can turn this into a date

ggplot() +
  geom_point(data = expdis_mut[expdis_mut$SITE == "Lewisetta",], aes(x = date, y = P_dermo)) + 
  geom_line(data = expdis_mut[expdis_mut$SITE == "Lewisetta",], aes(x = date, y = P_dermo)) + 
#  annotate("rect", 
#           xmin = as.Date("2023-05-05"), xmax = as.Date("2024-11-01"), 
#           ymin = 0, ymax = 0.55,
#           alpha = 0.2) +
  ylim(0, 0.55) +
  theme_classic() + 
  labs(title = "Dermo Prevalence at Lewisetta, 2023 - 2024", x = "Date", y = "Dermo Prevalence")

ggplot() +
  geom_point(data = expdis_mut[expdis_mut$SITE == "YorkRiver",], aes(x = date, y = P_dermo)) + 
  geom_line(data = expdis_mut[expdis_mut$SITE == "YorkRiver",], aes(x = date, y = P_dermo)) + 
  #  annotate("rect", 
  #           xmin = as.Date("2023-05-05"), xmax = as.Date("2024-11-01"), 
  #           ymin = 0, ymax = 0.55,
  #           alpha = 0.2) +
  ylim(0, 0.55) +
  theme_classic() + 
  labs(title = "Dermo Prevalence at York River, 2023 - 2024", x = "Date", y = "Dermo Prevalence")
###################


## plotting - all
#################
dis_long <- dis_avgs[,c("site_name","MSX_Prevalence","Dermo_Prevalence","Pea_crab")] %>% 
  tidyr::pivot_longer(cols = !site_name, names_to = "Disease", values_to = "Prevalence")
dis_long_se <- dis_avgs[,c("site_name","MSX_Prevalence_se","Dermo_Prevalence_se","Pea_crab_se")] %>% 
  tidyr::pivot_longer(cols = !site_name, names_to = "Disease", values_to = "std_err")
dis_long$std_err <- dis_long_se$std_err

ggplot(data = dis_long, aes(x = site_name, y = Prevalence, fill = Disease)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  geom_errorbar(data = dis_long, aes(x = site_name, 
                                     ymin = Prevalence - std_err, ymax = Prevalence + std_err),
                position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#fe4a49", "#fed766", "#009fb7"), 
                    labels = c("Dermo", "MSX", "Pea Crab")) + 
  labs(title = "Disease Prevalence at Experimental Common Gardens",
       x = "Common Garden Site", y = "Disease Prevalence") + 
  ylim(0,1) +
  theme_classic()

ggplot(data = dis_long[!dis_long$Disease == "Pea_crab",], 
       aes(x = site_name, y = Prevalence, fill = Disease)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  scale_fill_manual(values = c("#fe4a49", "#fed766"), 
                    labels = c("Dermo", "MSX")) + 
  labs(title = "Disease Prevalence at Experimental Common Gardens",
       x = "Common Garden Site", y = "Disease Prevalence") + 
  ylim(0,1) +
  theme_classic()
#################
