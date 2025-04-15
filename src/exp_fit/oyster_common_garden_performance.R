##################################
## Oyster common garden fitness ##
##################################

## setup
########
# set wd
setwd("~/MVP_Oyster_Analysis_Camille/MVP_Oysters_Analysis_Camille")

# packages
library(tidyverse) # for data wrangling
library(lubridate) # for dates
library(ggplot2) # for plotting
library(plotrix) # for std error
########

## load data
############
bags <- read.csv("data/ooters_prelim/MVP23-FieldBags - bags.csv")
mort <- read.csv("data/ooters_prelim/MVP23-FieldBags - mortality.csv")
pheno_bags <- read.csv("data/ooters_prelim/MVP23-FieldBags - phenotyping_bag.csv")
pheno <- read.csv("data/ooters_prelim/MVP23-FieldBags - phenotyping.csv")
expsites <- read.csv("data/ooters_prelim/MVP23-FieldBags - sites.csv")
spawntrt <- read.csv("data/ooters_prelim/MVP23-FieldBags - spawn_trt2.csv")
tags <- read.csv("data/ooters_prelim/MVP23-FieldBags - tags.csv")
tagleng <- read.csv("data/ooters_prelim/MVP23-FieldBags - tagsLength.csv")
############

## merging data
###############
# split bags to have more detailed bag data
bags[,5:6] <- str_split_fixed(bags$bags_label, "-", 3)[,2:3]
colnames(bags) <- c("bags_key","bag_site","bags_label","SpawnTrt_Key","pop","bagnum")

# and remove practice bags
bags_rm <- bags[-(61:62),]

# merging bags and mortality
head(mort)
merge1 <- merge(bags_rm, mort, by = c("bags_key"))
head(merge1)

# add spawntrt data
head(merge1)
head(spawntrt)
mort1 <- merge(merge1, spawntrt, by = c("SpawnTrt_Key"), all = T)
head(mort1)

# also put together tagleng data
tagsbags <- merge(tags, bags_rm, by = c("bags_key"), all = T)
tagsbags1 <- merge(tagsbags, tagleng, by = c("tags_key"), all = T)
###############

# clean data
############
# for mort data...
# trim datasets
colnames(mort1)
mort_trim <- mort1[,c("bags_key","bag_site","bags_label","pop","bagnum","SpawnTrt_Key","SpawnTrt_Label",
                         "latitudeDecimal","longitudeDecimal","mortality_key","mortality_label",
                         "mortality_timestamp","alive_count","dead_count","alive_returned")]

# split timestamps to get dates - I'll use mortality_timestamp for this
# dates are 4/29/24 - 5/7/24 for most recent monitoring event
# quick checks - looks good!
head(mort_trim[,c("mortality_timestamp")])
length(mort_trim[,c("mortality_timestamp")])

# split timestamp column into two
mort_trim[,c("mortality_timestamp")]
datetime_split <- str_split_fixed(mort_trim[,c("mortality_timestamp")], " ", 2) # split timestamp
colnames(datetime_split) <- c("mortality_date", "mortality_time") # name columns
mort_trim_dt <- cbind(mort_trim, datetime_split) # add to full dataset

# use dates to select monitoring event before thinning
# dates are 4/29/24 - 5/7/24
mort_trim_dt$mortality_date <- mdy(mort_trim_dt$mortality_date)

# take out practice rows
mort_dt_rm <- mort_trim_dt[!mort_trim_dt$SpawnTrt_Label == "Practice",]

# also put a monitoring event column in here
mort_dt_rm$monitoring_event <- ifelse(as_date(mort_dt_rm$mortality_date) > as_date("2023-11-16"),
                                     ifelse(as_date(mort_dt_rm$mortality_date) > as_date("2024-05-09"), 3, 2),
                                     1)

# for tags data...
# trim dataset
tagsbags2 <- tagsbags1[,c("tags_label","tags_timestamp","bag_site","bags_label",
                          "pop","bagnum","length","width","tagsLength_timestamp")]

# split timestamps to get dates, dates 4/29-5/7 for most recent monitoring
# split timestamp column into two
tagsbags2[,c("tagsLength_timestamp")]
datetime_split_tag <- str_split_fixed(tagsbags2[,c("tagsLength_timestamp")], " ", 2) # split timestamp
colnames(datetime_split_tag) <- c("tagsLength_date", "tagsLength_time") # name columns
tagsbags3 <- cbind(tagsbags2, datetime_split_tag) # add to full dataset

# use dates to split by monitoring event
# dates are 11/7/23 - 11/15/23, 4/29/24 - 5/7/24
tagsbags3$tagsLength_date <- mdy(tagsbags3$tagsLength_date)

# remove practice
tagsbags4 <- tagsbags3[!(tagsbags3$tagsLength_date == "2024-03-13"),]
tagsbags5 <- tagsbags4[!(is.na(tagsbags4$tagsLength_date)),]

tagsbags5$monitoring_event <- ifelse(as_date(tagsbags5$tagsLength_date) > as_date("2023-11-16"),
                                     ifelse(as_date(tagsbags5$tagsLength_date) > as_date("2024-05-09"), 3, 2),
                                     1)

tagsbags6 <- tagsbags5[!(tagsbags5$pop == ""),]
tagsbags6 <- tagsbags6[!(tagsbags6$pop == "LEW"),]
tagsbags6 <- tagsbags6[!(tagsbags6$pop == "YORK"),]

# check both datasets
head(mort_dt_rm)
head(tagsbags6)
############

# Mortality
###########
# define number of bags and create matrix to store data
bag <- levels(as.factor(mort_dt_rm$bags_label))
mortality_bag <- matrix(data = NA, nrow = length(bag), ncol = 2)

# try calculating survival at each time point
event1 <- mort_dt_rm[mort_dt_rm$monitoring_event == 1,]
event2 <- mort_dt_rm[mort_dt_rm$monitoring_event == 2,]
event3 <- mort_dt_rm[mort_dt_rm$monitoring_event == 3,]

mortality_bag_overall <- matrix(data = NA, nrow = length(bag), ncol = 5)

for (i in 1:length(bag)) {
  mortality_bag_overall[i,1] = bag[i]
  alive_orig = event1[event1$bags_label == bag[i],]$alive_count + event1[event1$bags_label == bag[i],]$dead_count
  alive_one = event1[event1$bags_label == bag[i],]$alive_count
  alive_two = event2[event2$bags_label == bag[i],]$alive_count
  alive_three = event3[event3$bags_label == bag[i],]$alive_count
  mortality_bag_overall[i,2] = alive_one / alive_orig
  mortality_bag_overall[i,3] = alive_two / alive_orig
  mortality_bag_overall[i,4] = alive_three / event2$alive_returned[i]
  mortality_bag_overall[i,5] = alive_three / alive_orig
}

mortality_bag_overall_df <- as.data.frame(mortality_bag_overall)
colnames(mortality_bag_overall_df) <- c("bags_label","survival_1","survival_2","survival_3","final_surv")

# also make a longer version of this
mortality_long1 <- data.frame(bags_label = mortality_bag_overall_df$bags_label,
                              survival = mortality_bag_overall_df$survival_1,
                              monitoring_event = 1, calc = "total")
mortality_long2 <- data.frame(bags_label = mortality_bag_overall_df$bags_label,
                              survival = mortality_bag_overall_df$survival_2,
                              monitoring_event = 2, calc = "total")
mortality_long3 <- data.frame(bags_label = mortality_bag_overall_df$bags_label,
                              survival = mortality_bag_overall_df$survival_3,
                              monitoring_event = 3, calc = "since_2")
mortality_long_all <- data.frame(bags_label = mortality_bag_overall_df$bags_label,
                                 survival = mortality_bag_overall_df$final_surv,
                                 monitoring_event = 3, calc = "total")
mortality_long <- rbind(mortality_long1, mortality_long2, mortality_long3, mortality_long_all)

# add bag data back
mortality_df <- merge(mortality_long, bags, by = c("bags_label"))
head(mortality_df)

# calculate larger trends
# by population
mortality_pop_surv <- mortality_df[mortality_df$calc == "total",] %>% group_by(pop, monitoring_event) %>% 
  summarise(mean_surv = mean(as.numeric(survival)), 
            sd_surv = sd(as.numeric(survival)),
            se_surv = std.error(as.numeric(survival)),
            .groups = 'drop')
mortality_pop_surv

# by site
mortality_site_surv <- mortality_df[mortality_df$calc == "total",] %>% group_by(bag_site, monitoring_event) %>% 
  summarise(mean_surv = mean(as.numeric(survival)), 
            sd_surv = sd(as.numeric(survival)),
            se_surv = std.error(as.numeric(survival)),
            .groups = 'drop')
mortality_site_surv

# by site and population
mortality_popsite_surv <- mortality_df[mortality_df$calc == "total",] %>% group_by(bag_site, pop, monitoring_event) %>% 
  summarise(mean_surv = mean(as.numeric(survival)), 
            sd_surv = sd(as.numeric(survival)),
            se_surv = std.error(as.numeric(survival)),
            .groups = 'drop')
mortality_popsite_surv

# for final time point, calculating mortality from amount put back
mortality_popsite_surv2 <- mortality_df[mortality_df$calc == "since_2",] %>% group_by(bag_site, pop, monitoring_event) %>% 
  summarise(mean_surv = mean(as.numeric(survival)), 
            sd_surv = sd(as.numeric(survival)),
            se_surv = std.error(as.numeric(survival)),
            .groups = 'drop')
mortality_popsite_surv2

mort_summary <- mortality_popsite_surv %>% 
#  filter(monitoring_event == 1, bag_site == "Lewisetta") %>%
  arrange(-mean_surv, .by_group = T) %>%
  as.data.frame()

lew3 <- mort_summary[mort_summary$monitoring_event == 3 & mort_summary$bag_site == "Lewisetta",]
lew2 <- mort_summary[mort_summary$monitoring_event == 2 & mort_summary$bag_site == "Lewisetta",]
lew1 <- mort_summary[mort_summary$monitoring_event == 1 & mort_summary$bag_site == "Lewisetta",]
york3 <- mort_summary[mort_summary$monitoring_event == 3 & mort_summary$bag_site == "YorkRiver",]
york2 <- mort_summary[mort_summary$monitoring_event == 2 & mort_summary$bag_site == "YorkRiver",]
york1 <- mort_summary[mort_summary$monitoring_event == 1 & mort_summary$bag_site == "YorkRiver",]

sink("results/mortality_sites.txt")
york1
york2
york3
lew1
lew2
lew3
sink()
###########

# Visualize survival
####################
# pops df for plotting
popsdf <- data.frame(pop = c("TX","LA","FL","JR","DEBY","LOLA","NH","ME","LARMIX","SEEDMIX"), 
                     order = 1:10, 
                     #                     cols = c("#6f1926","#de324c","#f4895f","#f8e16f","#95cf92","#369acc",
                     #                              "#9656a2","#cbabd1","black","gray"),
                     cols = c("#332288","#117733","#44AA99","#88CCEE","#DDCC77","#CC6677",
                              "#AA4499","#882255","black","gray"),
                     shape = c(21,21,21,21,23,23,21,21,24,24),
                     label = c("W1-TX","W2-LA","W3-FL","W4-VA","S1-LOLA","S2-DEBY",
                               "W5-NH","W6-ME","H1-LARMIX","H2-SEEDMIX"))

# plotting survival by population & site at final timepoint
ggplot(mortality_popsite_surv[!mortality_popsite_surv$pop == "LARMIX" & !mortality_popsite_surv$pop == "SEEDMIX" & mortality_popsite_surv$monitoring_event == 3,], 
       aes(x = pop, y = mean_surv, fill = bag_site)) +
  theme_classic() +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(name = "Site", values = c("cadetblue2","cyan4"),
                    labels = c("Lewisetta","York River")) +
  theme(plot.title = element_text(size = 24), 
        legend.title = element_text(size = 15),
        legend.position = "right") +
  ylim(0,0.2) +
  labs(fill = "Site") +
  xlab("Population") + 
  ylab("Mean Survival") +
  ggtitle("Survival by Population & Site Fall 2024")

ggplot(mortality_popsite_surv2[!mortality_popsite_surv2$pop == "LARMIX" & !mortality_popsite_surv2$pop == "SEEDMIX",], 
       aes(x = pop, y = mean_surv, fill = bag_site)) +
  theme_classic() +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(name = "Site", values = c("cadetblue2","cyan4"),
                    labels = c("Lewisetta","York River")) +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 15),
        legend.position = "right") +
  ylim(0,1) +
  labs(fill = "Site") +
  xlab("Population") + 
  ylab("Mean Survival") +
  ggtitle("Survival by Population & Site, Fall '24 (After Thinning)")

# prior to thinning
ggplot(mortality_popsite_surv[!mortality_popsite_surv$pop == "LARMIX" & !mortality_popsite_surv$pop == "SEEDMIX" & mortality_popsite_surv$monitoring_event == 2,], 
       aes(x = pop, y = mean_surv, fill = bag_site)) +
  theme_classic() +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(name = "Site", values = c("cadetblue2","cyan4"),
                    labels = c("Lewisetta","York River")) +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 15),
        legend.position = "right") +
  ylim(0,1) +
  labs(fill = "Site") +
  xlab("Population") + 
  ylab("Mean Survival") +
  ggtitle("Survival by Population & Site, Spring '24")

# add colors for each population
mortality_popsite_surv_mg <- merge(mortality_popsite_surv, popsdf, by = c("pop"))

# plot survival through time at each site - i'm stupid this is fucking up my color palettes
lew_surv <- ggplot(data = mortality_popsite_surv_mg[mortality_popsite_surv_mg$bag_site == "Lewisetta" & !mortality_popsite_surv_mg$pop == "LARMIX" & !mortality_popsite_surv_mg$pop == "SEEDMIX",]) + 
  geom_line(aes(x = monitoring_event, y = mean_surv, color = fct_reorder(pop,order))) + 
  scale_color_manual(name = "Population", values = popsdf$cols)+
  geom_point(aes(x = monitoring_event, y = mean_surv, fill = fct_reorder(pop,order)), 
             shape = 21, color = "black", size = 4) + 
  scale_fill_manual(name = "Population", values = popsdf$cols)+
  ylim(0,1) + 
  labs(color = "Population") +
  ylab("Survival") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.justification = "top") +
  ggtitle("(A) Lewisetta: Oyster Survival thru Time")
lew_surv

york_surv <- ggplot(data = mortality_popsite_surv_mg[mortality_popsite_surv_mg$bag_site == "YorkRiver" & !mortality_popsite_surv_mg$pop == "LARMIX" & !mortality_popsite_surv_mg$pop == "SEEDMIX",]) + 
  geom_line(aes(x = monitoring_event, y = mean_surv, color = fct_reorder(pop,order))) + 
  scale_color_manual(name = "Population", label = popsdf$pop, values = popsdf$cols)+
  geom_point(aes(x = monitoring_event, y = mean_surv, fill = fct_reorder(pop,order)), 
             shape = 21, color = "black", size = 4) + 
  scale_fill_manual(name = "Population", label = popsdf$pop, values = popsdf$cols)+
  ylim(0,1) + 
  labs(color = "Population") +
  ylab("Survival") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.justification = "top") +
  ggtitle("(B) York River: Oyster Survival thru Time")
york_surv
#####################

# Size data
###########
# calc bag level averages for each event
event1_bags <- tapply(tagsbags6[tagsbags6$monitoring_event == 1,]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 1,]$bags_label,
                      mean)
event2_bags <- tapply(tagsbags6[tagsbags6$monitoring_event == 2,]$length,
                      tagsbags6[tagsbags6$monitoring_event == 2,]$bags_label,
                      mean)
event3_bags <- tapply(tagsbags6[tagsbags6$monitoring_event == 3,]$length,
                      tagsbags6[tagsbags6$monitoring_event == 3,]$bags_label,
                      mean)

# put this in data frame
event1_bags_df <- data.frame(bag = rownames(event1_bags), 
                             monitoring_event = 1, 
                             length = event1_bags)
rownames(event1_bags_df) <- NULL
event2_bags_df <- data.frame(bag = rownames(event2_bags), 
                             monitoring_event = 2, 
                             length = event2_bags)
rownames(event2_bags_df) <- NULL
event3_bags_df <- data.frame(bag = rownames(event3_bags), 
                             monitoring_event = 3, 
                             length = event3_bags)
rownames(event3_bags_df) <- NULL

# put it together
lengths_bags_df <- rbind(event1_bags_df, event2_bags_df, event3_bags_df)

# save it
write.csv(lengths_bags_df, "results/lengths_bags_022025.csv")

# calc pop level averages for each event
event1_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                      mean)
event1_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event1_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 1,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 1,]$pop, 
                     mean)
event2_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                      mean)
event2_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event2_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 2,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 2,]$pop, 
                     mean)
event3_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                      mean)
event3_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event3_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 3,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 3,]$pop, 
                     mean)

# put it all into a df
df <- data.frame(site = "Lewisetta", pop = rownames(event1_lew), 
                 monitoring_event = 1, length = event1_lew)
df1 <- data.frame(site = "Lewisetta", pop = rownames(event1_lew),
                  monitoring_event = 2, length = event2_lew)
df3 <- data.frame(site = "Lewisetta", pop = rownames(event1_lew),
                  monitoring_event = 3, length = event3_lew)
df4 <- data.frame(site = "York", pop = rownames(event1_york), 
                  monitoring_event = 1, length = event1_york)
df5 <- data.frame(site = "York", pop = rownames(event1_york),
                  monitoring_event = 2, length = event2_york)
df6 <- data.frame(site = "York", pop = rownames(event1_york),
                  monitoring_event = 3, length = event3_york)
df7 <- data.frame(site = "Overall", pop = rownames(event1_tot), 
                  monitoring_event = 1, length = event1_tot)
df8 <- data.frame(site = "Overall", pop = rownames(event1_lew),
                  monitoring_event = 2, length = event2_tot)
df9 <- data.frame(site = "Overall", pop = rownames(event1_lew),
                  monitoring_event = 3, length = event3_tot)

lengths <- rbind(df, df1, df3, df4, df5, df6, df7, df8, df9)

len_summary <- lengths %>% 
  #  filter(monitoring_event == 1, bag_site == "Lewisetta") %>%
  arrange(-length, .by_group = T) %>%
  as.data.frame()

lew3 <- len_summary[len_summary$monitoring_event == 3 & len_summary$site == "Lewisetta",]
lew2 <- len_summary[len_summary$monitoring_event == 2 & len_summary$site == "Lewisetta",]
lew1 <- len_summary[len_summary$monitoring_event == 1 & len_summary$site == "Lewisetta",]
york3 <- len_summary[len_summary$monitoring_event == 3 & len_summary$site == "York",]
york2 <- len_summary[len_summary$monitoring_event == 2 & len_summary$site == "York",]
york1 <- len_summary[len_summary$monitoring_event == 1 & len_summary$site == "York",]

sink("results/len_sites.txt")
york1
york2
york3
lew1
lew2
lew3
sink()
###########

# Visualize size
################
# merge with popsdf
lengths_mg <- merge(lengths, popsdf, by = c("pop"))

# plot
lew_len <- ggplot(data = lengths_mg[lengths_mg$site == "Lewisetta",]) + 
  geom_line(aes(x = monitoring_event, y = length, color = fct_reorder(pop,order))) + 
  scale_color_manual(name = "Population", label = popsdf$label, values = popsdf$cols)+
  geom_point(aes(x = monitoring_event, y = length, fill = fct_reorder(pop,order), 
                 shape = fct_reorder(pop,order)),
             size = 4) + 
  scale_shape_manual(name = "Population", label = popsdf$label, values = popsdf$shape)+
  scale_fill_manual(name = "Population", label = popsdf$label, values = popsdf$cols)+
  ylim(20,80) + 
  labs(color = "Population") +
  ylab("Shell Length (mm)") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() + 
  ggtitle("(C) Lewisetta: Oyster Shell Length thru Time") +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.justification = "top") +
  ggtitle("Lewisetta: Oyster Shell Length thru Time")
lew_len

york_len <- ggplot(data = lengths_mg[lengths_mg$site == "York",]) + 
  geom_line(aes(x = monitoring_event, y = length, color = fct_reorder(pop,order))) + 
  scale_color_manual(name = "Population", label = popsdf$label, values = popsdf$cols)+
  geom_point(aes(x = monitoring_event, y = length, fill = fct_reorder(pop,order), 
                 shape = fct_reorder(pop,order)),
             size = 4) + 
  scale_shape_manual(name = "Population", label = popsdf$label, values = popsdf$shape)+
  scale_fill_manual(name = "Population", label = popsdf$label, values = popsdf$cols)+
  ylim(20,80) + 
  labs(color = "Population") +
  ylab("Shell Length (mm)") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() + 
  ggtitle("(C) Lewisetta: Oyster Shell Length thru Time") +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.justification = "top") +
  ggtitle("York River: Oyster Shell Length thru Time")
york_len

all_len <- ggplot(data = lengths[lengths$site == "Overall",]) + 
  geom_line(aes(x = monitoring_event, y = length, color = pop)) + 
  scale_color_manual(name = "Population", label = popsdf$pops, values = popsdf$cols)+
  geom_point(aes(x = monitoring_event, y = length, fill = pop), 
             shape = 21, color = "black", size = 4) + 
  scale_fill_manual(name = "Population", label = popsdf$pops, values = popsdf$cols)+
  ylim(20,80) + 
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.position = "right", legend.justification = "top")

all_len
lew_len
york_len
################

# survival as a function of environmental distance
##################################################
# first download environmental data for experimental sites
library(sdmpredictors)
library(raster)

# I have York River monitoring data
# for period from May 2023-May 2024...
york_env <- data.frame(site_name = "York", lat = 37.247284,
                       Mean_min_temperature_C = 3.95, 
                       Mean_max_temperature_C = 30.61, 
                       Mean_Annual_Temperature_C = 17.44,
                       Mean_min_Salinity_ppt = 12.10, 
                       Mean_max_Salinity_ppt = 25.06, 
                       Mean_Annual_Salinity_ppt = 20.06)

# and env data for other sites
envleng <- read.csv("data/ooters_prelim/Survival_Length_Envr_Data.csv")
env_sites <- envleng[,1:8]
env_sites <- env_sites[colnames(york_env)]

# put it together
env_york_sites <- rbind(york_env, env_sites)

# calculate environmental distance
library(vegan)
env_dist <- round(vegdist(env_york_sites[,3:8], method = "euclidian",
                          upper = FALSE, diag = TRUE), 4)
env_dist_mal <- round(vegdist(env_york_sites[,3:8], method = "mahalanobis",
                              upper = FALSE, diag = TRUE), 4)
env_dist_df <- as.data.frame(as.matrix(env_dist))
env_dist_mal_df <- as.data.frame(as.matrix(env_dist_mal))
colnames(env_dist_df) <- rownames(env_dist_df) <- colnames(env_dist_mal_df) <- rownames(env_dist_mal_df) <- env_york_sites$site_name
env_dist_df
env_dist_mal_df

# quickly plot these environmental distances
env_dist_df2 <- env_dist_df
env_dist_df2$pops1 <- rownames(env_dist_df2)
env_dist_long <-env_dist_df2 %>% 
  pivot_longer(-pops1, names_to = "pops2", values_to = "distances") %>%
  as.data.frame()
env_dist_long

popsdf0 <- rbind(popsdf, data.frame(pops = "York", order = 0, cols = "white"))
popsdf1 <- popsdf0
popsdf2 <- popsdf0
colnames(popsdf1) <- c("pops1","order1","cols")
colnames(popsdf2) <- c("pops2","order2","cols")
env_dist_long_mg <- merge(popsdf1[,c("pops1","order1")], env_dist_long, by = "pops1")
env_dist_long_mg2 <- merge(popsdf2[,c("pops2","order2")], env_dist_long_mg, by = "pops2")

library(viridis)
ggplot(env_dist_long_mg2, aes(x = fct_reorder(pops1, order1), 
                              y = fct_reorder(pops2, order2), 
                              fill = distances))+
  theme_classic()+
  geom_tile(color = "black") +
#  geom_text(aes(label = distances), color = "white", size = 4)+
  scale_fill_viridis_c(name = "Euclidean 
Distance", limits = c(0, 30))+
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.position = "right", legend.justification = "top") +
  ylab("")+
  xlab("")+
  ggtitle("(A) Environmental Distance between Sites") +
  theme(plot.title = element_text(size = 18), 
        legend.title = element_text(size = 14),
        legend.position = "right", legend.justification = "top")

# put together environmental distance with survival in York river @ May 2024 time period
# york river survival
york_surv_may24 <- mortality_popsite_surv[mortality_popsite_surv$bag_site == "YorkRiver" & mortality_popsite_surv$monitoring_event == 2, c("pop","mean_surv")]

env_dists_to_york <- data.frame(pop = colnames(env_dist_df[1,2:9]), dist_euc = as.numeric(env_dist_df[1,2:9]),
                                dist_mal = as.numeric(env_dist_mal_df[1,2:9]))
dist_surv <- merge(york_surv_may24, env_dists_to_york, by = c("pop"))
cor(dist_surv$mean_surv, dist_surv$dist_euc) # 0.1551486
cor(dist_surv$mean_surv, dist_surv$dist_mal) # 0.09742658

ggplot(data = dist_surv, aes(x = dist_euc, y = mean_surv)) + 
  theme_classic() +
  geom_point(aes(x = dist_euc, y = mean_surv, fill = pop), size = 4, shape = 21, col = "black") + 
  geom_smooth(method = "lm", color = "black") +
  scale_fill_manual(name = "Population", label = popsdf$pops, values = popsdf$cols) + 
  ylim(0,1) +
  ggtitle("(B) Survival as a Function of Environmental Distance") + 
  xlab("Euclidean Environmental Distance") + 
  ylab("Mean Survival") +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.position = "right", legend.justification = "top")

summary(lm(dist_surv$mean_surv ~ dist_surv$dist_euc))
# Multiple R2 = 0.02407, p-value = 0.7137

ggplot(data = dist_surv,aes(x = dist_mal, y = mean_surv)) + 
  theme_classic() +
  geom_point(aes(x = dist_mal, y = mean_surv, fill = pop), size = 4, shape = 21, col = "black") + 
  geom_smooth(method = "lm", color = "black") +
  scale_fill_manual(name = "Population", label = popsdf$pops, values = popsdf$cols) + 
  ylim(0,1) +
  ggtitle("Survival as a Function of Environmental Distance") + 
  xlab("Mahalanobis Environmental Distance") + 
  ylab("Mean Survival") +
  theme(plot.title = element_text(size = 20), 
        legend.title = element_text(size = 14),
        legend.position = "right", legend.justification = "top")

summary(lm(dist_surv$mean_surv ~ dist_surv$dist_mal))
# Multiple R2 = 0.009492, p-value = 0.8185
##################################################

