#########################
## OYSTER SITE MAPPING ##
#########################

## setup
########
# set wd
setwd("~/MVP_Oyster_Analysis_Camille/MVP_Oysters_Analysis_Camille")

# load libraries
library(tidyverse) # for data wrangling
library(BiocManager) # needed to download specific packages
library(psych) # environmental data cleaning
library(vegan) # environmental data cleaning
library(stringr) # data cleaning
library(sdmpredictors) # for downloading environmental data
library(ggplot2) # plotting
library(s2) # mapping
library(rnaturalearth) # mapping
library(rnaturalearthdata) # mapping
library(maps) # mapping
library(ggspatial) # mapping
########


## load data
############
expsites <- read.csv("data/ooters_prelim/MVP23-FieldBags - sites.csv")
spawntrt <- read.csv("data/ooters_prelim/MVP23-FieldBags - spawn_trt2.csv")
mortality_bag_overall <- read.csv("data/ooters_prelim/mortality_bag_overall_10092024.csv")
mortality_bag_overall <- mortality_bag_overall[,-1]
############


## combine datasets
###################
# prepare spawn data, this has info about source pops
spawntrt_red <- spawntrt[,c("Tank_naming","latitudeDecimal","longitudeDecimal")]
spawntrt_red[,4:5] <- str_split_fixed(spawntrt_red$Tank_naming, "-", 2)
colnames(spawntrt_red) <- c("Tank_naming","latitudeDecimalSource","longitudeDecimalSource","MVP","pop")
spawntrt_red2 <- spawntrt_red[1:10,c("pop","latitudeDecimalSource","longitudeDecimalSource")]

# check bag popsite data
bag_popsite <- mortality_bag_overall[,c("site","pop","bag_num","bags_label")]

# merge
source_bags <- merge(bag_popsite, spawntrt_red2, by = c("pop"), all = T)

# and check exp site lat/long
colnames(expsites) <- c("site","latitudeDecimalExp","longitudeDecimalExp")
expsites$site <- ifelse(expsites$site == "YorkRiver", "York River", "Lewisetta")

# merge
latlong_all_sites <- merge(source_bags, expsites, by = c("site"), all = T)

# create a df with just unique sites and their lat/longs
expsites2 <- expsites
spawntrt_red3 <- spawntrt_red2[1:8,]
colnames(expsites2) <- c("site","latitudeDecimal","longitudeDecimal")
colnames(spawntrt_red3) <- c("site","latitudeDecimal","longitudeDecimal")
sites <- rbind(expsites2, spawntrt_red3)
sites$latitudeDecimal <- as.numeric(sites$latitudeDecimal)
sites$longitudeDecimal <- as.numeric(sites$longitudeDecimal)
sites
###################


## download environmental data
##############################
datasets <- list_datasets(marine = T)
list_layers(datasets)
envr_layers <- load_layers(layercodes = c("BO_ph","BO_salinity","BO_sstmax","BO_sstmean","BO_sstmin","BO_sstrange"), 
                           rasterstack = F)

# this isn't working, come back later
sites_environment <- data.frame(Name = sites$site, 
                                lat = sites$latitudeDecimal,
                                long = sites$longitudeDecimal,
                                BO_ph = raster::extract(envr_layers$BO_ph, sites[,2:3]),
                                BO_salinity = raster::extract(envr_layers$BO_salinity, sites[,2:3]),
                                BO_sstmax = raster::extract(envr_layers$BO_sstmax, sites[,2:3]),
                                BO_sstmean = raster::extract(envr_layers$BO_sstmean, sites[,2:3]),
                                BO_sstmin = raster::extract(envr_layers$BO_sstmin, sites[,2:3]),
                                BO_sstrange = raster::extract(envr_layers$BO_sstrange, sites[,2:3]))
sites_environment
##############################

# Map sites & environment
#########################
# set s2 false
sf::sf_use_s2(FALSE)

# set object
world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- sf::st_crop(world, c(xmin = -97.5, xmax = -65, ymin = 25, ymax = 48))

# check world_crop
ggplot() +
  geom_sf(data = world_crop, 
          #aes(fill = name), 
          show.legend = FALSE) + 
  coord_sf() #+
#  geom_sf_label(data = world_crop, aes(label = name))

# crop environmental layers - this throws errors
oyster <- raster::extent(-97.5, -65, 25, 48)
sal <- raster::crop(envr_layers$BO_salinity, oyster)
sstmean <- raster::crop(envr_layers$BO_sstmean, oyster)
sstmax <- raster::crop(envr_layers$BO_sstmax, oyster)

# map of sampling sites
map_sites <- ggplot(data = world) +
  theme_bw() +
  geom_sf(data = world_crop, fill = 'antiquewhite1') +
  geom_point(data = sites, aes(x=longitudeDecimal, y=latitudeDecimal, 
                               fill = forcats::fct_reorder(site, as.numeric(longitudeDecimal))), 
             size = 3, pch = 21) +
#  geom_point(data = sites[sites$site == c("York River","Lewisetta"),], aes(x=longitudeDecimal, y=latitudeDecimal), 
#             size = 3, pch = 8) +
#  scale_fill_viridis() +
  labs(title = "Sampling Sites", fill = "Site") +
  theme(plot.title = element_text(size = 24), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(color = "aliceblue"),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.position = "right") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Sites") +
  ggtitle("Map of Sampling & Experimental Sites")
(map_sites)

# zoom in on experimental sites
chesapeake_crop <- sf::st_crop(world, c(xmin = -74, xmax = -78, ymin = 39.7, ymax = 36))
ggplot() +
  geom_sf(data = chesapeake_crop, 
          #aes(fill = name), 
          show.legend = FALSE) + 
  coord_sf()

# plot just experimental sites
map_sites_exp <- ggplot(data = world) +
  theme_bw() +
  geom_sf(data = chesapeake_crop, fill = 'antiquewhite1') +
  geom_point(data = sites[sites$site == c("York River", "Lewisetta"),], 
             aes(x=longitudeDecimal, y=latitudeDecimal, fill = site), 
             size = 3, pch = 21) +
  geom_label(data = sites[sites$site == c("York River", "Lewisetta"),],
             aes(x=longitudeDecimal, y=latitudeDecimal, label = site)) +
  labs(title = "Experimental Sites", fill = "Site") +
  theme(plot.title = element_text(size = 24), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(color = "aliceblue"),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.position = "right") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Sites") +
  ggtitle("Map of Experimental Sites")
(map_sites_exp)

# turn rasters into dfs
sstmean_df <- raster::as.data.frame(sstmean, xy = T)
sstmax_df <- raster::as.data.frame(sstmax, xy = T)
sal_df <- raster::as.data.frame(sal, xy = T)

# check dfs
head(sstmean_df)
head(sstmax_df)
head(sal_df)

# set color palettes for sst and salinity
library(RColorBrewer)
cols_sst <- rev(brewer.pal(11, "RdYlBu")) # maximum number of colors in palette YlOrRd is 9
pal_sst <- colorRampPalette(cols_sst)

cols_sal <- brewer.pal(9, "Blues")
pal_sal <- colorRampPalette(cols_sal)

# try ggplot raster maps
# sstmean
map_sstmean <- ggplot(data = world) +
  theme_bw() +
  geom_raster(data = sstmean_df, aes(x = x, y = y, fill = BO_sstmean)) + 
  geom_sf(data = world_crop, fill = "antiquewhite1") +
  coord_sf() + 
  scale_fill_gradientn(colors = pal_sst(20), limits = c(3, 15), na.value = "white") +
  theme(plot.title = element_text(size = 24), 
        panel.grid.major = element_line(color = "aliceblue"),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") +
  labs(fill = "Temperature (ºC)") + 
  ggtitle("Mean Sea Surface Temperature (ºC)")
(map_sstmean)

# salinity
map_sal <- ggplot(data = world) +
  theme_bw() +
  geom_raster(data = sal_df, aes(x = x, y = y, fill = BO_salinity)) + 
  geom_sf(data = world_crop, fill = "antiquewhite1") +
  coord_sf() + 
  scale_fill_gradientn(colors = pal_sal(20), limits = c(0, 36), na.value = "white") +
  theme(plot.title = element_text(size = 24), 
        panel.grid.major = element_line(color = "aliceblue"),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.position = 'right') +
  xlab("Longitude") + 
  ylab("Latitude") + 
  labs(fill = "Salinity (psu)") + 
  ggtitle("Mean Salinity (psu)")
(map_sal)
#########################