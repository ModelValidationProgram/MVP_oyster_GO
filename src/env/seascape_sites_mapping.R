############################
## MAPPING SEASCAPE SITES ##
############################

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

setwd("~/Documents/Github/MVP_oyster_GO")

seascape_sites <- read.csv("data/EnvDat/seascape/SeascapeSamples_site.csv")
exp_pops <- read.csv("data/IndDat/popsdf.csv")[,-1]
exp_pops$region <- c("South","South","South","Local","Local","Local","North","North","Exp","Exp")
exp_pops$ancestral_group <- c("Gulf","Gulf","Atlantic","Atlantic","Selected","Selected","Atlantic","Atlantic","Exp","Exp")

world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- sf::st_crop(world, c(xmin =-100 , xmax = -60, ymin = 24, ymax = 50))
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

ches_ext <- st_bbox(c(xmin =-77.856476 , xmax = -72.762787, ymin = 36.019557, ymax = 39.939163),
                    crs = st_crs(world))
world_chesapeake <- sf::st_crop(world, ches_ext)
coastline_chesapeake <- sf::st_crop(coastline, ches_ext)

seascape_map <- ggplot(data = coastline) +
  geom_sf() +
  theme_classic() +
  labs(title = "Map of Seascape Sampling Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = seascape_sites, aes(x = Longitude_manual, y = Latitude_manual, fill = Region),
             color = "black", shape = 21, size = 2.5) +
  scale_fill_manual(values = c("#6C5A87","#919BCA","#E59ED7","#DE3F8C"),
                    breaks = c("Northeast","Mid-Atlantic","Southeast","Gulf"))
seascape_map

seascape_map_basic <- ggplot(data = coastline) +
  geom_sf() +
  theme_classic() +
  labs(title = "Map of Seascape Sampling Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = seascape_sites, aes(x = Longitude_manual, y = Latitude_manual),
             color = "black", fill = "#3155A9", shape = 21, size = 3)
seascape_map_basic

exp_sites <- ggplot(data = coastline) + 
  geom_sf()+
  theme_classic()+
  labs(title = "Map of Experimental Adult Source Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = exp_pops[1:8,], aes(x = longitude, y = latitude, fill = region),
             color = "black", shape = 21, size = 2.5) +
  scale_fill_manual(values = c("#6C5A87","#E59ED7","#DE3F8C"),
                    breaks = c("North","Local","South"))
exp_sites

exp_sites_basic <- ggplot(data = coastline) + 
  geom_sf()+
  theme_classic()+
  labs(title = "Map of Experimental Adult Source Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = exp_pops[1:8,], aes(x = longitude, y = latitude),
             color = "black", fill  = "orange", shape = 21, size = 3)+
  geom_label_repel(data = exp_pops[1:8,],
                   aes(x = longitude, y = latitude, label = site_name),
                   nudge_x = -3,
                   nudge_y = 0.5,
                   fill = "white")
exp_sites_basic

common_gardens <- ggplot(data = coastline_chesapeake) + 
  geom_sf()+
  theme_classic()+
  labs(title = "Map of Common Garden Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-77.856476, -72.762787), ylim = c(36.019557, 39.939163), expand = FALSE) +
  geom_point(data = exp_pops[9:10,], aes(x = longitude, y = latitude, fill = site_name),
             color = "black", shape = 21, size = 3)+
  scale_fill_manual(values = c("#00fa9a","#1d90ff"),
                    breaks = c("LEWISETTA","YORKRIVER")) +
  geom_label_repel(data = exp_pops[9:10,],
                   aes(x = longitude, y = latitude, label = site_name),
                   nudge_x = 1.7,
                   nudge_y = 0.5,
                   fill = "white")
common_gardens