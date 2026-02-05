############################
## MAPPING SEASCAPE SITES ##
############################

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)

setwd("~/Documents/Github/MVP_oyster_GO")

seascape_sites0 <- read.csv("data/EnvDat/seascape/SeascapeSamples_site.csv")
env_scaled <- read.csv("data/EnvDat/env_scaled_2026-01-30.csv")
exp_pops <- read.csv("data/IndDat/popsdf.csv")[,-1]
exp_pops$region <- c("South","South","South","Local","Local","Local","North","North","Exp","Exp")
exp_pops$ancestral_group <- c("Gulf","Gulf","Atlantic","Atlantic","Selected","Selected","Atlantic","Atlantic","Exp","Exp")
exp_pops$site_name_plotting <- gsub("_", "-", exp_pops$site_name)
exp_pops[exp_pops$site_name_plotting == "LEWISETTA",]$site_name_plotting <- "Lewisetta"
exp_pops[exp_pops$site_name_plotting == "YORKRIVER",]$site_name_plotting <- "York River"
seascape_sites0$dataset <- "Seascape"
seascape_sites <- seascape_sites0[!is.na(seascape_sites0$Mean_Annual_Salinity_ppt) | !is.na(seascape_sites0$temp_lon) ,]
exp_pops$dataset <- ifelse(exp_pops$site_name_plotting == "Lewisetta", "Lewisetta", ifelse(exp_pops$site_name_plotting == "York River", "York River", "Experimental"))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- sf::st_crop(world, c(xmin =-100 , xmax = -60, ymin = 24, ymax = 50))
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

ches_ext <- st_bbox(c(xmin =-77.856476 , xmax = -72.762787, ymin = 36.019557, ymax = 39.939163),
                    crs = st_crs(world))
world_chesapeake <- sf::st_crop(world, ches_ext)
coastline_chesapeake <- sf::st_crop(coastline, ches_ext)

seascape_map_basic <- ggplot(data = coastline) +
  geom_sf() +
  theme_classic() +
  labs(title = "Map of Seascape Sampling Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = seascape_sites, aes(x = Longitude_manual, y = Latitude_manual),
             color = "black", fill = "#3155A9", shape = 21, size = 3)
seascape_map_basic

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

# putting the whole thing together
seascape_exp_map <- ggplot(data = coastline) +
  geom_sf() +
  theme_classic() +
  labs(title = "(A) Map of Seascape Populations & Experimental \nSource Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_rect(aes(xmin = -77.856476, xmax = -72.762787, ymin = 36.019557, ymax = 39.939163),
            color = "black",
            fill = "transparent",
            size = 0.5) +
  geom_point(data = seascape_sites, aes(x = Longitude_manual, y = Latitude_manual),
             color = "black", fill = "#3155A9", shape = 21, size = 5) +
  geom_point(data = exp_pops[1:8,], aes(x = longitude, y = latitude),
             color = "black", fill  = "orange", shape = 22, size = 5)+
  geom_label_repel(data = exp_pops[exp_pops$pop %in% c("TX", "LA", "FL", "NH", "ME"),],
                   aes(x = longitude, y = latitude, label = site_name_plotting),
                   nudge_x = -2,
                   nudge_y = 2.5,
                   min.segment.length = 0,
                   fill = "white")
seascape_exp_map

# and a common garden inset panel
common_gardens <- ggplot(data = coastline_chesapeake) + 
  theme_classic() +
  geom_sf() +
  labs(title = "Map of Common Garden Sites",
       x = "Longitude", y = "Latitude", 
       fill = "Site") +
  coord_sf(xlim = c(-77.856476, -72.762787), ylim = c(36.019557, 39.939163), expand = FALSE) +
  geom_point(data = exp_pops[exp_pops$pop %in% c("VA","LOLA","DEBY"),], 
             aes(x = longitude, y = latitude, fill = dataset),
             color = "black", shape = 22, size = 5)+
  geom_point(data = seascape_sites[seascape_sites$Latitude_manual <= 39.939163 & 
                                     seascape_sites$Latitude_manual >= 36.019557 & 
                                     seascape_sites$Longitude_manual <= -72.762787 & 
                                     seascape_sites$Longitude_manual >= -77.856476,], 
             aes(x = Longitude_manual, y = Latitude_manual, fill = dataset),
             color = "black", shape = 21, size = 5) +
  geom_point(data = exp_pops[9:10,], aes(x = longitude-0.15, y = latitude, fill = site_name_plotting),
             color = "black", shape = 24, size = 5)+
  scale_fill_manual(values = c("#3155A9", "orange", "#00fa9a", "#1d90ff"),
                    breaks = c("Seascape","Experimental", "Lewisetta", "York River")) +
  geom_label_repel(data = exp_pops[exp_pops$pop %in% c("DEBY","LOLA"),],
                   aes(x = longitude, y = latitude, label = site_name_plotting),
                   min.segment.length = 0,
                   nudge_x = 1,
                   nudge_y = 0,
                   fill = "white")+
  geom_label_repel(data = exp_pops[exp_pops$pop %in% c("LEWISETTA","YORKRIVER"),],
                   aes(x = longitude-0.15, y = latitude, label = site_name_plotting),
                   nudge_x = -0.7,
                   nudge_y = 0.18,
                   min.segment.length = 0,
                   fill = "white") +
  geom_label_repel(data = exp_pops[exp_pops$pop %in% c("VA"),],
                   aes(x = longitude, y = latitude, label = site_name_plotting),
                   nudge_x = 0.5,
                   nudge_y = -0.7,
                   min.segment.length = 0,
                   fill = "white") +
  theme(legend.position = "right")
common_gardens

# export to ppt
library(officer)
library(rvg)

doc <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = dml(ggobj = seascape_exp_map), location = ph_location_type(type = "body")) %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = dml(ggobj = common_gardens), location = ph_location_type(type = "body"))
print(doc, target = "results/figures/site_maps.pptx")
