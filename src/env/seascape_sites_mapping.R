############################
## MAPPING SEASCAPE SITES ##
############################

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

setwd("~/MVP_Oyster_Analysis_Camille/MVP_Oysters_Analysis_Camille/data")

seascape_sites <- read.csv("SeascapeSamples_site.csv")
exp_pops <- read.csv("MVP23-FieldBags_spawn_trt2.csv")

exp_pops_trim <- exp_pops[1:8,]
exp_pops_trim$Region <- c("Gulf","Gulf","Northeast","Northeast","Mid-Atlantic",
                          "Southeast","Mid-Atlantic","Mid-Atlantic")
exp_pops_trim

world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- sf::st_crop(world, c(xmin =-100 , xmax = -60, ymin = 24, ymax = 50))
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

seascape_map <- ggplot(data = coastline) +
  geom_sf() +
  theme_classic() +
  labs(title = "Map of Seascape Sampling Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = seascape_sites, aes(x = Longitude_manual,
                                        y = Latitude_manual,
                                        fill = Region),
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
  geom_point(data = seascape_sites, aes(x = Longitude_manual,
                                        y = Latitude_manual),
             color = "black", fill = "#3155A9", shape = 21, size = 2)
seascape_map_basic

exp_sites <- ggplot(data = coastline) + 
  geom_sf()+
  theme_classic()+
  labs(title = "Map of Experimental Population Source Sites",
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE) +
  geom_point(data = exp_pops_trim, aes(x = (longitudeDecimal),
                                       y = (latitudeDecimal),
                                       fill = Region),
             color = "black", shape = 21, size = 2.5) +
  scale_fill_manual(values = c("#6C5A87","#919BCA","#E59ED7","#DE3F8C"),
                    breaks = c("Northeast","Mid-Atlantic","Southeast","Gulf"))
exp_sites

