###########################
## INTERPOLATING ENV DAT ##
###########################

# setup
setwd("~/Documents/GitHub/MVP_oyster_GO")

# libraries
library(dplyr)
library(tidyverse)
library(sf)
library(terra)
library(gstat)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(stars)

# data
dat <- read.csv("data/EnvDat/env_scaled_2025-09-22.csv")

# create a shapefile
dat_sf <- st_as_sf(dat, 
                   coords = c("Longitude_manual","Latitude_manual"),
                   crs = 4326) # for WGS84 projection

# set spherical geometry
sf::sf_use_s2(FALSE)

# set object
world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- sf::st_crop(world, c(xmin =-102, xmax = -65, ymin = 24, ymax = 47.05))
coast <- ne_coastline(scale = "medium", returnclass = "sf")
coast_crop <- sf::st_crop(coast, c(xmin =-102, xmax = -65, ymin = 24, ymax = 47.05))

# test map
map_test <- ggplot(data = world) +
  theme_bw()+
  geom_sf(data = world_crop, fill = 'antiquewhite1') +
  geom_sf(data = dat_sf, mapping = aes(col = salinity_quantile_10)) +
  theme(plot.title = element_text(size = 24), panel.grid.major = element_line(color = "aliceblue"),
        panel.background = element_rect(fill = "aliceblue"), legend.position = 'right')+
  labs(title = "Salinity 10% Quantile (PPT)", x = "Longitude", y = "Latitude", fill = "Genomic \n Offset")
(map_test)

# looks good, now determine area for interpolation
# start by downloading and cropping oceans data
oceans110 <- ne_download(scale=110L, type = "ocean", category = "physical")
oceans_crop <- sf::st_crop(oceans110, c(xmin = -102, xmax = -65, ymin = 24, ymax = 47.05))

# check crs
st_crs(oceans_crop)
st_crs(coast_crop)
st_crs(world_crop) # looks good

# define buffer distance
dist <- 2.5 # 2.5º difference

# define buffer region - note we need to specify the layer, as there are lots of different coastlines
buffered_region <- st_buffer(coast_crop, dist)[51,]
plot(buffered_region, col = "lightblue2")

# crop shapefile to region of interest for interpolation
# this is the region we will plot, but we need to interpolate the entire buffered region
int_region <- st_intersection(oceans_crop, buffered_region) 

# define grid across ocean area
grid <- st_make_grid(buffered_region, cellsize = 0.1, what = "polygons")
grid_int <- st_intersection(grid, buffered_region)

# interpolate
interpolated_sal10 <- idw(salinity_quantile_10_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_sal10, "data/results/lg_results/interpolated_sal10.rds")

interpolated_sal90 <- idw(salinity_quantile_90_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_sal90, "data/results/lg_results/interpolated_sal90.rds")

interpolated_temp10 <- idw(temp_quantile_10_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_temp10, "data/results/lg_results/interpolated_temp10.rds")

interpolated_temp90 <- idw(temp_quantile_90_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_temp90, "data/results/lg_results/interpolated_temp90.rds")

interpolated_dermo <- idw(Dermo_Prevalence_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_dermo, "data/results/lg_results/interpolated_dermo.rds")

interpolated_pea <- idw(Pea_crab_scaled~1, dat_sf, grid_int)
saveRDS(interpolated_pea, "data/results/lg_results/interpolated_pea.rds")

# empty raster
ext <- ext(st_bbox(interpolated_sal10)) # get spatial extent
rast_empty <- rast(ext, resolution = 0.1, crs = st_crs(interpolated_sal10)$wkt)

# create a raster stack
# set up
sf_ls <- c(interpolated_sal10, interpolated_sal90, # sal
           interpolated_temp10, interpolated_temp90, # temp
           interpolated_dermo, interpolated_pea) # disease
rast_ls <- list()

# loop through sf files to rasterize
for (i in length(sf_ls)) {
  sf = sf_ls[i]
  rast = terra::rasterize(sf, rast_empty, field = "var1.pred", background = NA)
  rast_ls[[i]] = rast
}

# combine
rast_all <- rast(rast_ls)
names(rast_all) <- c("interpolated_sal10", "interpolated_sal90", # sal
                     "interpolated_temp10", "interpolated_temp90", # temp
                     "interpolated_dermo", "interpolated_pea") # disease

# save
saveRDS(rast_all, "data/EnvDat/seascape/raster_interpolated_scaled_env.RDS")
