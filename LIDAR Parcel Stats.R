library(tidyverse)
library(sf)
library(terra)
library(stars)
library(units)

rm(list=ls())
key <- "40.00-2-18.2"
cty <- "Albany"

# download parcel shape
parcel <- arcpullr::get_spatial_layer('https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/ArcGIS/rest/services/NYS_Tax_Parcels_Public/FeatureServer/1', 
                                          where=str_c("COUNTY_NAME = '",cty,"' AND PRINT_KEY = '",key,"'"))

# search for a lidar file from lidar dem index, iterate to find
# which one is avaliable for this part of state
lidar.url <- tibble()
  
for (i in seq(2,14)) {
  if (nrow(lidar.url) == 0)
    lidar.url <- arcpullr::get_layer_by_poly(str_c('https://elevation.its.ny.gov/arcgis/rest/services/Dem_Indexes/MapServer/',i),
                                                parcel, sp_rel='intersects'
                                                )                                
}

# download and merge all lidar titles that intersect
lidar <- map(lidar.url$DIRECT_DL, \(x) {
  lidar.file <- tempfile(fileext = '.tif')
  download.file(x, destfile = lidar.file)  
  lidar <- rast(lidar.file)
}) %>% sprc %>% merge

# clip lidar to property lines
parcel.v <- vect(parcel)
parcel.v <- project(parcel.v, crs(lidar))
lidar <- crop(lidar, parcel.v, mask=T) 

# convert meters to feet
lidar <- lidar * 3.28084 

# calculate slope then contour 
# ( reduce resolution to 5 meter (16 feet) first otherwise we pick up 
# things like stone walls or other small details on
# the ground )
lidar_slope <- 
  lidar %>%
  aggregate(fact=5) %>%
  terrain(v='slope') %>%
  st_as_stars %>%
  st_contour(breaks=c(seq(0,100,5))) %>%
  st_as_sf 

# table showing slopes
library(gt)
lidar_slope %>%
  mutate(Acres = st_area(.) %>% set_units('acres') %>% drop_units(),
         Percent = Acres/sum(Acres)
         ) %>%
  st_drop_geometry() %>%
  select(-slope) %>%
  gt() %>%
  fmt_number(3, decimals=1) %>%
  fmt_percent(4) %>%
  gtExtras::gt_theme_538()


# calculate aspect (direction of slope) then contour 
# ( reduce resolution to 5 meter (16 feet) first otherwise we pick up 
# things like stone walls or other small details on
# the ground )

dir <- tibble(
  Direction = c(
    "N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"
  ),
  dir = c(0,seq(22.5,360,45))
)

lidar_aspect <-
  lidar %>%
  aggregate(fact=5) %>%
  terrain(v='aspect') %>% 
  st_as_stars %>%
  st_contour(breaks=c(0,seq(22.5,360,45))) %>%
  st_as_sf  %>%
  left_join(dir, join_by(Min == dir))

# table showing aspect
library(gt)
lidar_aspect %>%
  mutate(Acres = st_area(.) %>% set_units('acres') %>% drop_units(),
         Percent = Acres/sum(Acres)
  ) %>%
  st_drop_geometry() %>%
  select(-aspect, -Min, -Max) %>%
  gt() %>%
  fmt_number(2, decimals=1) %>%
  fmt_percent(3) %>%
  gtExtras::gt_theme_538()


# Show Direction of Slope (Aspect)
mapview::mapview(lidar_aspect,zcol='Direction')
