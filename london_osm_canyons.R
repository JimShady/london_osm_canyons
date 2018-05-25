rm(list=ls(all=TRUE)) 

library(devtools)
devtools::install_github("r-spatial/sf")
library(sf)
devtools::install_github("ropensci/osmdata")
library(osmdata)
library(raster)
devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(rgeos)
library(data.table)
library(knitr)
library(mapview)
library(RCurl)

print('loading raster')
london_raster <- raster('buildings_raster/UK001L2_LONDON_UA2012_DHM/UK001L2_LONDON_UA2012_DHM.tif')

extent              <- extent(london_raster)
extent              <- as(extent, 'SpatialPolygons')
proj4string(extent) <- crs(london_raster)
latlong             <- CRS("+init=epsg:4326")
extent              <- spTransform(extent, latlong)

min_x   <- extent(extent)[1]
max_x   <- extent(extent)[2]
min_y   <- extent(extent)[3]
max_y   <- extent(extent)[4]
rm(extent)

print('loading roads')

primary           <- opq(bbox = c(min_x, min_y, max_x, max_y)) %>% add_osm_feature(key = 'highway', value='primary') %>% osmdata_sf()
primary           <- primary$osm_lines[,c('osm_id', 'lanes', 'geometry')]
primary$type      <- 'primary'

print('loaded primary')

secondary         <- opq(bbox = c(min_x, min_y, max_x, max_y)) %>% add_osm_feature(key = 'highway', value='secondary') %>% osmdata_sf()
secondary         <- secondary$osm_lines[,c('osm_id', 'lanes', 'geometry')]
secondary$type    <- 'secondary'

print('loaded secondary')

motorway          <- opq(bbox = c(min_x, min_y, max_x, max_y)) %>% add_osm_feature(key = 'highway', value='motorway') %>% osmdata_sf()
motorway          <- motorway$osm_lines[,c('osm_id', 'lanes', 'geometry')]
motorway$type     <- 'motorway'

print('loaded motorway')

trunk             <- opq(bbox = c(min_x, min_y, max_x, max_y)) %>% add_osm_feature(key = 'highway', value='trunk') %>% osmdata_sf()
trunk             <- trunk$osm_lines[,c('osm_id', 'lanes', 'geometry')]
trunk$type        <- 'trunk'

print('loaded trunk')

tertiary          <- opq(bbox = c(min_x, min_y, max_x, max_y)) %>% add_osm_feature(key = 'highway', value='tertiary') %>% osmdata_sf()
tertiary          <- tertiary$osm_lines[,c('osm_id', 'lanes', 'geometry')]
tertiary$type     <- 'tertiary'

print('loaded tertiary')

print('binding roads')

roads             <- rbind(primary, secondary, motorway, trunk, tertiary) 
roads$osm_id      <- as.numeric(as.character(roads$osm_id))

print('bound')

rm(primary, secondary, motorway, trunk, tertiary, max_x, min_x, max_y, min_y)

roads             <- roads[!grepl(';', roads$lanes),]

roads[roads$type == 'primary'     & is.na(roads$lanes),'lanes'] <- 2
roads[roads$type == 'secondary'   & is.na(roads$lanes),'lanes'] <- 2
roads[roads$type == 'motorway'    & is.na(roads$lanes),'lanes'] <- 6
roads[roads$type == 'trunk'       & is.na(roads$lanes),'lanes'] <- 4
roads[roads$type == 'tertiary'    & is.na(roads$lanes),'lanes'] <- 2

roads$width             <- as.numeric(roads$lanes) * 3.15

roads_to_ignore         <- c(9393,11079,14595,15578)
roads                   <- roads[-roads_to_ignore,]

roads                   <- st_transform(roads, 27700)

roads$area              <- st_geometry(st_buffer(roads, dist = 15)) / st_geometry(st_buffer(roads, dist = 10))

roads$geometry          <- st_transform(roads$geometry,4326)
roads$area              <- st_transform(roads$area, 4326)

roads$geometry          <- st_transform(roads$geometry, crs(london_raster)@projargs)
roads$area              <- st_transform(roads$area, crs(london_raster)@projargs)

rm(roads_to_ignore)

road_polygons       <- as(roads$area,'Spatial')
roads$area          <- NULL 

roads$weighted_mean <- NA
roads$cell_count    <- NA
roads$zero_cells    <- NA

print('about extracting data')

extracted <- extract(london_raster, road_polygons, weights=T, na.rm=F)

print('extracted data, using sapply to put into the roads file')

roads$weighted_mean <- sapply(extracted, FUN = function(x)(sum(x[,1] * x[,2])))
roads$cell_count    <- sapply(extracted, FUN = function(x)(length(x[,1])))
roads$zero_cells    <- sapply(extracted, FUN = function(x)(sum(x[,1]==0)))

print('done')

roads$geometry      <- st_transform(roads$geometry, 4326)

print('saving data')
saveRDS(roads, file = "roads_output.rds")
