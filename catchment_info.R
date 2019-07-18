############################################################################################
# Calculate proportion peatland, lakes, burned area etc
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# load catchment and lake shapefile
# transform to correct projection
library(rgeos)
library(raster)
require(rgdal)
catch <- readOGR(dsn = "./catch", layer = "catchments_wgs84")
lakes <- readOGR(dsn = "./catch", layer = "lakes")
burnt <- readOGR(dsn = "./catch", layer = "burnt_area")
peat <- readOGR(dsn = "./catch", layer = "peat_cover_wgs84")

catch.t <- spTransform(catch, CRS("+init=epsg:3006"))
lakes.t <- spTransform(lakes, CRS("+init=epsg:3006"))
burnt.t <- spTransform(burnt, CRS("+init=epsg:3006"))
peat.t <- spTransform(peat, CRS("+init=epsg:3006"))

# calculate proportion burned
idat <- gIntersection( catch.t,burnt.t,byid=TRUE)
idat$area <- area(idat)
# a vallsjöbäcken is split up into two polygons. We need to merge them first.
idat@data$catch <- c(1,2,2,3:6)
idat2 = aggregate(idat, by = "catch")
idat2$area <- area(idat2)
idat2@data$name <- catch.t@data$namn
idat2@data$whole_area <- area(catch.t)
#proportion burnt
(idat2@data$whole_area-idat2@data$area) / idat2@data$whole_area*100

# calculate proportion lake
idat <- raster::intersect(catch.t,  lakes.t)
idat$area <- area(idat)
lakes <- aggregate(area ~ namn, idat@data, sum)
catch.t$area <- area(catch.t)
prop.lakes <- merge(catch.t@data, lakes, by = "namn", all.x=TRUE)
prop.lakes$area.y[is.na(prop.lakes$area.y)] <- 0
prop.lakes$prop.lakes <- (prop.lakes$area.y/prop.lakes$area.x) * 100

# calculate proportion peatland
idat <- raster::intersect(catch.t,  peat.t)
idat$area <- area(idat)
peat <- aggregate(area ~ namn, idat@data, sum)
catch.t$area <- area(catch.t)
prop.peat <- merge(catch.t@data, peat, by = "namn", all.x=TRUE)
prop.peat$prop.peat <- (prop.peat$area.y/prop.peat$area.x) * 100

# Get coordiantes for sampling points
# load chemical data to get coordinates
source("load_stream_lake_chem_data.R")
# fix polygons
loc <- unique(data.frame(chem.str$StNamn, chem.str$SWEREF_N, chem.str$SWEREF_E))
colnames(loc) <- c("name", "lat", "lon")
loc.lake <- unique(data.frame(chem.lake$StNamn, chem.lake$SWEREF_N, chem.lake$SWEREF_E))
colnames(loc.lake) <- c("name", "lat", "lon")
loc <- rbind(loc,loc.lake)
loc <- loc[!(is.na(loc$lat)),]
coordinates(loc) <- ~ lon + lat
projection(loc) <- "+init=epsg:3006"
loc <- spTransform(loc, CRS("+init=epsg:4326"))
coordinates(loc)
