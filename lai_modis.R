############################################################################################
# Analyse LAI from MODIS data
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# install.packages("MODIS")
library(MODIS)

MODISoptions(localArcPath = "/path/to/modis/files", 
             outDirPath = "/path/to/modis/files/processed")

runGdal("MOD13Q1", collection = "006", # see getCollection("MOD13Q1", forceCheck = TRUE)
        tileH = 25, tileV = 6, # MODIS tile id
        begin = "2000.02.18", end = "2000.03.31", # time period
        SDSstring = "110000000000")

# load packages
library(MODISTools)
library(raster)
library(dplyr)
library(tidyr)
library(rts)

# check products, bands and available dates
products <- mt_products()
head(products)
# MCD15A2H 8 day LAI

bands <- mt_bands(product = "MCD15A2H")
head(bands)
av.dates <- mt_dates(product = "MCD15A2H", lat = 59.905809, lon = 16.169129) # dates for this coordinate
av.dates$date <- as.Date(av.dates$calendar_date)
av.dates[av.dates$date > as.Date("2014-05-01") & av.dates$date < as.Date("2019-10-01"), ]
# Jun 15 to July 29 seems like a good period, gives 6 'images' over the summer

# download data
st.en <- data.frame( start = c("2014-06-15","2015-06-15","2016-06-15", "2017-06-15", "2018-06-15", "2019-06-15"),
            end = c("2014-07-28", "2015-07-28", "2016-07-28", "2017-07-28", "2018-07-28", "2019-07-28"))
#se <- st.en[2,]
#se <- droplevels(se)
lai <- get_modis(product = "MCD15A2H", start.end = st.en, km_lr = 10, km_ab = 10, band = c("FparLai_QC","FparExtra_QC","Lai_500m"))

#tt <- mt_subset(product= "MCD15A2H", lat = 59.905809, lon = 16.169129, start = as.character(st.en[1,1]),
#                end = as.character(st.en[1,2]),
#  km_lr = 0,km_ab = 0,        
#  site_name = "sitename",out_dir = tempdir(),internal = TRUE,progress = TRUE)


get_modis <- function (product = "MCD15A2H", lat = 59.905809, lon = 16.169129, start.end = NULL,
                            km_lr = NULL, km_ab = NULL, band = NULL) {
  out = list()
  for (i in 1:nrow(start.end)) {
    st <- as.character(start.end[i,1])
    en <- as.character(start.end[i,2])

out[[i]] <- mt_subset(product = product,
                    lat = lat,
                    lon = lon,
                    band = band,
                    #band = "FparLai_QC",
                    start = st,
                    end =  en,
                    km_lr = km_lr,
                    km_ab = km_ab,
                    site_name = "testsite",
                    internal = TRUE)
  }
  return(out)
}
# make sure that objects are identical except for dates
identical(lai[[1]][,-c(13:17,19,21)], lai[[2]][,-c(13:17,19,21)]) 
identical(lai[[3]][,-c(13:17,19,21)], lai[[4]][,-c(13:17,19,21)]) 
identical(lai[[5]][,-c(13:17,19,21)], lai[[6]][,-c(13:17,19,21)]) 

# if TRUE, merge the objects
subset.lai <- rbind(lai[[1]], lai[[2]], 
                    lai[[3]], lai[[4]],
                    lai[[5]], lai[[6]])

#mt_write(subset.lai, "~/") #save data if wanted

#to raster function
#LC_r <- mt_to_raster(df = lai[[1]])

metainfo <- lai[[1]][1,-c(13:17,19,21,22)]


#MODISGrid(NoDataValues = list("MCD15A2H" = c("Lai_500m" = rep(254,6))))

# modProj <- paste('PROJCS["Sinusoidal",GEOGCS["GCS_Undefined",DATUM["Undefined",',
#                  'SPHEROID["User_Defined_Spheroid",6371007.181,0.0]],PRIMEM["Greenwich",0.0],',
#                  'UNIT["Degree",0.0174532925199433]],PROJECTION["Sinusoidal"],',
#                  'PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],',
#                  'PARAMETER["Central_Meridian",0.0],UNIT["Meter",1.0]]',
#                  sep = "")



# value 248−255 are NAs 
subset.lai$value[subset.lai$value > 247] <- NA
subset.lai <- subset.lai[,-which(colnames(subset.lai) %in% c("units","scale"))] # remove scale column as it is different for bands

dates <- unique(subset.lai$calendar_date) # save downloaded dates
# loop to filter each day (bad cells = NA) and produce a raster list
r.list <- list()
for (i in 1:length(dates)) {
data.file <-subset.lai[subset.lai$calendar_date == dates[i],] 

w.mod <- data.file %>% mutate(pixel = factor(pixel,levels=unique(pixel))) %>% 
  spread(band, value) # put data into wide format (value was before data)
          #qc <- unique(w.mod$FparLai_QC)
          #qc <- qc[order(qc)]
          #for (i in 1:length(qc)) {print(c(tail(rev(as.numeric(intToBits(qc[i]))), 8), qc[i]))}
keep.qc = c(0, 32, 97)  # 32= other method but good to use, 97=other algo used
                        # 2 = aqua sensor used

          #qc.x <- unique(w.mod$FparExtra_QC)
          #qc.x <- qc.x[order(qc.x)]
          #for (i in 1:length(qc.x)) {print(c(tail(rev(as.numeric(intToBits(qc.x[i]))), 8), qc.x[i]))}

keep.qc.x = c(0, 1,
            8,9, # aerosols average-high
            32,33,34, # clouds detected
            34, # cirrus detected
            48, 49,# clouds + cirrus detected
            64,65, #cloud shadow detected
            72,73, #cloud shadow+aerosol detected
            136, #aerosol + other biome
            160,161, # cloud shadow + other biome
            200 ) # aerosol + cloud shadow + other biome

# filter out bad pixels and set them to NA
w.mod$Lai_500m.filt <- with(w.mod, ifelse(FparLai_QC %in% keep.qc 
                                          & FparExtra_QC %in% keep.qc.x
                                          ,Lai_500m, NA))

grid.data <- matrix(w.mod[,"Lai_500m.filt"],
                    nrow = metainfo$nrows, ncol = metainfo$ncols, byrow = TRUE)

r <- raster(grid.data, 
            xmn = as.numeric(metainfo$xllcorner), ymn = as.numeric(metainfo$yllcorner),
            xmx = as.numeric(metainfo$xllcorner)+as.numeric(metainfo$cellsize)*ncol(grid.data), 
            ymx = as.numeric(metainfo$yllcorner)+as.numeric(metainfo$cellsize)*NROW(grid.data),
            crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

r.list[[dates[i]]] <- r
}

# make a raster stack and put it into a time series
rs <- stack(unlist(r.list))
lai.rts <- rts(rs, as.Date(dates))

# calculate annual means, sd
dd <- rts::apply.yearly(lai.rts, FUN = function (x) mean(x, na.rm=TRUE) ) 
#d2 <- projectRaster(dd[[1]], crs = "+init=epsg:4326",method = "ngb") # method = "ngb" often not recommended

# plot year 1
plot(dd)

# load catchment and lake shapefile
# transform to correct projection
require(rgdal)
catch <- readOGR(dsn = "./catch", layer = "catchments")
lakes <- readOGR(dsn = "./catch", layer = "lakes")
lakes.t <- spTransform(lakes, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
catch.t <- spTransform(catch, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

# plot with map
plot(dd[[6]])
plot(catch.t, add=T)
plot(lakes.t, add=T)

#d2[lakes[,]] <- 100
#d3 <- mask(d2, lakes, inverse=TRUE)
#plot.new()
#plot(d3)

# remove pixiels with more than 25% of water
SpP_ras <- rasterize(lakes.t, dd[[1]], getCover=TRUE)
SpP_ras[SpP_ras > 25] <- NA
lai.nolakes <- mask(dd@raster, SpP_ras, inverse=FALSE)

#plot(lai.nolakes)
#plot(lakes.t,  add=TRUE)

# extract values per catchment
v <- raster::extract(lai.nolakes, catch.t, fun = mean, na.rm = TRUE)
v <- data.frame(catch=catch.t@data$namn, lai=v)
#real LAI numbers we multiply with 0.1
v[,2:7] <- v[,2:7]*0.1
v 
# remove 'gottricksbacken' catchment
v <- v[!(v$catch == "gottricksbacken"),]
colnames(v)[2:7] <- 2014:2019
v <- v  %>% gather(year, lai, '2014':'2019') %>% mutate(year, as.numeric(year))
v <- v[order(v$catch),]
v2<-v
# Add dates
dd<-ifelse(v$year=="2014", as.POSIXct("2014-07-10 CEST"), 
                ifelse(v$year=="2015", as.POSIXct("2015-07-10 CEST"), 
                        ifelse(v$year=="2016", as.POSIXct("2016-07-10 CEST"),
                                  ifelse(v$year=="2017", as.POSIXct("2017-07-10 CEST"),
                                                                    ifelse(v$year=="2018", as.POSIXct("2018-07-10 CEST"),
                                                                                                      as.POSIXct("2018-07-10 CEST"))))))
v$year_pos <- as.POSIXct(ifelse(v$year=="2014", "2014-07-10 CEST", 
           ifelse(v$year=="2015", "2015-07-10 CEST", 
                  ifelse(v$year=="2016", "2016-07-10 CEST",
                         ifelse(v$year=="2017", "2017-07-10 CEST",
                                ifelse(v$year=="2018", "2018-07-10 CEST",
                                       "2019-07-10 CEST"))))))

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

lai.fig <- ggplot(v, aes(y=lai, x=year_pos, group=catch, shape=catch)) +
  geom_path() +
  geom_point(size=3) +
  ylim(c(0,4)) +
  scale_x_datetime(limits = c(as.POSIXct("2014-05-01 CEST"), as.POSIXct("2019-07-31 CEST")), 
                   date_breaks = "1 year", date_labels = "%Y") +
  labs(y=expression(paste("Leaf Area Index for June-July")), 
       x= "Year") +
  scale_shape_manual(name=c("Catchment"), 
                     breaks = c("garsjobacken", "ladangsbacken", "marrsjobacken",
                                                     "myckelmossbacken", "vallsjobacken"),
                     values = c(1,2,3,4,5), labels = c("Gärsjöbäcken", "Ladängsbacken", "Märrsjöbäcken",
                                                 "Myckelmossen", "Vallsjöbäcken")) +
  guides(shape=guide_legend(ncol=2,byrow=TRUE)) +
  theme(legend.justification = c(0, 0), 
                    legend.position = c(0.32, 0.72),
                    legend.key.size = unit(0, "line"),
                    legend.text = element_text(size=12), #, face = "italic"),
                    legend.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  draw_plot_label("(c)", x= as.POSIXct("2014-08-31 CEST"), y = 3.8, 
                  hjust = 0, vjust = 0, size=18 )


ggsave(lai.fig, file = "lai.png")


plot(r)
r.wgs84 <- projectRaster(r, crs = "+init=epsg:4326")
writeRaster(r.wgs84, "test3.grd", overwrite=TRUE)
plot(r.wgs84)




# From MODIStools so not needed but put here for documentation. Remove later.
MODISGrid <-
  function(Dir = ".", DirName = "MODIS_GRID", SubDir = TRUE, NoDataValues)  {
    ## DEFINE
    NUM_METADATA_COLS <- 10
    
    if(Dir == '.') cat('Files downloaded will be written to ', file.path(getwd(), DirName), '.\n', sep = '')
    if(Dir != '.') cat('Files downloaded will be written to ', file.path(Dir, DirName), '.\n', sep = '')
    
    ## Create directory for storing new grid files.
    if(!file.exists(file.path(Dir, DirName))) dir.create(path = file.path(Dir, DirName))
    
    ## Find all MODIS data files in Dir.
    file.list <- list.files(path = Dir, pattern = ".csv$")
    file.list <- file.list[grepl("___", file.list)]
    if(length(file.list) == 0) stop("Could not find any MODIS data files in directory specified.")
    
    ## Check NoDataValues is a list of named vectors.
    if(!is.list(NoDataValues)) stop("NoDataValues should be a list of named vectors. See help doc.")
    
    ## Check the number of products in NoDataValues list equals the number found in file.list.
    prod.set <- unique(do.call(c, as.vector(lapply(
      file.path(Dir, file.list), function(x) read.csv(x, header = FALSE, as.is = TRUE, skip = 16)[ ,3]
    ))))
    if(any(nchar(prod.set) == 0)) stop("A subset was incompletely downloaded. Check MODISSubsets output and retry the subset.")
    if(!all(prod.set %in% names(NoDataValues))) stop("Mismatch between NoDataValues and data products found in files.")
    
    ## Check that NoDataValues value is specified for every data band found in file.list.
    band.set <- unique(as.vector(sapply(
      lapply(file.path(Dir, file.list), function(x) read.csv(x, header = FALSE, as.is = TRUE, skip=16)[ ,3]),
      function(x) unique(substr(x, (gregexpr(".", x, fixed = TRUE)[[1]][5] + 1), nchar(x)))
    )))
    if(!all(band.set %in% names(do.call(c, unname(NoDataValues))))){
      stop("Mismatch between NoDataValues and data bands found in files.")
    }
    
    for(i in 1:length(file.list))
    {
      cat("Creating new GIS ASCII files from MODIS data file", i, "out of", length(file.list), "\n")
      
      data.file <- read.csv(file.path(Dir, file.list[i]), skip=15, header = TRUE, as.is = TRUE)
      names(data.file) <- c("nrow", "ncol", "xll", "yll", "pixelsize", "row.id", "product.code", "MODIS.acq.date",
                            "where", "MODIS.proc.date", 1:(ncol(data.file) - NUM_METADATA_COLS))
      
      ## Create directory for this data file if SubDir = TRUE.
      sub.dir <- substr(file.list[i], 1, regexpr(".asc$", file.list[i])-1)
      if(SubDir & !file.exists(file.path(Dir, DirName, sub.dir))) dir.create(path = file.path(Dir, DirName, sub.dir))
      
      for(n in 1:nrow(data.file))
      {
        data.band <- substr(data.file$row.id[n],
                            gregexpr(".", data.file$row.id[n], fixed = TRUE)[[1]][5] + 1,
                            nchar(data.file$row.id[n]))
        data.date <- data.file$MODIS.acq.date[n]
        
        path <- ifelse(SubDir,
                       file.path(Dir, DirName, sub.dir,
                                 paste0("GRID_", sub.dir, "_", data.band, "_", data.date)),
                       file.path(Dir, DirName,
                                 paste0("GRID_", sub.dir, "_", data.band, "_", data.date)))
        
        write(c(sprintf("ncols\t\t %i", data.file$ncol[n]),
                sprintf("nrows\t\t %i", data.file$nrow[n]),
                sprintf("xllcorner\t %.2f", data.file$xll[n]),
                sprintf("yllcorner\t %.2f", data.file$yll[n]),
                sprintf("cellsize\t %s", as.character(data.file$pixelsize[n])),
                sprintf("NODATA_value\t %s", as.character(NoDataValues[[data.file$product.code[n]]][data.band]))),
              file = file.path(paste0(path,".asc")))
        
        WritePRJ(Path = file.path(paste0(path,".prj")))
        
        grid.data <- matrix(data.file[n,(NUM_METADATA_COLS+1):ncol(data.file)],
                            nrow = data.file$nrow[n], ncol = data.file$ncol[n], byrow = TRUE)
        write.table(grid.data, file = file.path(paste0(path,".asc")), append = TRUE, col.names = FALSE, row.names = FALSE)
      }
    }
  }
