############################################################################################
# Load and fix stream and lake water quality data
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# this script takes raw data and produces two objects: 
# 'chem.str' (five streams)  and 'chem.lakes' (one lake)

# import data from xls files
library(readxl) 

# import and fix data from streams and lakes ####
# these are raw data files that will be cleaned up after publication
chem.str <- read_xls("streams_chem_vm_fire.xls",
                     sheet = 1, skip = 23)
colnames(chem.str) <- make.names(colnames(chem.str)) # give better column names

chem.lake <- read_xls("lakes_chem_vm_fire.xls",
                      sheet = 1, skip = 22)

# Fix data

# time
chem.str$time <- as.POSIXlt(paste(chem.str$year,chem.str$month, chem.str$day, sep="-"), 
                            format="%Y-%m-%d", tz = "UTC")
chem.lake$time <- as.POSIXlt(paste(chem.lake$year, chem.lake$month, chem.lake$day, sep="-"), 
                             format="%Y-%m-%d", tz = "UTC")
#chem$time <- as.Date(chem$time)

# clean up things
# streams columns with <
col_with_detectLim <- which(sapply(chem.str,function(x) {any(grep("<",x))} ))
# remove < and make a numeric column
chem.str[,col_with_detectLim] <- data.frame(lapply(chem.str[,col_with_detectLim], function(x) {detlev = grepl("<", x, fixed=TRUE)
out = type.convert(gsub("<", "", x))
#print(str(out))
out[detlev] <- out[detlev]/2 
return(out)}
))

# lake columns with <
col_with_detectLim <- which(sapply(chem.lake,function(x) {any(grep("<",x))} ))
# remove < and make a numeric column
chem.lake[,col_with_detectLim] <- data.frame(lapply(chem.lake[,col_with_detectLim], function(x) {detlev = grepl("<", x, fixed=TRUE)
out = type.convert(gsub("<", "", x))
#print(str(out))
out[detlev] <- out[detlev]/2 
return(out)}
))

# remove doubble samples in Gärsjöbäcken
chem.str <- chem.str[ !(chem.str$time == "2014-10-08" 
                        & chem.str$StNamn == "Gärsjöbäcken övre"),]
chem.str <- chem.str[ !(chem.str$time == "2014-10-21" 
                        & chem.str$StNamn == "Gärsjöbäcken nedre"),]
chem.str <- chem.str[ !(chem.str$time == "2014-10-22" & chem.str$StNamn == "Gärsjöbäcken nedre"),]
chem.str <- chem.str[ !(chem.str$StNamn == "Gärsjöbäcken x-traprov nedströms Lugnet"),]
chem.str[chem.str$StNamn == "Gärsjöbäcken nedre", "StNamn"] <- "Gärsjöbäcken"

# select focus streams from:
# "Märrsjöbäcken", "Sågbäcken", "Vallsjöbäcken", "Gärsjöbäcken", 
# "Gottricksbäcken", "Myckelmossbäcken", "Ladängsbäcken"
chem.str <- chem.str[chem.str$StNamn %in% 
                       c("Märrsjöbäcken",  "Vallsjöbäcken", "Gärsjöbäcken", 
                         "Myckelmossbäcken", "Ladängsbäcken"),]
chem.str$StNamn <- factor(chem.str$StNamn)

# select focus lake
chem.lake <- chem.lake[chem.lake$StNamn == "Märrsjön",]
chem.lake <- droplevels(chem.lake)
