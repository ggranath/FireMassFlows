############################################################################################
# Get peat bulk densities for C-N loss calculations of peatlands 
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# used data are from Granath et al. 2016 (see Methods)
dat.bd <- read.csv("~/projects/brand_vm/burn_sev/bd_boreal_peat.csv")
dat.bd <- dat.bd[dat.bd$microform.type != "postfire", ]

# remove lower depths (only available for Finland)
dat2 <- dat.bd[dat.bd$depth_mean_cm <= 55,]
# giva all Wainfleet data the same mire id (ie merge the two sampling rounds)
dat2[dat2$mire_id == 7, "mire_id"] <- 6

# classify data into 5 cm intervals
dat2$agg_depth_mean_cm <- numeric(nrow(dat2))
for (x in 1:nrow(dat2)) {
  if(dat2$depth_mean_cm[x] > 0 & dat2$depth_mean_cm[x] < 5) {dat2$agg_depth_mean_cm[x] <- 2.5}
  if(dat2$depth_mean_cm[x] >= 5 & dat2$depth_mean_cm[x] < 10) {dat2$agg_depth_mean_cm[x] <- 7.5}
  if(dat2$depth_mean_cm[x] >= 10 & dat2$depth_mean_cm[x] < 15) {dat2$agg_depth_mean_cm[x] <- 12.5}
  if(dat2$depth_mean_cm[x] >= 15 & dat2$depth_mean_cm[x] < 20) {dat2$agg_depth_mean_cm[x] <- 17.5}
  if(dat2$depth_mean_cm[x] >= 20 & dat2$depth_mean_cm[x] < 25) {dat2$agg_depth_mean_cm[x] <- 22.5}
  if(dat2$depth_mean_cm[x] >= 25 & dat2$depth_mean_cm[x] < 30) {dat2$agg_depth_mean_cm[x] <- 27.5}
  if(dat2$depth_mean_cm[x] >= 30 & dat2$depth_mean_cm[x] < 35) {dat2$agg_depth_mean_cm[x] <- 32.5}
  if(dat2$depth_mean_cm[x] >= 35 & dat2$depth_mean_cm[x] < 40) {dat2$agg_depth_mean_cm[x] <- 37.5}
  if(dat2$depth_mean_cm[x] >= 40 & dat2$depth_mean_cm[x] < 45) {dat2$agg_depth_mean_cm[x] <- 42.5}
  if(dat2$depth_mean_cm[x] >= 45 & dat2$depth_mean_cm[x] < 50) {dat2$agg_depth_mean_cm[x] <- 47.5}
  if(dat2$depth_mean_cm[x] >= 50 & dat2$depth_mean_cm[x] < 55) {dat2$agg_depth_mean_cm[x] <- 52.5}
  #if(dat2$depth_mean_cm[x] %in% seq(from=5, to=55, by=5)) {dat2$agg_depth_mean_cm[x] <- dat2$depth_mean_cm[x]}
}
# average over subsamples in sites
    #db.agg <- aggregate(bd_kg_m.3 ~ factor(agg_depth_mean_cm) + site + type + mire_id + climate, dat2, mean)
    #db.agg$group <- factor(paste(db.agg$site, db.agg$type, db.agg$mire_id, sep = "_"))
    #levels(db.agg$climate) <- c("dfb=warm sum cont", "dfc=cont subarctic")
    #levels(db.agg$type) <- c("drained", "restored mined site", "undrained", "unrestored mined site")
    
    # Number of sites in each category
    #apply(table(db.agg$group, db.agg$type), 2, function (x) sum(x>0) ) # peatland status
    #apply(table(db.agg$group, db.agg$climate), 2, function (x) sum(x>0) ) # peatland status

# average over intervals and peatland type
bd.av <- aggregate(bd_kg_m.3 ~ factor(agg_depth_mean_cm) + type, dat2, mean)
# select drained and undrained
bd.av <- bd.av[bd.av$type == "drained"|bd.av$type == "undrained",]
rm(dat.bd) # remove objects
rm(dat2)