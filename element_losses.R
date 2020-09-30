############################################################################################
# Calculate direct C and N loss from soil and ground vegetation 
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# required packages
library(readxl)
library(ggplot2)
library(dplyr)

# load data for reference sites and merge the data
pdat <- read_xlsx("reference_plots_soil.xlsx", sheet = "coord_mass_chem")
plots <- read_xlsx("reference_plots_soil.xlsx", sheet = "plotdata")

plots$id <- paste(plots$landscape_transect, plots$landscape_transect_nr, sep="-")
dat <- merge(plots, pdat, by="id", sort= FALSE)

# shrub cover
dat <- dat %>% mutate(ling = ifelse(grepl("li", fält), 1,0), 
               blue = ifelse(grepl("bl", fält), 1,0),
               odon = ifelse(grepl("od", fält), 1,0))

cov <- aggregate(cbind(ling, blue, odon) ~ landscape_transect.x + landscape_transect_nr.x+id, data=dat, mean)
aggregate(cbind(ling, blue, odon) ~ landscape_transect.x , data=cov, mean)
apply(dat[,c("ling","blue","odon")],2,mean)
apply(dat[dat$landscape_transect.x %in% c(1,4,6,8),c("ling","blue","odon")],2,mean)
# the transects used for vegetation plots show similar values as the mean for all transeects.
# Thus, the transect used for vegetation plots can be viewed as representative.

# shrub cover with veg plots
vegp <- read_xlsx("reference_plots_shrub_cover.xlsx")
vegp <- vegp %>% mutate_all(funs(replace(., is.na(.), 0)))
vegp <- vegp[vegp$com.!= "Not done",]
vegp$transect <- factor(vegp$transect)
trans.means <- aggregate(cbind(V.myrtillus, V.vitis_idaea, R.tomentosum,
                               V.uliginosum, Calluna.vulgaris) ~ transect, data=vegp, mean)
hab.means <- aggregate(cbind(V.myrtillus, V.vitis_idaea, R.tomentosum,
                             V.uliginosum, Calluna.vulgaris) ~ habitat, data=vegp, mean)
# # end shrub cover

# calculate bulk density (BD) at reference sites 
  # first keep the 5 subplots where samples were taken (middle and 5m away in all directions)
plots.bd <- plots[plots$transect == 5 | plots$transect == 0,]
  # calculate BD for vegetation layer (top "soil" layer) 
pp <- aggregate(cbind(s_cm, OH_cm) ~ landscape_transect + landscape_transect_nr+id, plots.bd, sum)
dat <- merge(pp, pdat, by="id", sort= FALSE)
dat$s_meanBD <- (dat$veg_layer_g*1e-3) / (dat$s_cm * pi*5*5*1e-6) # kg per m3
  # calculate BD for soil layer 
dat$oh_meanBD <- (dat$c_h_soil_g*1e-3) / (dat$OH_cm * pi*5*5*1e-6) # kg per m3
  # calculate carbon and nitrogen BD
dat$s_c_meanBD <- dat$s_meanBD * (dat$C_perc_fol/100)
dat$s_n_meanBD <- dat$s_meanBD * (dat$N_perc_fol/100)
dat$oh_c_meanBD <- dat$oh_meanBD * (dat$C_perc_soil/100)
dat$oh_n_meanBD <- dat$oh_meanBD * (dat$N_perc_soil/100)
dat$s_p_meanBD <- dat$s_meanBD * (dat$P_perc_soil/100)
dat$s_s_meanBD <- dat$s_meanBD * ((dat$S_mg_kg_soil*1e-4)/100) # mg per kg to percent
dat$s_ca_meanBD <- dat$s_meanBD * (dat$Ca_perc_soil/100)
dat$s_k_meanBD <- dat$s_meanBD * (dat$K_perc_soil/100)
dat$oh_p_meanBD <- dat$oh_meanBD * (dat$P_perc_fol/100)
dat$oh_s_meanBD <- dat$oh_meanBD * ((dat$S_mg_kg_fol*1e-4)/100) # mg per kg to percent
dat$oh_ca_meanBD <- dat$oh_meanBD * (dat$Ca_perc_fol/100)
dat$oh_k_meanBD <- dat$oh_meanBD * (dat$K_perc_fol/100)


# mean BD values for C and N
oh_c_meanBD <- mean(dat$oh_c_meanBD, na.rm = TRUE) #mean soil carbon BD kg per m3
s_c_meanBD <- mean(dat$s_c_meanBD, na.rm = TRUE) #mean veg layer carbon BD kg per m3
oh_n_meanBD <- mean(dat$oh_n_meanBD, na.rm = TRUE) #mean soil nitrogen BD kg per m3
s_n_meanBD <- mean(dat$s_n_meanBD, na.rm = TRUE) #mean veg layer nitrogen BD kg per m3

oh_p_meanBD <- mean(dat$oh_p_meanBD, na.rm = TRUE) #mean soil P BD kg per m3
s_p_meanBD <- mean(dat$s_p_meanBD, na.rm = TRUE) #mean veg layer P BD kg per m3
oh_s_meanBD <- mean(dat$oh_s_meanBD, na.rm = TRUE) #mean soil S BD kg per m3
s_s_meanBD <- mean(dat$s_s_meanBD, na.rm = TRUE) #mean veg layer S BD kg per m3
oh_ca_meanBD <- mean(dat$oh_ca_meanBD, na.rm = TRUE) #mean soil Ca BD kg per m3
s_ca_meanBD <- mean(dat$s_ca_meanBD, na.rm = TRUE) #mean veg layer Ca BD kg per m3
oh_k_meanBD <- mean(dat$oh_k_meanBD, na.rm = TRUE) #mean soil K BD kg per m3
s_k_meanBD <- mean(dat$s_k_meanBD, na.rm = TRUE) #mean veg layer K BD kg per m3

  # mean soil and "vegetation" depth
pp <- aggregate(cbind(OH_cm, s_cm) ~ landscape_transect + landscape_transect_nr+id, plots, mean)
colnames(pp)[4:5] <- c("mean_OH_cm", "mean_s_cm")
dat <- merge(pp[,3:5], dat, by="id", sort= FALSE)

# Plot 9-6 in peatland so remove for upland calculations
dat <- dat[dat$id != "9-6",]

SL <- mean(dat$mean_s_cm) # veg layer
OL <- mean(dat$mean_OH_cm, na.rm = TRUE) # org soil layer

# plot C stock versus soil moisture. No indication that moisture can predict C stock
# or just bulk density
ggplot(dat, aes(y=oh_c_meanBD*mean_OH_cm*0.01,  x=soil_moist,colour=factor(landscape_transect.x))) +
  geom_point() +
  facet_wrap(~landscape_transect.x)

# Calculate C lost at each grid plot ####
# load burn severity data
dat.burn <- read_xlsx("burn_severity_data_back.xlsx", sheet = "main")

# Remove two duplicates
dat.burn <- dat.burn[dat.burn$duplicate == "0",]

# Remove non-measured points
dat.burn <- dat.burn[!(is.na(dat.burn$dob) & is.na(dat.burn$humus_depth)),]

# HERE THAT DOB MEASUREMENTS ARE TAKEN FIRST WHEN BOTH EXISTS
#
dat.burn$humus_depth.orig <- dat.burn$humus_depth
dat.burn$humus_depth <- ifelse(!(is.na(dat.burn$humus_depth)) &  !(is.na(dat.burn$dob)), 
                               NA, dat.burn$humus_depth)

# indicate unburned plots 
dat.burn$calcDOB <- ifelse(dat.burn$`soil_moss_burn_sev(0_3)` == 0 | dat.burn$dob == 0, 0, NA) # NA if burned
dat.burn$calcAGloss <- ifelse(dat.burn$`burn_sev_shrubs(0_3)` < 2, 0, NA) # NA if burned

dd <- dat.burn %>% group_by(id_samp) %>% summarize(burned = sum(is.na(calcDOB)), 
                                                   unburned = sum(calcDOB==0, na.rm=TRUE),
                                                   burnedAG = sum(is.na(calcAGloss)),
                                                   unburnedAG = sum(calcAGloss==0, na.rm=TRUE)) 
dd$propBurned <- dd$burned/(dd$burned + dd$unburned)
dd$propBurnedAG <- dd$burnedAG/(dd$burnedAG + dd$unburnedAG)

# any plots where only above-ground plants burned but not the soil? 
sum(dd$propBurnedAG != 0 & dd$propBurned == 0) # NO! Fine to sort on if soil burned

# use only upland data, non-DOB measuremnts and only burned subplots
 dat.left <- dat.burn[dat.burn$habitat_type %in% "upland" & !(dat.burn$habitat_type %in% "fen") & 
                        !(dat.burn$habitat_type %in% "bog") & !(dat.burn$habitat_type %in% "peat"), ]

    dat.left.hum <- dat.burn
mean.left <- dat.left.hum %>% group_by(id_samp) %>% filter(is.na(dob)) %>%
  summarize(hum.left = mean(humus_depth), hum.plots = length(humus_depth)) # subplot mean humus layer depth left and number of subplots
    
    # calculate depth of burn (DOB) by (1) use the mesured DOB and also register the amount of subplots measured
    #                                  (2) taking mean organic layer depth in reference sites, minus remaining organic layer
mean.dob <- dat.left.hum %>% group_by(id_samp)  %>% filter(!(is.na(dob))) %>% 
  summarize(dob.meas = mean(dob), dob.plots = length(dob))
mean.left$dob.calc <-  OL - mean.left$hum.left # OL = mean in reference sites

# join the two measurements
dobs <- full_join(mean.left, mean.dob, by = "id_samp")
# merge to get coords and other data
dobs <- left_join(dobs, distinct(select(dat.burn, id_samp, Sweref99.3006.x, Sweref99.3006.y, 
                                         habitat_type, area, NAMN, VOL)) , 
                   by="id_samp")

# some samples on edge between upland and peatland. Here, we separate direct DOB measurements as peatland
# and remaining organic soil as uplands. We need to label plots as forest (can have some peat in them), peatland,
# and edge (plots covering both a peatland and forest)
dobs$hab <- factor(ifelse(grepl(paste(c("fen", "bog", "peat"), collapse="|"), dobs$habitat_type,ignore.case = TRUE)
                          & !(grepl(paste(c("upland", "edge", "mix"), collapse="|"), dobs$habitat_type,ignore.case = TRUE)), 
                           "peat", 
                          ifelse(grepl("edge", dobs$habitat_type,ignore.case = TRUE), "edge", "forest")))
dobs$peat.type <- ifelse(dobs$VOL < 10, "open", "treed") # what type of peatland is in the plot


    #dobs$hab.edge <- factor(ifelse(grepl(paste(c("fen", "bog", "peat","mire", "mix"), collapse="|"), 
     #                                dobs$habitat_type,ignore.case = TRUE) & 
      #                           grepl("edge", 
       #                                dobs$habitat_type,ignore.case = TRUE), 
        #                  "edge", "pure"))


  # merge with proportion burnt in plot
dobs <- left_join(dd, dobs, by= "id_samp")

# Here calculate a value for uplands, forested peatlands, and open peatlands
# then scale these values with proportion cover of the catchment

      # first make sure that unburned plots should have zero losses
dobs[dobs$burned == 0, c("dob.calc", "dob.meas")] <- 0

  # calculate C loss from each type of measurement: (a)DOB through remaining organic soil, and (b) direct DOB measurement
  # For (a), the SL depth has a separate BD (object SL, which is BD for the moss/lichen layer), deeper burn has OL BD
dobs$c.soil.loss <- dobs$dob.calc * 1e-2 * oh_c_meanBD
dobs$n.soil.loss <- dobs$dob.calc * 1e-2 * oh_n_meanBD

dobs$c.glayer.loss <- ifelse(!(is.na(dobs$c.soil.loss)) & dobs$c.soil.loss!=0,
                      SL* 1e-2 *        # average ground layer depth in meter  
                      s_c_meanBD,         # C bulk density for ground layer, kg m-3
                      NA)
dobs$n.glayer.loss <- ifelse(!(is.na(dobs$n.soil.loss)) & dobs$n.soil.loss!=0,
                             SL* 1e-2 *        # average ground layer depth in meter  
                               s_n_meanBD,         # C bulk density for ground layer, kg m-3
                             NA)

# load equations to calculate C and N losses for shrubs
source("~/projects/GG_exp/shrub_allom_equ.R")
# this gives the weith (g) per 0.16 m2
#shrub.equ
#hab.means
# we calculate losses using C concentration 50% and N concentration 1%.
# 0.16m2 is up-scaled to 1 m2
shrub.equ$AG.c.loss.forest <- NA;shrub.equ$AG.c.loss.forestpeat <- NA
shrub.equ$AG.n.loss.forest <- NA;shrub.equ$AG.n.loss.forestpeat <- NA
for (i in 1:4) {
  shrub.equ[i, c("AG.c.loss.forest","AG.c.loss.forestpeat")]  <- shrub.equ[i,"slope"] * hab.means[,1+i] *0.5 * 6.25
  shrub.equ[i, c("AG.n.loss.forest","AG.n.loss.forestpeat")] <- shrub.equ[i,"slope"] * hab.means[,1+i] *0.01 * 6.25
}

# loop through plots to calculate plot losses of C and N depending on the peatland type
dobs$AG.c.peat.loss = NA; dobs$AG.n.peat.loss = NA # make C loss column
for (i in 1:NROW(dobs)) {
  if(dobs$burnedAG[i] == 0|dobs$hab[i] == "forest") next # skip if no AG plants in peatlands burned
  if(dobs$peat.type[i] == "open") {dobs$AG.c.peat.loss[i] <- 0 # C loss from shrubs assumed zero in open peatlands 
  dobs$AG.n.peat.loss[i] <- 0}  # N loss from shrubs assumed zero in open peatlands 
  if(dobs$peat.type[i] == "treed") {dobs$AG.c.peat.loss[i] <- sum(shrub.equ$AG.c.loss.forestpeat)*1e-3 # C loss in kg m-2
  dobs$AG.n.peat.loss[i] <- sum(shrub.equ$AG.n.loss.forestpeat)*1e-3} # N loss kg m-2
}

# calculate C and N losses for the forest habitat
# including the forest part of edge habitats
dobs$AG.c.shrublayer.loss <- ifelse(dobs$burnedAG != 0 & (dobs$hab == "forest"|dobs$hab == "edge"),
                                      sum(shrub.equ$AG.c.loss.forest)*1e-3, # C loss kg m-2
                                   NA)
dobs$AG.n.shrublayer.loss <- ifelse(dobs$burnedAG != 0 & (dobs$hab == "forest"|dobs$hab == "edge"),
                                      sum(shrub.equ$AG.n.loss.forest)*1e-3, # C loss kg m-2
                                      NA)
# merge shrublayer losses to one column
dobs$AG.c.shrublayer.loss.tot <-  ifelse(is.na(dobs$AG.c.shrublayer.loss), 
                                         dobs$AG.c.peat.loss,dobs$AG.c.shrublayer.loss)
dobs$AG.n.shrublayer.loss.tot <-  ifelse(is.na(dobs$AG.n.shrublayer.loss), 
                                         dobs$AG.n.peat.loss,dobs$AG.n.shrublayer.loss)
dobs$AG.n.shrublayer.loss.tot[is.na(dobs$AG.n.shrublayer.loss.tot)] <- 0 # NAs should be zero losses


# For (b), since this is for peatlands, we use BD at 5 cm interval from Granath et al. for natural and drained peatlands
source("peat_bulkdensity_db.R") # get bulk densities for drained and undrained peatlands
v <- seq(0,55,by=5) # create bins for bulk density, 5 cm bins
intv = findInterval(dobs$dob.meas, v) # check how many bins the DOB covers
bd.openpeat = bd.av[bd.av$type=="undrained","bd_kg_m.3"] # BD values for open peatlands, 5 cm intervals
bd.forestpeat = bd.av[bd.av$type=="drained","bd_kg_m.3"] # BD values for forested peatlands, 5 cm intervals
# get C and N bulk density, 55% C and 2% N after Minkkinen and Laine 1998 Can. J. For. Res.
bd.c.openpeat = bd.openpeat*0.55
bd.c.forestpeat = bd.forestpeat*0.55
bd.n.openpeat = bd.openpeat*0.02
bd.n.forestpeat = bd.forestpeat*0.02

  
dobs$c.peat.loss = NA # make C loss column
dobs$n.peat.loss = NA # make C loss column

# loop to estimate C loss in peatlands, unsing the 5 cm bulk density intervals 
# 
for (i in 1:length(dobs$dob.meas)) {
  if(is.na(dobs$dob.meas[i])) next # skip if direct DOB measurement is missing
  part=rep(0,length(bd.openpeat))
  part[1:intv[i]] = c(rep(1, intv[i]-1), (dobs$dob.meas[i] - (intv[i]-1)*5)/5) # proportion burned of the intervals
  if(dobs$peat.type[i] == "open") {dobs$c.peat.loss[i] <- sum(0.05*part*bd.c.openpeat) # C loss in kg m-2, (*0.05 since we work with 5 cm intervals) 
                                   dobs$n.peat.loss[i] <- sum(0.05*part*bd.n.openpeat)}
  if(dobs$peat.type[i] == "treed") {dobs$c.peat.loss[i] <- sum(0.05*part*bd.c.forestpeat)
                                    dobs$n.peat.loss[i] <- sum(0.05*part*bd.n.openpeat)}
  }

  # calculate average and accounting for unburnt subplots
  # some plots have both "remaining organic layer" and DOB measurements, eg when the plot 
  # included mineral soil and some peaty areas. Such plots are treated as "uplands" and a plot mean
  # is calculated as a weighted mean of the two measurements (ie organic soil left and DOB)
  # calculation also considers the number of burned plots
dobs$no.plots <- dobs$burned + dobs$unburned
dobs$wmean.c.plot <- with(dobs, apply(cbind(
                                      c.soil.loss * (hum.plots/ no.plots), # mineral soil C loss
                                      c.glayer.loss * (hum.plots/ no.plots),  # moss/lichen layer C loss
                                      AG.c.shrublayer.loss.tot * propBurnedAG, # shrub layer C loss
                                      c.peat.loss * (dob.plots/ no.plots)),   # peatland C loss
                                      1, sum, na.rm=TRUE ))
dobs$wmean.n.plot <- with(dobs, apply(cbind(
                                      n.soil.loss * (hum.plots/ no.plots), # mineral soil N loss
                                      n.glayer.loss * (hum.plots/ no.plots),  # moss/lichen layer N loss
                                      AG.n.shrublayer.loss.tot *  propBurnedAG, # shrub layer N loss
                                      n.peat.loss * (dob.plots/ no.plots)),   # peatland N loss
                                      1, sum, na.rm=TRUE ))

# For the edge plots we want to split it into a forest and a peat estimate.
# lets make a separate column for each group
dobs$wmean.c.forest <- with(dobs, ifelse(hab == "forest", wmean.c.plot,
                            ifelse(hab == "edge",  
                                   c.soil.loss + c.glayer.loss + 
                                     AG.c.shrublayer.loss* propBurnedAG* (hum.plots/ no.plots), NA)))
dobs$wmean.c.peat.open <- with(dobs, ifelse(hab == "peat" & peat.type == "open", wmean.c.plot,
                               ifelse(hab == "edge" & peat.type == "open", c.peat.loss, NA)))
dobs$wmean.c.peat.forest <- with(dobs, ifelse(hab == "peat" & peat.type == "treed", wmean.c.plot,
                               ifelse(hab == "edge" & peat.type == "treed", 
                                      c.peat.loss + 
                                        AG.c.peat.loss* propBurnedAG*(dob.plots/ no.plots), NA)))

dobs$wmean.n.forest <- with(dobs, ifelse(hab == "forest", wmean.n.plot,
                                         ifelse(hab == "edge",  
                                                n.soil.loss + n.glayer.loss + 
                                                  AG.n.shrublayer.loss*propBurnedAG* (hum.plots/ no.plots), NA)))
dobs$wmean.n.peat.open <- with(dobs, ifelse(hab == "peat" & peat.type == "open", wmean.n.plot,
                                            ifelse(hab == "edge" & peat.type == "open", n.peat.loss, NA)))
dobs$wmean.n.peat.forest <- with(dobs, ifelse(hab == "peat" & peat.type == "treed", wmean.n.plot,
                                              ifelse(hab == "edge" & peat.type == "treed", 
                                                     n.peat.loss + 
                                                       AG.n.peat.loss * propBurnedAG*(dob.plots/ no.plots), NA)))

              # dobs$wmean.n.forest <- ifelse(dobs$hab == "forest", dobs$wmean.n.plot,
              #                               ifelse(dobs$hab == "edge",  dobs$n.soil.loss+ dobs$n.glayer.loss, NA))
              # dobs$wmean.n.peat.open <- ifelse(dobs$hab == "peat" & dobs$peat.type == "open", dobs$wmean.n.plot,
              #                                  ifelse(dobs$hab == "edge" & dobs$peat.type == "open", dobs$n.peat.loss, NA))
              # dobs$wmean.n.peat.forest <- ifelse(dobs$hab == "peat" & dobs$peat.type == "treed", dobs$wmean.n.plot,
              #                                    ifelse(dobs$hab == "edge" & dobs$peat.type == "treed", dobs$n.peat.loss, NA))

# we calculate without shrublayer as well
dobs$wmean.c.forest.soil <- with(dobs, ifelse(hab == "forest", 
                                              wmean.c.plot - AG.c.shrublayer.loss * propBurnedAG* (hum.plots/ no.plots),
                                         ifelse(hab == "edge",  
                                                c.soil.loss + c.glayer.loss, NA)))
dobs$wmean.c.peat.forest.soil <- with(dobs, ifelse(hab == "peat" & peat.type == "treed", 
                                                   wmean.c.plot - AG.c.peat.loss * propBurnedAG*(dob.plots/ no.plots),
                                              ifelse(hab == "edge" & peat.type == "treed", 
                                                     c.peat.loss, NA)))
dobs$wmean.n.forest.soil <- with(dobs, ifelse(hab == "forest", 
                                              wmean.n.plot - AG.n.shrublayer.loss * propBurnedAG* (hum.plots/ no.plots),
                                         ifelse(hab == "edge",  
                                                n.soil.loss + n.glayer.loss, NA)))
dobs$wmean.n.peat.forest.soil <- with(dobs, ifelse(hab == "peat" & peat.type == "treed", 
                                                   wmean.n.plot -  - AG.n.peat.loss * propBurnedAG*(dob.plots/ no.plots),
                                              ifelse(hab == "edge" & peat.type == "treed", 
                                                     n.peat.loss, NA)))


             # Calculate pre-fire O-horizon storage in forest (not peatlands)
             dobs$c.soil.stock.forest <- OL * 1e-2 * oh_c_meanBD + SL*1e-2*s_c_meanBD 
             dobs$n.soil.stock.forest <- OL * 1e-2 * oh_n_meanBD + SL*1e-2*s_n_meanBD
           
             dobs$p.soil.stock.forest <- OL * 1e-2 * oh_p_meanBD + SL*1e-2*s_p_meanBD 
             dobs$s.soil.stock.forest <- OL * 1e-2 * oh_s_meanBD + SL*1e-2*s_s_meanBD
             dobs$ca.soil.stock.forest <- OL * 1e-2 * oh_ca_meanBD + SL*1e-2*s_ca_meanBD 
             dobs$k.soil.stock.forest <- OL * 1e-2 * oh_k_meanBD + SL*1e-2*s_k_meanBD
             

# Select plots that are within the two large catchments, Gärsjöbäcken and Vallsjöbäcken.
# Import shapefile and overlay the plot points
library(rgeos)
library(raster)
require(rgdal)
catch <- readOGR(dsn = "~/catch", layer = "catchments_wgs84") # ./catch
burnt <- readOGR(dsn = "~/catch", layer = "burnt_area")
peat <- readOGR(dsn = "~/catch", layer = "peat_cover_wgs84")
catch.t <- spTransform(catch, CRS("+init=epsg:3006"))
burnt.t <- spTransform(burnt, CRS("+init=epsg:3006"))
peat.t <- spTransform(peat, CRS("+init=epsg:3006"))

coordinates(dobs) = ~ Sweref99.3006.x + Sweref99.3006.y
projection(dobs) <- CRS("+init=epsg:3006")
dobs$catch <- over(dobs, catch.t)$namn
dobs2 <- dobs[!(is.na(dobs$catch)),]
loss.sub <- dobs2[dobs2$catch == "vallsjobacken_whole" | dobs2$catch == "garsjobacken", ]
#loss.sub <- loss.sub[!(is.na(loss.sub$id_samp)),]
loss.sub@data <- droplevels(loss.sub@data)

loss.long <- tidyr::gather(loss.sub@data, key=habitat, value=loss, wmean.c.forest:wmean.n.peat.forest.soil) #wmean.n.peat.forest
loss.split <- aggregate(loss ~ habitat + catch, loss.long, mean)

# overall averages 
# including all data
loss.all.long <- tidyr::gather(dobs@data, key=habitat, value=loss, wmean.c.forest:wmean.n.peat.forest.soil) #wmean.n.peat.forest
loss.all <- aggregate(loss ~ habitat, loss.all.long, mean)
loss.all$se <- aggregate(loss ~ habitat, loss.all.long, function (x) sd(x)/sqrt(length(x)) )$loss

# humus left result
loss.long.hum <- tidyr::gather(loss.sub@data, key=habitat, value=loss, hum.left) #wmean.n.peat.forest
loss.split.hum <- aggregate(loss ~ habitat + catch, loss.long.hum, mean)
#average left
mean(loss.split.hum[,3])
#reduction in humus
(OL * 1e-2 * oh_c_meanBD - mean(loss.split.hum[,3]) * 1e-2 * oh_c_meanBD)/(OL * 1e-2 * oh_c_meanBD)
(OL * 1e-2 * oh_n_meanBD - mean(loss.split.hum[,3]) * 1e-2 * oh_n_meanBD)/(OL * 1e-2 * oh_n_meanBD)


# weighted mean loss, sum of peatlands and forest including vascular plant layer
#loss.split <- loss.split[loss.split$catch == "garsjobacken" | loss.split$catch == "vallsjobacken_whole", ]
loss.split$weights <- c(rep(c(0.66, 0.66, 0.17,0.17, 0.15), 2), rep(c(0.79, 0.79, 0.13, 0.13, 0.05), 2)) 
loss.garsjob <- with(loss.split[loss.split$catch == "garsjobacken",], 
                (loss[c(1,6)]*weights[1] + loss[c(3, 8)]*weights[3] + 
                   loss[c(5,10)]*weights[5])/sum(weights[c(1,3,5)]) )
loss.vallsjob <- with(loss.split[loss.split$catch == "vallsjobacken_whole",], 
                      (loss[c(1,6)]*weights[1] + loss[c(3, 8)]*weights[3] + 
                         loss[c(5,10)]*weights[5])/sum(weights[c(1,3,5)]) )

write.csv(dobs, file = "ll.csv")
#plot(CtotLoss.kg.m2 ~ hab, closs)

# Pre-fire stock
stock.long <- tidyr::gather(loss.sub@data, key=habitat, value=stock, 
                            c.soil.stock.forest:k.soil.stock.forest)
stock.split <- aggregate(stock ~ habitat + catch, stock.long, mean)
# now on weighted basis
# probably not relevant
stock.split$weights <- rep(c(0.66, 0.79), each=6) 
stock.garsjob <- with(stock.split[stock.split$catch == "garsjobacken",], 
                     (stock[c(1:6)]*weights[1])/1)
stock.vallsjob <- with(stock.split[stock.split$catch == "vallsjobacken_whole",], 
                      (stock[c(1:6)]*weights[1])/1 )

# Post-fire stock
# Pre-fire estimate minus the estiamted loss in forests for the two catchments
(OL * 1e-2 * oh_c_meanBD + SL*1e-2*s_c_meanBD) - loss.split[c(2,12),3]
(OL * 1e-2 * oh_n_meanBD + SL*1e-2*s_n_meanBD) - loss.split[c(7,17),3]

