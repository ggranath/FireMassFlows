############################################################################################
# Calculate hydrological export
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# needed package
library(readxl)
library(tidyr)
library(dplyr)

# load chemical data
source("load_stream_lake_chem_data.R")
# load stream flow data
flow <- read_xlsx("stream_flows.xlsx",
                  sheet = 1, col_types = c("text", "numeric","numeric", "numeric","numeric", "text"))
flow$time <- as.POSIXct(flow$time, format="%Y-%m-%d", tz="UTC")

# mean water outflow age
flowage  <- flow[flow$time > "2014-07-31" & flow$time < "2015-08-01",]
aggregate(mean_water_age_yr ~ catch, flowage, mean)

#flow$q_m3_s <- type.convert(flow$q_m3_s) # make Q numeric, not sure why but sometimes this is needed 

# total outflow per year (Table 2)
flowsum  <- flow[flow$time > "2010-01-01" & flow$time < "2013-12-31",]
sums <- aggregate(q_m3_s ~ catch, flowsum, mean)
sums[,2] <- sums[,2]*60*60*24*365 #m3 per year per catchment
sums$outflow <- (sums[,2]/(c(1,1,2170,1,1440, 374, 930, 1830)*10000)) #get outflow in meter per year 

# total outflow post-fire
flowsum.1415  <- flow[flow$time > "2014-08-01" & flow$time < "2015-07-31",]
flowsum.1516  <- flow[flow$time > "2015-08-01" & flow$time < "2016-07-31",]
flowsum.1617  <- flow[flow$time > "2016-08-01" & flow$time < "2017-07-31",]
  
sums.1415 <- aggregate(q_m3_s ~ catch, flowsum.1415, mean)
sums.1415[,2] <- sums.1415[,2]*60*60*24*365 #m3 per year per catchment
sums.1415$outflow <- (sums.1415[,2]/(c(1,1,2170,1,1440, 374, 930, 1830)*10000)) #get outflow in meter per year 

sums.1516 <- aggregate(q_m3_s ~ catch, flowsum.1516, mean)
sums.1516[,2] <- sums.1516[,2]*60*60*24*365 #m3 per year per catchment
sums.1516$outflow <- (sums.1516[,2]/(c(1,1,2170,1,1440, 374, 930, 1830)*10000)) #get outflow in meter per year 

sums.1617 <- aggregate(q_m3_s ~ catch, flowsum.1617, mean)
sums.1617[,2] <- sums.1617[,2]*60*60*24*365 #m3 per year per catchment
sums.1617$outflow <- (sums.1617[,2]/(c(1,1,2170,1,1440, 374, 930, 1830)*10000)) #get outflow in meter per year 

cbind(sums.1415, sums.1516[,3],sums.1617[,3])

any(is.na(flow$q_m3_s))# check for NAs

# list nutrient variables
nut = c("SO4_IC.mg.l", "Tot.N_TNb.µg.l","Tot._P.µg.l", "Ca.mg.l", "K.mg.l", 
        "TOC.mg.l") 

# Is element concentration related to flow?
# Fix data frame with flow estimates and element concentration data for both
# catchments
flow.gar <- flow[flow$catch == "Gärsjöbäcken",] # extract flow data for catchment Gärsjöbäcken
flow.gar$time.pos <- as.POSIXlt(flow.gar$time)
flow.gar$time.pos <- as.character(flow.gar$time)
chem.str.sub <- chem.str[chem.str$StNamn == "Gärsjöbäcken",c("time",nut)]
chem.str.sub$time <- as.character(chem.str.sub$time)
cvsf.gar <- merge(flow.gar, chem.str.sub, by.x = "time.pos", by.y = "time")

flow.val <- flow[flow$catch == "Vallsjöbäcken",] # extract flow data for catchment Vallsjön
flow.val$time.pos <- as.POSIXlt(flow.val$time)
flow.val$time.pos <- as.character(flow.val$time)
chem.str.sub <- chem.str[chem.str$StNamn == "Vallsjöbäcken",c("time",nut)]
chem.str.sub$time <- as.character(chem.str.sub$time)
cvsf.val <- merge(flow.val, chem.str.sub, by.x = "time.pos", by.y = "time")

# run regressions for all elements and print R2 values
# Gärsjöbäcken
  #2010 to 2014-07-31
  cvsf.gar10_14 <- cvsf.gar[cvsf.gar$time > "2010-01-01" & cvsf.gar$time < "2014-07-31",]
  for(i in 8:13){
  reg = lm(cvsf.gar10_14$q_m3_s ~ cvsf.gar10_14[,i])
  print(paste(round(summary(reg)$r.squared*100, digits=1), colnames(cvsf.gar)[i], sep = "   "))
  }  
# Flow can only explain much before fire event. Most 19% of stream concentration for Ca.
# So not that great predictor.
  
  #2014-07-31 to 2017-12-18
  cvsf.gar14_17 <- cvsf.gar[cvsf.gar$time > "2014-07-31" & cvsf.gar$time < "2017-12-31",]
  for(i in 8:13){
  reg = lm(cvsf.gar14_17$q_m3_s ~ cvsf.gar14_17[,i])
  print(paste(round(summary(reg)$r.squared*100, digits=1), colnames(cvsf.gar)[i], sep = "   "))
  }  
# Flow can actually explain stream concentration a bit better after the fire.
# But only as much as 21% for S,N and K (TOC 2.3%).
# So not that great predictor.

# Vallsjöbäcken
  for(i in 8:13){
    reg = lm(cvsf.val$q_m3_s ~ cvsf.val[,i])
    print(paste(round(summary(reg)$r.squared*100, digits=1), colnames(cvsf.val)[i], sep = "   "))
    # Flow can only explain as much as 17% of stream concentration (for K)
    # So not that great predictor.
  }    
# Make Figure S1 plot: conc vs flow ####
# first select columns and standardize response variable
# for Gärsjöbäcken and Vallsjöbäcken
#resp.var <- c("q_m3_s", "SO4_IC.mg.l","Tot.N_TNb.µg.l","Tot._P.µg.l","Ca.mg.l","K.mg.l","TOC.mg.l")
resp.var <- c("q_m3_s", "SO4_IC.mg.l","K.mg.l","TOC.mg.l")
cvsf.gar.2 <- cvsf.gar %>%
  select(resp.var) %>%
  mutate_at(resp.var[-1], scale)
#varnames <- c("SO4", "K", "TOC")
#colnames(cvsf.gar.2)[-1] <- paste("Gärsjöb.", varnames, sep="-")
colnames(cvsf.gar.2)[1] <- "Gärsjöbäcken"

#resp.var <- c("q_m3_s", "SO4_IC.mg.l","Tot.N_TNb.µg.l","Tot._P.µg.l","Ca.mg.l","K.mg.l","TOC.mg.l")
resp.var <- c("q_m3_s", "SO4_IC.mg.l","K.mg.l","TOC.mg.l")
cvsf.val.2 <- cvsf.val %>%
  select(resp.var) %>%
  mutate_at(resp.var[-1], scale)
#varnames <- c("SO4", "K", "TOC")
#colnames(cvsf.val.2)[-1] <- paste("Vallsjöb.", varnames, sep="-")
colnames(cvsf.val.2)[1] <- "Vallsjöbäcken"

# make plots
library(cowplot)
a <- ggplot(cvsf.gar.2, aes(y = SO4_IC.mg.l, x = Gärsjöbäcken)) +
  geom_point() +
  #xlab(expression(Q~"("*m^3~s^-1*")")) +
  #xlab("") +
  ylab(expression("SO"[4]^"2-"~"(z-score)")) +
  annotate("text", x = 0, y = 4, label = "(A)", size=4) +
  labs(title = "Gärsjöbäcken") +
  theme(axis.title.x = element_blank())
b <- ggplot(cvsf.val.2, aes(y = SO4_IC.mg.l, x = Vallsjöbäcken)) +
  geom_point() +
  #xlab(expression(Q~"("*m^3~s^-1*")")) +
  #ylab(expression("SO"[4]^"2-"~"(z-score)")) +
  #labs(y="",x="") +
  annotate("text", x = 0, y = 4, label = "(B)", size=4)+
  labs(title="Vallsjöbäcken")+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())

c <- ggplot(cvsf.gar.2, aes(y = K.mg.l, x = Gärsjöbäcken)) +
  geom_point() +
  #xlab(expression(Q~"("*m^3~s^-1*")")) +
  #xlab("") +
  ylab(expression("K"~"(z-score)")) +
  annotate("text", x = 0, y = 4, label = "(C)", size=4)+
  theme(axis.title.x = element_blank())
d <- ggplot(cvsf.val.2, aes(y = K.mg.l, x = Vallsjöbäcken)) +
  geom_point() +
  #xlab(expression(Q~"("*m^3~s^-1*")")) +
  #ylab(expression("K"~"(z-score)")) +
  #labs(y="",x="") +
  annotate("text", x = 0, y = 4, label = "(D)", size=4)+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())

e <- ggplot(cvsf.gar.2, aes(y = TOC.mg.l, x = Gärsjöbäcken)) +
  geom_point() +
  xlab(expression(Q~"("*m^3~s^-1*")")) +
  ylab(expression("TOC"~"(z-score)")) +
  annotate("text", x = 0, y = 4, label = "(E)", size=4)
f <- ggplot(cvsf.val.2, aes(y = TOC.mg.l, x = Vallsjöbäcken)) +
  geom_point() +
  xlab(expression(Q~"("*m^3~s^-1*")")) +
  #ylab("") +
  #ylab(expression("TOC"~"(z-score)")) +
  annotate("text", x = 0, y = 4, label = "(F)", size=4) +
  theme(axis.title.y = element_blank())

png("figure_S1_concVSflow.png", width = 18, height = 15, units = "cm", res=300)
cowplot::plot_grid(a,b,c,d,e,f, ncol=2,rel_heights=c(0.95,0.8,1))
dev.off()
# END FIGURE
  
# Include prefire data? Only possible for Gärsjön: prefire = TRUE
# run loops for Gärsjön and Vallsjön
gar.exp.res = list()
for (i in 1:length(nut)) { 
  gar.exp.res[[i]] <- mass_exp(chem.str = chem.str, catch = "Gärsjöbäcken", 
                               catch.size = 21.7374*10^6, prefire = TRUE, nut = nut[i])
  names(gar.exp.res)[i] <- nut[i]
}

vall.exp.res = list()
for (i in 1:length(nut)) { 
  vall.exp.res[[i]] <- mass_exp(chem.str = chem.str, catch = "Vallsjöbäcken", 
                           catch.size = 18.3213*10^6, prefire = FALSE, nut[i])
  names(vall.exp.res)[i] <- nut[i]
}

#gar.exp.res <- mass_exp(chem.str = chem.str, catch, catch.size, prefire = TRUE, nut)
#vall.exp.res <- mass_exp(chem.str = chem.str, catch = "Vallsjöbäcken", 
#                        catch.size = 18.3213*10^6, prefire = FALSE, nut)

# mean export in Garsjon 2010-2013 (4 yrs), mg per m2
pre.exp <- lapply(gar.exp.res, function (x) mean(x[1:4,3])*1000 ) 
# post fire export 3 years mg per m2
post.exp <- lapply(gar.exp.res, function (x) x[9:11,3]*1000 ) 

ratios = matrix(NA, nrow=6, ncol=3)
for (i in 1:6) {
  ratios[i,] <- post.exp[[i]] / pre.exp[[i]]
}
round(ratios,1)

# mean post fire export in Vallsjön over 3 years, mg per m2
post.exp <- lapply(vall.exp.res, function (x) x[5:7,3]*1000 ) 


# function to load
mass_exp <- function (chem.str = chem.str, catch, catch.size, prefire, nut) {

# fix conversion factor to pure S
if(nut %in% c("Ca.mg.l", "K.mg.l", "Tot._P.µg.l", "Tot.N_TNb.µg.l", "TOC.mg.l")) {conv=1}
if(nut == "SO4_IC.mg.l") {conv = (32/(32+16*4))}
#units: mg to kg:1e+6 (for S, Ca, K) µg to kg 1e+9 
if(nut %in% c("Ca.mg.l", "K.mg.l", "SO4_IC.mg.l", "TOC.mg.l")) {unit = 1e+6}
if(nut %in% c("Tot._P.µg.l", "Tot.N_TNb.µg.l")) {unit = 1e+9}

cat.dat <- chem.str[chem.str$StNamn == catch,c("time", nut)]
cat.dat <- cat.dat[!(is.na(cat.dat[[nut]])),]

# day of the fire
fire.day <- as.POSIXlt("2014-08-01 CET")

#ggplot(gar[order(gar$time),], aes_string(y=nut, x="time")) +
#  geom_point() +
#  geom_line() +
#  geom_vline(xintercept = as.numeric(fire.day), linetype="dotted") 

cat.dat <- cat.dat[order(cat.dat$time),] # order data
if(catch=="Gärsjöbäcken") {cat.dat <- transform(cat.dat, TotalDays=cumsum(c(1, diff(time)/(60*60*24)))) # get number of days since first record
          } else {cat.dat <- transform(cat.dat, TotalDays=cumsum(c(1, diff(time)))) # get number of days since first record
          # dont have to do: /(60*60*24) here as for Garsjön...
          }


# interpolate between samples
want <- seq(min(cat.dat$TotalDays), max(cat.dat$TotalDays))
interp <- as.data.frame(approx(cat.dat$TotalDays, cat.dat[[nut]], xout=want))

#gar.S <- transform(gar.S, time.num = as.Date(time))
#gar.S <- transform(gar.S, time.num = as.numeric(time)/100)

# other interpolation methods
#mod <- gam(SO4_IC.mg.l~s(TotalDays), data = gar.S)
#mod.lo <- loess(SO4_IC.mg.l~TotalDays, bw=nrd0, data = gar.S)
#mod.lo <- spline(gar.S$TotalDays, gar.S$SO4_IC.mg.l, n=199)

#want <- seq(min(gar.S$TotalDays), max(gar.S$TotalDays))
#p  <- as.data.frame(predict(mod,  newdata = pdat, type = "link"))
#pdat$pred <- as.vector(unlist(p[,]))


# predict trend contributions
#ggplot(gar, aes_string(y=nut, x="TotalDays")) +
#  geom_point() +
#  geom_line() +
#  geom_point(data=interp, aes(y=y, x=x), color="red", size=0.5)

interp$time <-  as.POSIXlt(60*24*60*(interp$x-1), origin = cat.dat[1,1], tz = "UTC") # convert back number to time format

flow.catch <- flow[flow$catch == catch,] # extract flow data for catchment
last.chem.date <- interp$time[NROW(interp$time)] # identify last chemical sampling date
if(flow.catch$time[nrow(flow.catch)] < last.chem.date) {last.chem.date <- flow.catch$time[nrow(flow.catch)]}

if(prefire == TRUE) {
        fir.last <- c(flow.catch[order(flow.catch$time),"time"][1,], 
                      #flow.catch[order(flow.catch$time),"time"][nrow(flow.catch),]) # first/last day
                      flow.catch[order(flow.catch$time),"time"]$time[flow.catch$time==last.chem.date]) # first/last day
        } else { 
          fir.last <- c(interp[order(interp$time),"time"][1], # vallsjöbäcken S cons starts aug 21, 2014 
                        #flow.catch[order(flow.catch$time),"time"]$time[nrow(flow.catch)]) # first/last day
                        flow.catch[order(flow.catch$time),"time"]$time[flow.catch$time==last.chem.date]) # first/last day
        }
interp.sub <- interp[which(interp$time == fir.last[1]):which(interp$time == fir.last[2]),] # concentration per day for the period
flow.gar.sub  <- flow.catch[which(flow.catch$time == fir.last[1]):which(flow.catch$time == fir.last[2]),] # match period with flow data
mass.calc.df <- cbind(interp.sub[order(interp.sub$time),], q_m3_s=flow.gar.sub[order(flow.gar.sub$time),"q_m3_s"])

# caclculate exported per day

#conv = (32/(32+16*4)) # for other copunds this is not 1: SO4_IC.mg.l = (32/(32+16*4)
#unit = 1e+6 #mg to kg:1e+6 (for S, Ca, K), µg to kg: 1e+9 
mass.calc.df$exp_day_kg <- ((mass.calc.df$q_m3_s*1000*60*60*24 * mass.calc.df$y)/unit) * conv

mass.calc.df$exp_day_kg <- ((mass.calc.df$q_m3_s*1000*60*60*24 * mass.calc.df$y)/unit) * conv
  #ggplot(mass.calc.df, aes(y=exp_day_kg, x=time)) +
  #  geom_line()

# annual exports, area = 21.7374 km2
mass.calc.df$year <- as.numeric(substr(mass.calc.df$time,1,4))
year.exp <- aggregate(exp_day_kg ~ year, data = mass.calc.df, sum) # kg per year

# define years post-fire. Not exactly the same between the catchments due to lack of prefire data for Vallsjon
if(catch == "Gärsjöbäcken") { yrs = c("2014-08-01", "2015-07-31","2015-08-01", "2016-07-31", "2016-08-01", "2017-07-31")
                           } else {
                             yrs = c("2014-08-21", "2015-08-20","2015-08-21", "2016-08-20", "2016-08-21", "2017-08-20")
                           }
burn.yr1 <- c(which(mass.calc.df$time == as.POSIXlt(yrs[1], tz = "UTC")), 
             which(mass.calc.df$time == as.POSIXlt(yrs[2], tz = "UTC")))
burn.yr2 <- c(which(mass.calc.df$time == as.POSIXlt(yrs[3], tz = "UTC")), 
              which(mass.calc.df$time == as.POSIXlt(yrs[4], tz = "UTC")))
burn.yr3 <- c(which(mass.calc.df$time == as.POSIXlt(yrs[5], tz = "UTC")), 
              which(mass.calc.df$time == as.POSIXlt(yrs[6], tz = "UTC")))
year.exp[nrow(year.exp)+1,2] <-sum(mass.calc.df[burn.yr1[1]:burn.yr1[2],"exp_day_kg"]) # burn year
year.exp[nrow(year.exp),1] <- paste(yrs[1]," to ", yrs[2])
year.exp[nrow(year.exp)+1,2] <-sum(mass.calc.df[burn.yr2[1]:burn.yr2[2],"exp_day_kg"]) # burn year
year.exp[nrow(year.exp),1] <- paste(yrs[3]," to ", yrs[4])
year.exp[nrow(year.exp)+1,2] <-sum(mass.calc.df[burn.yr3[1]:burn.yr3[2],"exp_day_kg"]) # burn year
year.exp[nrow(year.exp),1] <- paste(yrs[5]," to ", yrs[6])

# calculate gram per m-2
year.exp$g_m2 <- (year.exp$exp_day_kg*1000)/(catch.size)
#year.exp$g_m2[nrow(year.exp)] / mean(year.exp$g_m2[1:4])

# test if numbers make sense
  #mass.calc.df$q_m3_day <- mass.calc.df$q_m3_s*60*60*24 
  #year.q <- aggregate(q_m3_day ~ year, data = mass.calc.df, sum) # 
  #year.q$sum.m2 <- year.q$q_m3_day/(21.7374*10^6)
  #aggregate(y ~ year, data = mass.calc.df, mean)
#return(year.exp)
 return(year.exp)
}


