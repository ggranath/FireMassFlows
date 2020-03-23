############################################################################################
# EC-flux data analyses
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

# load data
dat <- read.csv("ec_flux_2014fire.csv")

# make long format
dat <- reshape(dat, direction='long', 
        varying=c('NEE.south', 'R.south', 'GPP.south', 'NEE.north', 'R.north', 'GPP.north'), 
        timevar='site', 
        times=c('south', 'north'),
        idvar='date', new.row.names = NULL)
# format date
dat$date <- as.POSIXct(as.character(dat$date))

# summarise per month
dat$mon = strftime(dat$date, "%Y-%m")
dat.months = aggregate(NEE ~ site + mon, dat,FUN = sum)
dat.months$date <- as.POSIXct(as.character(paste(dat.months$mon,"15", sep="-")), "%Y-%m-%d")

# transform NEE CO2 to CO2-C
dat.months$NEE.C <- dat.months$NEE/3.67


# data up to dec 2018
dat.months.sub <- dat.months[dat.months$date > as.POSIXct("2014-08-01 CEST") & 
             dat.months$date < as.POSIXct("2018-12-31 CEST"),]

# NEE per month figure 
f.month <- 
  ggplot(data = dat.months.sub, aes(x = date, y = NEE.C, group = site)) + 
  geom_path( size = 0.5, na.rm=FALSE, linetype=2) + 
  geom_path(data=dat.months.sub[dat.months.sub$date > as.POSIXct("2015-05-01 CEST"),], 
            size = 0.5, na.rm=FALSE, linetype=1) + 
  geom_point(size = 2.5, aes(shape=site)) +
  scale_color_discrete(name="Site") +
  ylab(as.expression(bquote('NEE of'~CO[2]*'-C (g' ~m^-2*~month^-1*')'))) + #g CO2-C /m2/month.
  xlab("") +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_shape_manual(name="Site", values=c(1, 19),
                     labels = c("North", "South")) + 
  #scale_y_continuous(breaks=seq(0,160, 20), limits= c(-20,160)) +
  scale_y_continuous(breaks=seq(-5,50, 5), limits= c(-5,50)) +
  scale_x_datetime(limits = c(as.POSIXct("2014-05-01 CEST"), as.POSIXct("2019-07-31 CEST")), 
                   date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position=c(0.55,0.9),
        plot.title=element_text(hjust=0, face=1)) +
  draw_plot_label("(a)", x= as.POSIXct("2014-08-31 CEST"), y = 50, 
                  hjust = 0, vjust = 0, size=18 )

dat.months.sub = transform(dat.months.sub, NEEsum = ave(NEE.C, site, FUN = cumsum))

# Cumulative NEE figure 
f.month.sum <-
ggplot(data = dat.months.sub, aes(x = date, y = NEEsum, group = site)) + 
  geom_path( size = 0.5, na.rm=FALSE, linetype=2) + 
  geom_path(data=dat.months.sub[dat.months.sub$date > as.POSIXct("2015-05-01 CEST"),], 
            size = 0.5, na.rm=FALSE, linetype=1) + 
  geom_point(size = 2.5, aes(shape=site)) +
  scale_color_discrete(name="Site") +
  ylab(as.expression(bquote('Cumulative NEE of'~CO[2]*'-C (g' ~m^-2*')'))) + #g CO2-C /m2.
  xlab("") +
  geom_hline(yintercept = 0, linetype="dotted") +
  #labs(x="", title = paste("(",labels[i],")", sep="")) +
  #geom_vline(xintercept = as.numeric(fire.day), linetype="dotted") +
  scale_shape_manual(name="Site", values=c(1, 19),
                     labels = c("North", "South")) + 
  #scale_y_continuous(breaks=seq(0,3000, 250), limits= c(0,3000)) +
  scale_y_continuous(breaks=seq(0,750, 100), limits= c(0,750)) +
  scale_x_datetime(limits = c(as.POSIXct("2014-05-01 CEST"), as.POSIXct("2019-07-31 CEST")), 
      date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position=c(0.55,0.9),
        plot.title=element_text(hjust=0, face=1)) +
  draw_plot_label("(b)", x= as.POSIXct("2014-08-31 CEST"), y = 725, 
                  hjust = 0, vjust = 0, size=18 )

# BEFORE SAVING FIGURE!
# Go to lai_modis.R and create lai panel (lai.fig) 
neefig <- plot_grid(f.month, f.month.sum, lai.fig, ncol=1,  align = "v")
#save_plot("fig2_nee_c_rev.png", neefig, base_height = NULL, base_asp = 0.55, base_width = 6)
save_plot("fig2_nee_c_rev.pdf", neefig, base_height = NULL, base_asp = 0.55, base_width = 6)

# Add LAI data


# Amount C 
# First year August-August
c.first.yr <- dat.months.sub[dat.months.sub$date < as.POSIXct("2015-08-01 CEST"),]
mean(aggregate(NEE.C ~ site, c.first.yr, sum)$NEE.C)
# Two years August-August
c.second.yr <- dat.months.sub[
                                dat.months.sub$date < as.POSIXct("2016-08-01 CEST"),]
aggregate(NEE.C ~ site, c.second.yr, sum)

# Three years August-August
c.three.yr <- dat.months.sub[dat.months.sub$date < as.POSIXct("2017-08-01 CEST"),]
mean(aggregate(NEE.C ~ site, c.three.yr, sum)$NEE.C)
aggregate(NEE.C ~ site, c.three.yr, sum)
