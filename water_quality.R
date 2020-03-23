############################################################################################
# Analyses of water quality
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################

# Load needed packages
library(ggplot2) # plotting
library(gridExtra) # plotting

# get data
source("load_stream_lake_chem_data.R")

# MS fig 3 - change in conc of major elements and nutrients ####
library(dplyr)

#colnames(chem.str)[c(35, 37, 39, 43, 54, 60)] <- c("NH4_N_µg_l", "NO2_NO3_N_µg_l", "PO4_P_µg_l", "SO4_IC_mg_l", "Ca_mg_l", "K_mg_l")
chem.str <- chem.str %>% mutate(NH4_N_µmol_l = NH4_N.µg.l/14.0067,
                                NO2_NO3_N_µmol_l = NO2.NO3_N.µg.l/14.0067,
                                PO4_P_µmol_l = PO4_P.µg.l/30.973,
                                SO4_IC_µmol_l = 1000*(SO4_IC.mg.l/(96.06)),
                                Ca_µmol_l = 1000*(Ca.mg.l/40.078),
                                K_µmol_l = 1000*(K.mg.l/39.0983),
                                Tot_N_µg_l = Tot.N_TNb.µg.l-NH4_N.µg.l-NO2.NO3_N.µg.l,
                                TOC_mg_l = TOC.mg.l)

meta.list <- c("StNamn", "time")
var.list <- c("NH4_N_µmol_l", "NO2_NO3_N_µmol_l", "PO4_P_µmol_l", "Tot_N_µg_l","SO4_IC_µmol_l", 
              "Ca_µmol_l", "K_µmol_l", "TOC_mg_l")
chem.str.nut <- chem.str[ chem.str$year >2013, c(meta.list,var.list)]
chem.str.nut$time <- as.POSIXct(chem.str.nut$time)

fire.day <- as.POSIXlt("2014-08-01 CET")
chem.str.nut <- chem.str.nut[order(chem.str.nut$StNamn, chem.str.nut$time),]
var.list <- var.list[1:8]
#ylabs <- sapply(strsplit(var.list, "_"), function (x) x[1])

ylabs <- c(
as.expression(bquote('NH'[4]^'  +'*' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('NO'[2]^'  -'*' + '~'NO'[3]^'  -'~' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('PO'[4]^'  3-'*' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('TON'*' ('*mu*'g' ~l^-1*')')),
as.expression(bquote('SO'[4]^'  2-'*' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('Ca'^'2+'*' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('K'^'+'*' ('*mu*'mol' ~l^-1*')')),
as.expression(bquote('TOC'*' (mg' ~l^-1*')'))
)
library(cowplot)
theme_set(theme_cowplot())
labels <- letters[1:8]
pp <- list()
ylims <- c(300, 100, 2, 2000, 400, 750, 200, 60)
for (i in 1:length(var.list)){
  if(i==1 | i==5) {leg.pos=c(0.4, 0.8)}
  else {leg.pos="none"}
  yl <- ylabs[i]
  #yl <- as.expression(bquote(.(yl)[4]^'2+'*' ('*mu*'mol' ~l^-1*')'))
  
  pp[[i]] <- ggplot(data = chem.str.nut[!(is.na(chem.str.nut[[var.list[i]]])), ], 
                    aes_string(x = "time", y = var.list[i], color= "StNamn")) + 
    geom_path( size = 0.5, na.rm=FALSE) + 
    geom_point(size = 2.5, shape=1) +
    scale_color_discrete(name="Stream") +
    ylim(c(0, ylims[i])) +
    ylab(yl) +
    labs(x="", title = paste("(",labels[i],")", sep="")) +
    geom_vline(xintercept = as.numeric(fire.day), linetype="dotted") +
    theme(legend.position=leg.pos,
          plot.title=element_text(hjust=0, face=1)) 
}
nut.plot <- marrangeGrob(c(pp[1],pp[2],pp[3], pp[4],pp[5],pp[6],pp[7],pp[8]) , 
                         nrow=4, ncol=2, top=" ")
#ggsave("Fig3_nutrients_pub.png", nut.plot, width=14, height=18, units = "in" )
save_plot("Fig3_nutrients_pub_rev.png", nut.plot, base_height=10, units = "in", 
          base_asp= 1 # make room for figure legend
          )
save_plot("Fig3_nutrients_pub_rev.pdf", nut.plot, base_height=10, units = "in", 
          base_asp= 1 # make room for figure legend
)

# MS fig 4 - long-term change in conc of major elements and nutrients: Garsjon/Marrsjon ####
library(dplyr)
chem.str.lt <- chem.str[chem.str$StNamn %in% "Gärsjöbäcken",]
chem.str.lt <- droplevels(chem.str.lt)
colnames(chem.str.lt) <- make.names(colnames(chem.str.lt))
colnames(chem.lake) <- make.names(colnames(chem.lake))

# merge streams and lake data
# pick variables
variab <- c("StNamn", "time", "year", "NH4_N.µg.l", "NO2.NO3_N.µg.l", "PO4_P.µg.l", "SO4_IC.mg.l", 
            "Ca.mg.l", "K.mg.l","pH", "Alk..Acid.mekv.l", 
            "TOC.mg.l", "Tot.N_TNb.µg.l")
str.lake.lt <- rbind(chem.str.lt[,variab],chem.lake[,variab])

#colnames(chem.str)[c(35, 37, 39, 43, 54, 60)] <- c("NH4_N_µg_l", "NO2_NO3_N_µg_l", "PO4_P_µg_l", "SO4_IC_mg_l", "Ca_mg_l", "K_mg_l")
str.lake.lt <- str.lake.lt  %>% mutate(NH4_N_µmol_l = NH4_N.µg.l/14.0067,
                                NO2.NO3_N_µmol_l = NO2.NO3_N.µg.l/14.0067,
                                PO4_P_µmol_l = PO4_P.µg.l/30.973,
                                SO4_IC_µmol_l = 1000*(SO4_IC.mg.l/(96.06)),
                                Ca_µmol_l = 1000*(Ca.mg.l/40.078),
                                K_µmol_l = 1000*(K.mg.l/39.0983),
                                Alk.Acid_mekv.l = Alk..Acid.mekv.l,
                                Tot_N_µg_l = Tot.N_TNb.µg.l-NH4_N.µg.l-NO2.NO3_N.µg.l,
                                TOC_mg_l = TOC.mg.l)

meta.list <- c("StNamn", "time")
var.list <- c("NH4_N_µmol_l", "NO2.NO3_N_µmol_l", "PO4_P_µmol_l", "Tot_N_µg_l", "SO4_IC_µmol_l", 
              "Ca_µmol_l", "K_µmol_l", "TOC_mg_l")
str.lake.lt.sub  <- str.lake.lt [ str.lake.lt$year >2004, c(meta.list,var.list)]
str.lake.lt.sub$time <- as.POSIXct(str.lake.lt.sub$time)

fire.day <- as.POSIXlt("2014-08-01 CET")
str.lake.lt.sub <- str.lake.lt.sub[order(str.lake.lt.sub$StNamn, str.lake.lt.sub$time),]
var.list <- var.list[1:8]
#ylabs <- sapply(strsplit(var.list, "_"), function (x) x[1])

ylabs <- c(
  as.expression(bquote('NH'[4]^'  +'*' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('NO'[2]^'  -'*' + '~'NO'[3]^'  -'~' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('PO'[4]^'  3-'*' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('TON'*' ('*mu*'g' ~l^-1*')')),
  as.expression(bquote('SO'[4]^'  2-'*' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('Ca'^'2+'*' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('K'^'+'*' ('*mu*'mol' ~l^-1*')')),
  as.expression(bquote('TOC'*' (mg' ~l^-1*')'))
)
library(cowplot)
theme_set(theme_cowplot())
labels <- letters[1:8]

pp <- list()
ylims <- c(250, 40, 1.8, 1500, 250, 350, 200, 60)
for (i in 1:length(var.list)){
  if(i==1 | i==5) {leg.pos=c(0.1, 0.8)}
  else {leg.pos="none"}
  yl <- ylabs[i]
  #yl <- as.expression(bquote(.(yl)*~mu*'mol' ~l^-1))
  pp[[i]] <- ggplot(data = str.lake.lt.sub[!(is.na(str.lake.lt.sub[[var.list[i]]])), ], 
                    aes_string(x = "time", y = var.list[i], color= "StNamn")) + 
    geom_path( size = 0.7, na.rm=FALSE) + 
    geom_point(size = 2.5, shape=1) +
    scale_color_discrete(name="Site", labels = c("Stream: Gärsjöbäcken", "Lake: Märrsjön")) +
    ylim(c(0, ylims[i])) +
    ylab(yl) +
    labs(x="", title = paste("(",labels[i],")", sep="")) +
    geom_vline(xintercept = as.numeric(fire.day), linetype="dotted") +
    theme(legend.position=leg.pos,
          plot.title=element_text(hjust=0, face=1)) +
    #scale_x_datetime(date_breaks = "4 year", date_minor_breaks = "1 year", date_labels = "%Y")
    scale_x_datetime(breaks = as.POSIXct(c("2005-01-01 CET", "2006-01-01 CET", "2007-01-01 CET",
                                           "2008-01-01 CET","2009-01-01 CET","2010-01-01 CET", 
                                           "2011-01-01 CET","2012-01-01 CET", "2013-01-01 CET", 
                                           "2014-01-01 CET","2015-01-01 CET","2016-01-01 CET",
                                           "2017-01-01 CET", "2018-01-01 CET")), 
                     labels = c("","2006","","","","2010","","","","2014",
                                             "","","","2018"))
    }
lt.plot <- marrangeGrob(c(pp[1],pp[2],pp[3],pp[4],pp[5],pp[6],pp[7],pp[8]) , 
                        nrow=4, ncol=2, top = " ")
#ggsave("Fig4_longterm_nutrients.png", lt.plot, width=9, height=11, units = "in" )
save_plot("Fig4_longterm_nutrients_pub_rev.png", lt.plot, base_height=10, units = "in", 
          base_asp= 1 ## make room for figure legend
          )
save_plot("Fig4_longterm_nutrients_pub_rev.pdf", lt.plot, base_height=10, units = "in", 
          base_asp= 1 ## make room for figure legend
          )


# Supp Mtrl Figure S2 Long-term trend pH-Alk-TOC####
library(dplyr)
chem.str.lt <- chem.str[chem.str$StNamn %in% "Gärsjöbäcken",]
chem.str.lt <- droplevels(chem.str.lt)
colnames(chem.str.lt) <- make.names(colnames(chem.str.lt))
colnames(chem.lake) <- make.names(colnames(chem.lake))

# merge streams and lake data
# pick variables
variab <- c("StNamn", "time", "year", "NH4_N.µg.l", "NO2.NO3_N.µg.l", "PO4_P.µg.l", "SO4_IC.mg.l", "Ca.mg.l", "K.mg.l",
            "pH", "Alk..Acid.mekv.l", "TOC.mg.l")
str.lake.lt <- rbind(chem.str.lt[,variab],chem.lake[,variab])

#colnames(chem.str)[c(35, 37, 39, 43, 54, 60)] <- c("NH4_N_µg_l", "NO2_NO3_N_µg_l", "PO4_P_µg_l", "SO4_IC_mg_l", "Ca_mg_l", "K_mg_l")
str.lake.lt <- str.lake.lt  %>% mutate(NH4_N_µmol_l = NH4_N.µg.l/14.0067,
                                       NO2.NO3_N_µmol_l = NO2.NO3_N.µg.l/14.0067,
                                       PO4_P_µmol_l = PO4_P.µg.l/30.973,
                                       SO4_IC_µmol_l = 1000*(SO4_IC.mg.l/(96.06)),
                                       Ca_µmol_l = 1000*(Ca.mg.l/40.078),
                                       K_µmol_l = 1000*(K.mg.l/39.0983),
                                       Alk.Acid_mekv.l = Alk..Acid.mekv.l,
                                       TOC_mg_l= TOC.mg.l)

meta.list <- c("StNamn", "time")
var.list <- c("Alk.Acid_mekv.l", "pH", "TOC_mg.l")
str.lake.lt.sub  <- str.lake.lt[ str.lake.lt$year >2004, c(meta.list,var.list)]

fire.day <- as.POSIXct("2014-08-01 CET")
str.lake.lt.sub$time <- as.POSIXct(str.lake.lt.sub$time)
str.lake.lt.sub <- str.lake.lt.sub[order(str.lake.lt.sub$StNamn, str.lake.lt.sub$time),]
#var.list <- var.list[1:3]
#ylabs <- sapply(strsplit(var.list, "_"), function (x) x[1])
ylabs <-var.list
ylabs <- sapply(strsplit(var.list, "_"), function (x) x[1])
ylabs[1] <- "Alkalinity"
ylabs <- c(as.expression(bquote(.(ylabs[1])*~'(mequiv' ~l^-1*")")), ylabs[2],   
           as.expression(bquote(.(ylabs[3])*~'(mg' ~l^-1*")")))

pp <- list()
ylims <- c(0.4, 8, 60)
ylims <- data.frame(min=c(0, 3, 0), max=c(0.4, 8, 60))
lab = c("a","b","c")
for (i in 1:length(var.list)){
  yl <- ylabs[i]
  #yl <- as.expression(bquote(.(yl)*~mu*'mol' ~l^-1))
  pp[[i]] <- ggplot(data = str.lake.lt.sub[!(is.na(str.lake.lt.sub[[var.list[i]]])), ], 
                    aes_string(x = "time", y = var.list[i], color= "StNamn")) + 
    geom_path( size = 0.7, na.rm=FALSE) + 
    geom_point(size = 1.5) +
    scale_color_discrete(name="Site", labels = c("Stream: Gärsjöbäcken", "Lake: Märrsjön")) +
    ylim(unlist(ylims[i,])) +
    ylab(yl) +
    annotate(geom = 'text', label = lab[i], x = as.POSIXct(c("2005-05-01 CET")), 
             y = Inf, hjust = 1, vjust = 2, size=7) +
    geom_vline(xintercept = as.numeric(fire.day), linetype="dotted")
    if(i != 1) {pp[[i]] <- pp[[i]] + theme(legend.position="none")} else {
      pp[[i]] <- pp[[i]] +  theme(legend.position=c(0.35, 0.8)) 
    }
}
lt.plot <- marrangeGrob(c(pp[1],pp[3],pp[2]) , nrow=2, ncol=2)
ggsave("SM_FS1_longterm_nutrients.png", lt.plot, width=9, height=7, units = "in" )


# Supp Mtrl Figure S3 catchment pH-RCOO-TOC-Alc####

#colnames(chem.str)[c(35, 37, 39, 43, 54, 60)] <- c("NH4_N_µg_l", "NO2_NO3_N_µg_l", "PO4_P_µg_l", "SO4_IC_mg_l", "Ca_mg_l", "K_mg_l")
chem.str <- chem.str %>% mutate(RCOO = RCOO__1,
                                Alk.Acid_mekv.l = Alk..Acid.mekv.l,
                                TOC_mg_l= TOC.mg.l)

meta.list <- c("StNamn", "time")
var.list <- c("pH", "RCOO", "Alk.Acid_mekv.l", "TOC_mg.l")
chem.str.nut <- chem.str[ chem.str$year >2013, c(meta.list,var.list)]

fire.day <- as.POSIXlt("2014-08-01 CET")
chem.str.nut$time <- as.POSIXct(chem.str.nut$time)
chem.str.nut <- chem.str.nut[order(chem.str.nut$StNamn, chem.str.nut$time),]
var.list <- var.list[1:4]
ylabs <- sapply(strsplit(var.list, "_"), function (x) x[1])
ylabs[3] <- "Alkalinity"
ylabs <- c(ylabs[1], as.expression(bquote(.(ylabs[2])^-1)),  
        as.expression(bquote(.(ylabs[3])*~'(mequiv' ~l^-1*")")), as.expression(bquote(.(ylabs[4])*~'(mg' ~l^-1*")")))

pp <- list()
ylims <- data.frame(min=c(3, 0, 0, 0), max=c(10, 0.6, 0.6, 70))
lab = c("a","b","c","d")
for (i in 1:length(var.list)){
  yl <- ylabs[i]
  pp[[i]] <- ggplot(data = chem.str.nut[!(is.na(chem.str.nut[[var.list[i]]])), ], 
                    aes_string(x = "time", y = var.list[i], color= "StNamn")) + 
    geom_path( size = 0.7, na.rm=FALSE) + 
    geom_point(size = 2) +
    scale_color_discrete(name="Stream") +
    ylim(unlist(ylims[i,])) +
    xlim(as.POSIXct(c("2014-01-01 CET", "2018-01-01 CET"))) +
    ylab(yl) +
    geom_vline(xintercept = as.numeric(fire.day), linetype="dotted") +
    theme(legend.position=c(0.8, 0.8)) +
  annotate(geom = 'text', label = lab[i], x = as.POSIXct(c("2014-05-01 CET")), 
           y = Inf, hjust = 1, vjust = 2, size=10)
}
nut.plot.sm <- marrangeGrob(c(pp[1],pp[3],pp[2],pp[4]) , nrow=2, ncol=2)
ggsave("SM_FS2_nutrients.png", nut.plot.sm, width=10, height=10, units = "in" )
