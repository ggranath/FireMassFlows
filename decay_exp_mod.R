############################################################################################
# Calculate double exponential decay curves
#
# "The impact of wildfire on biogeochemical fluxes and water quality on boreal catchments" 
# Granath et al.
# 
# Contact: gustaf.granath@gmail.com
############################################################################################


# packages
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(brms)

# Figure 5 ####
#__Ca ####
ca <- readxl::read_xlsx("decay_curves.xlsx", sheet="Ca")
colnames(ca)[c(3,10)] <- c("days", "CA")

decay.ca=nls(CA ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=ca, 
              start=list(a=68, b=8, c=163, d=131, e=126))
summary(decay.ca)
newdat <- data.frame(days = 0:ca$days[sum(!(is.na(ca$CA)))])
newdat$p.sum <- predict(decay.ca, newdata = newdat)
newdat$p.fast <- coef(decay.ca)[4]*0.5^(newdat$days/coef(decay.ca)[2])
newdat$p.slow <- coef(decay.ca)[5]*0.5^(newdat$days/coef(decay.ca)[3])

p.ca <- ggplot(ca, aes(x = days, y = CA)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum, linetype = "Modelled", colour="Modelled")) +
  geom_line(data=newdat, aes(x = days, y = p.fast, linetype = "Fast decay pool", colour="Fast decay pool")) +
  geom_line(data=newdat, aes(x = days, y = p.slow, linetype = "Slow decay pool", colour="Slow decay pool")) +
  geom_point(aes(fill="Observed"), size=2,shape = 21, colour = "black", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,400, by=50),limits = c(0,400), expand = c(0, 0)) + 
  draw_plot_label("(a) Calcium", x= 300, y = 365, fontface = "plain", 
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE) +
  scale_fill_manual(name = "", values = c("Observed" = "red")) +
  scale_colour_manual("", breaks = c("Modelled", "Fast decay pool", "Slow decay pool"),
                      values=c("Modelled" = 1,"Fast decay pool" = "gray","Slow decay pool" = 1)) +
  scale_linetype_manual("",  breaks = c("Modelled", "Fast decay pool", "Slow decay pool")
                        ,values=c("Modelled" = 1,"Fast decay pool" = 1,"Slow decay pool" = 2)) +
  theme(legend.position = c(0.50, 0.8),
        legend.key.width = unit(3, "line"),
        legend.key.height = unit(1, "line"),
        legend.text = element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))

#__K####
k <- readxl::read_xlsx("~/projects/brand_vm/FireMassFlows/decay_curves.xlsx", sheet="K")
colnames(k)[c(4,11)] <- c("days", "K")

decay.k=nls(K ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=k, 
              start=list(a=15, b=9, c=161, d=35, e=89), control = list(maxiter = 150))
summary(decay.k)
newdat <- data.frame(days = 0:k$days[sum(!(is.na(k$K)))])
newdat$p.sum <- predict(decay.k, newdata = newdat)
newdat$p.fast <- coef(decay.k)[4]*0.5^(newdat$days/coef(decay.k)[2])
newdat$p.slow <- coef(decay.k)[5]*0.5^(newdat$days/coef(decay.k)[3])

library(cowplot)
theme_set(theme_cowplot())

p.k <- ggplot(k, aes(x = days, y = K)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum)) +
  geom_line(data=newdat, aes(x = days, y = p.fast), colour = "gray") +
  geom_line(data=newdat, aes(x = days, y = p.slow), linetype = "dashed") +
  geom_point(size=2,shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,160, by=20),limits = c(0,160), expand = c(0, 0)) + 
  draw_plot_label("(b) Potassium", x= 300, y = 145, fontface = "plain",  
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE) +
  theme(plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))

#__SO4####
so4 <- readxl::read_xlsx("~/projects/brand_vm/FireMassFlows/decay_curves.xlsx", sheet="SO4")
colnames(so4)[c(3,10)] <- c("days", "SO4")

decay.so4=nls(SO4 ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=so4, 
             start=list(a=30, b=6, c=111, d=216, e=299))
summary(decay.so4)
newdat <- data.frame(days = 0:so4$days[sum(!(is.na(so4$SO4)))])
newdat$p.sum <- predict(decay.so4, newdata = newdat)
newdat$p.fast <- coef(decay.so4)[4]*0.5^(newdat$days/coef(decay.so4)[2])
newdat$p.slow <- coef(decay.so4)[5]*0.5^(newdat$days/coef(decay.so4)[3])

library(cowplot)
theme_set(theme_cowplot())

p.so4 <- ggplot(so4, aes(x = days, y = SO4)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum)) +
  geom_line(data=newdat, aes(x = days, y = p.fast), colour = "gray") +
  geom_line(data=newdat, aes(x = days, y = p.slow), linetype = "dashed") +
  geom_point(size=2,shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,600, by=100),limits = c(0,600), expand = c(0, 0)) + 
  draw_plot_label("(c) Sulphate", x= 300, y = 555, fontface = "plain",  
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE) +
  theme(plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))


#__Cl####
cl <- readxl::read_xlsx("~/projects/brand_vm/FireMassFlows/decay_curves.xlsx", sheet="Cl")
colnames(cl)[c(3,10)] <- c("days", "CL")

decay.cl=nls(CL ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=cl[,], 
              start=list(a=46, b=4.2, c=181, d=23, e=119))
summary(decay.cl)
newdat <- data.frame(days = 0:cl$days[sum(!(is.na(cl$CL)))])
newdat$p.sum <- predict(decay.cl, newdata = newdat)
newdat$p.fast <- coef(decay.cl)[4]*0.5^(newdat$days/coef(decay.cl)[2])
newdat$p.slow <- coef(decay.cl)[5]*0.5^(newdat$days/coef(decay.cl)[3])

library(cowplot)
theme_set(theme_cowplot())

p.cl <- ggplot(cl, aes(x = days, y = CL)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum)) +
  geom_line(data=newdat, aes(x = days, y = p.fast), colour = "gray") +
  geom_line(data=newdat, aes(x = days, y = p.slow), linetype = "dashed") +
  geom_point(size=2,shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,180, by=20),limits = c(0,180), expand = c(0, 0)) + 
  draw_plot_label("(d) Chloride", x= 300, y = 163, fontface = "plain",  
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE)  +
  theme(plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))


#__NH4####
nh4 <- readxl::read_xlsx("~/projects/brand_vm/FireMassFlows/decay_curves.xlsx", sheet="NH4")
colnames(nh4)[3:4] <- c("days", "NH4")

decay.nh4=nls(NH4 ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=nh4, 
          start=list(a=22, b=4, c=190, d=1282, e=1824))
summary(decay.nh4)
newdat <- data.frame(days = 0:nh4$days[sum(!(is.na(nh4$NH4)))])
newdat$p.sum <- predict(decay.nh4, newdata = newdat)
newdat$p.fast <- coef(decay.nh4)[4]*0.5^(newdat$days/coef(decay.nh4)[2])
newdat$p.slow <- coef(decay.nh4)[5]*0.5^(newdat$days/coef(decay.nh4)[3])

library(cowplot)
theme_set(theme_cowplot())

p.nh4 <- ggplot(nh4, aes(x = days, y = NH4)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum)) +
  geom_line(data=newdat, aes(x = days, y = p.fast), colour = "gray") +
  geom_line(data=newdat, aes(x = days, y = p.slow), linetype = "dashed") +
  geom_point(size=2,shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,3500, by=500),limits = c(0,3500), expand = c(0, 0)) + 
  draw_plot_label("(e) Ammonium", x= 300, y = 3200,  fontface = "plain", 
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE)  +
  theme(plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))
  

#__Total nitrogen####
tn <- readxl::read_xlsx("~/projects/brand_vm/FireMassFlows/decay_curves.xlsx", sheet="TN")
colnames(tn)[3:4] <- c("days", "TN")

decay.tn=nls(TN ~ a + d*0.5^(days/b) + e*0.5^(days/c), data=tn, 
             start=list(a=606, b=6, c=190, d=1724, e=1478))
summary(decay.tn)
newdat <- data.frame(days = 0:tn$days[sum(!(is.na(tn$TN)))])
newdat$p.sum <- predict(decay.tn, newdata = newdat)
newdat$p.fast <- coef(decay.tn)[4]*0.5^(newdat$days/coef(decay.tn)[2])
newdat$p.slow <- coef(decay.tn)[5]*0.5^(newdat$days/coef(decay.tn)[3])

library(cowplot)
theme_set(theme_cowplot())

p.tn <- ggplot(tn, aes(x = days, y = TN)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum)) +
  geom_line(data=newdat, aes(x = days, y = p.fast), colour = "gray") +
  geom_line(data=newdat, aes(x = days, y = p.slow), linetype = "dashed") +
  geom_point(size=2,shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.75) +
  scale_x_continuous(name = "Time since peak concentration (days)", 
                     breaks = c(0,250,500,750,1000,1250), 
                     limits = c(0,1250), expand = c(0, 0)) + 
  scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
                     breaks = seq(0,4500, by=500),limits = c(0,4500), expand = c(0, 0)) + 
  draw_plot_label("(f) Total nitrogen", x= 300, y = 4100, fontface = "plain", 
                  hjust = 0, vjust = 0, size=16 ) +
  coord_cartesian(clip = 'off') +
  panel_border(color = "black", size = 0.5, linetype = 1,
               remove = FALSE)  +
  theme(plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))

decayfig <- plot_grid(p.ca,p.k, p.so4, p.cl, p.nh4, p.tn, ncol=2,  align = "v")
#save_plot("fig2_nee_c_rev.png", neefig, base_height = NULL, base_asp = 0.55, base_width = 6)
save_plot("figure5_decay_rev.pdf", decayfig, base_height = NULL, base_asp = 1, base_width = 10)
save_plot("figure5_decay_rev.eps", decayfig, base_height = NULL, base_asp = 1, base_width = 10)


# Table 3 - decay curves in catchments of major elements and nutrients ####
library(dplyr)
library(brms)

# get data
source("load_stream_lake_chem_data.R")

chem.str <- chem.str %>% mutate(NH4_N_µmol_l = NH4_N.µg.l/14.0067,
                                SO4_IC_µmol_l = 1000*(SO4_IC.mg.l/(96.06)),
                                Ca_µmol_l = 1000*(Ca.mg.l/40.078),
                                Mg_µmol_l = 1000*(Mg.mg.l/24.305),
                                K_µmol_l = 1000*(K.mg.l/39.0983),
                                Cl_µmol_l = 1000*(Cl.mg.l/35.453),
                                Tot_N_µmol_l = Tot.N_TNb.µg.l/14.0067)

meta.list <- c("StNamn", "time")
var.list <- c("NH4_N_µmol_l", "Tot_N_µmol_l","SO4_IC_µmol_l", 
              "Ca_µmol_l", "Mg_µmol_l", "K_µmol_l", "Cl_µmol_l")
chem.str.dec <- chem.str[ chem.str$year >2013, c(meta.list,var.list)]
chem.str.dec$time <- as.POSIXct(chem.str.dec$time)
chem.str.dec <- chem.str.dec[order(chem.str.dec$StNamn, chem.str.dec$time),]

# Funtion to extract from models
tab.row <- function(mod, site = sites[k]) {
  if(site=="Ladängsbäcken"|site =="Vallsjöbäcken"|site =="Gärsjöbäcken"|site =="Myckelmossbäcken") {
    end = nchar(formula(mod)[1]$formula[2])
    element = substr(formula(mod)[1]$formula[2], start = 1, stop =end)
    ele.row = data.frame(site =  site, element = element, 
                         bas.conc = fixef(mod)[1], bas.per = fixef(mod)[1]/ sum(fixef(mod)[c(1,4,5)]),
                         fast.conc = fixef(mod)[4], fast.per = fixef(mod)[4]/ sum(fixef(mod)[c(1,4,5)]), fast.t=fixef(mod)[2],
                         slow.conc = fixef(mod)[5], slow.per = fixef(mod)[5]/ sum(fixef(mod)[c(1,4,5)]), slow.t=fixef(mod)[3],
                         peak.bas = sum(fixef(mod)[c(4,5)])/fixef(mod)[1], fast.sign = !(any(summary(mod)$fixed[c(2,4),3]<0)))
  } else {
    end = nchar(formula(mod)[2])
    element = substr(formula(mod)[2], start = 1, stop =end)
    
    ele.row = data.frame(site =  site, element = element, 
                         bas.conc = coef(mod)[1], bas.per = coef(mod)[1]/ sum(coef(mod)[c(1,4,5)]),
                         fast.conc = coef(mod)[4], fast.per = coef(mod)[4]/ sum(coef(mod)[c(1,4,5)]), fast.t=coef(mod)[2],
                         slow.conc = coef(mod)[5], slow.per = coef(mod)[5]/ sum(coef(mod)[c(1,4,5)]), slow.t=coef(mod)[3],
                         peak.bas = sum(coef(mod)[c(4,5)])/coef(mod)[1],  fast.sign = !(any(summary(mod)$parameters[c(2,4),4]>0.05)))
  }
  return(ele.row)
}


#__models####
sites <- unique(chem.str.dec$StNamn)
sites <- as.character(sites[-3])
sites <- sites[c(1,3,4,2)]
tab3 <- list()
for (k in 1:length(sites)) {
  dat <- chem.str.dec[chem.str.dec$StNamn == sites[k],]
  df <- list()
  for (i in 1:length(var.list)) {
    dat.ele <- dat[which.max(dat[[var.list[i]]]):NROW(dat),]
    dat.ele <- dat.ele[!(is.na(dat.ele[[var.list[i]]])),]
    dat.ele$days <- as.numeric(as.Date(dat.ele[["time"]]) - as.Date(dat.ele[["time"]][1]))
  form = formula(paste(var.list[i],"~ a + d*0.5^(days/b) + e*0.5^(days/c)", sep=""))
  if(sites[k] == "Ladängsbäcken"|sites[k] =="Vallsjöbäcken"|sites[k] =="Gärsjöbäcken"|sites[k] =="Myckelmossbäcken") {
    prior1 <- prior(normal(10, 10), nlpar = "a") +
      prior(normal(10, 10), nlpar = "b") +
      prior(normal(90, 25), nlpar = "c") +
      prior(normal(90, 25), nlpar = "d") +
      prior(normal(70, 25), nlpar = "e") 
    mod <- brm(bf(form, a + b+ c+ d+ e ~ 1, nl = TRUE),
                data = dat.ele, prior = prior1)
    } else {
    #mod = nls(form, data = dat.ele, control = list(maxiter = 150),
    #           start=list(a=10, b=10, c=50, d=100, e=100))
      library(nls.multstart)
    mod = nls_multstart(form, data = dat.ele,
                  iter = 2000,
                  lower = c(a=0, b=0, c=0, d=0, e=0),
                  start_lower = c(a=0, b=0, c=0, d=0, e=0),
                  start_upper = c(a=200, b=100, c=500, d=3000, e=3000),
                  supp_errors ='Y')
    }
  df[[i]] <- tab.row(mod, site = sites[k])
  print(c(i,k))
  }
tab3[[k]] <- do.call(rbind, df)
}

tab3.s <- do.call(rbind, tab3)
tab3.s[,c("bas.per","fast.per", "slow.per")] <- tab3.s[,c("bas.per","fast.per", "slow.per")]*100
tab3.s[,3:11] <- round(tab3.s[,3:11])
write.table(tab3.s, "tab3.csv", sep=",", row.names = FALSE, fileEncoding = "UTF-8")  

        # # Save data for each model and save to excel
        # sites <- unique(chem.str.dec$StNamn)
        # sites <- as.character(sites[-3])
        # sites <- sites[c(1,3,4,2)]
        # dfs <- list()
        # for (k in 1:length(sites)) {
        #   dat <- chem.str.dec[chem.str.dec$StNamn == sites[k],]
        #   tmp <- list()
        #   for (i in 1:length(var.list)) {
        #     dat.ele <- dat[which.max(dat[[var.list[i]]]):NROW(dat),]
        #     dat.ele <- dat.ele[!(is.na(dat.ele[[var.list[i]]])),]
        #     dat.ele$days <- as.numeric(as.Date(dat.ele[["time"]]) - as.Date(dat.ele[["time"]][1]))
        #     tmp[[i]] <- dat.ele[,c("time", "days",var.list[i])] 
        #   }
        #   dfs[[k]] <- tmp
        # }
        # names(dfs) <- sites
        # 
        # library(writexl)
        # write_xlsx(dfs[[1]],"Gärsjöbäcken.xlsx")
        # write_xlsx(dfs[[2]],"Myckelmossbäcken.xlsx")
        # write_xlsx(dfs[[3]],"Vallsjöbäcken.xlsx")
        # write_xlsx(dfs[[4]],"Ladängsbäcken.xlsx")

# Plot each model
tab.p <- do.call(rbind, tab3)

pp <- list()
for (j in 1:4) {
for (i in 1:7) {
if(j==1) {k <-i } else {k = (j-1)*7+i}
  print(k)
lab <- names(dfs[j])
newdat <- data.frame(days = 0:max(dfs[[j]][[i]]$days))

newdat$p.sum <- tab.p[k,3] + tab.p[k,5]*0.5^(newdat$days/tab.p[k,7]) + 
  tab.p[k,8]*0.5^(newdat$days/tab.p[k,10])

newdat$p.fast <- tab.p[k,5]*0.5^(newdat$days/tab.p[k,7])
newdat$p.slow <- tab.p[k,8]*0.5^(newdat$days/tab.p[k,10])
y <- names(dfs[[j]][[i]][,3])
pp[[k]] <- ggplot(dfs[[j]][[i]], aes_string(x = "days", y = y)) + 
  geom_line(data=newdat, aes(x = days, y = p.sum, linetype = "Modelled", colour="Modelled")) +
  geom_line(data=newdat, aes(x = days, y = p.fast, linetype = "Fast decay pool", colour="Fast decay pool")) +
  geom_line(data=newdat, aes(x = days, y = p.slow, linetype = "Slow decay pool", colour="Slow decay pool")) +
  geom_point(aes(fill="Observed"), size=2,shape = 21, colour = "black", size = 2, stroke = 0.75) +
  #scale_x_continuous(name = "Time since peak concentration (days)", 
  #                   breaks = c(0,250,500,750,1000,1250), 
  #                   limits = c(0,1250), expand = c(0, 0)) + 
  #scale_y_continuous(name = as.expression(bquote(mu*'mol' ~l^-1)), 
  #                   breaks = seq(0,400, by=50),limits = c(0,400), expand = c(0, 0)) + 
  draw_plot_label(lab,hjust = -1, vjust = -5, size=14 ) +
  #coord_cartesian(clip = 'off') +
  #panel_border(color = "black", size = 0.5, linetype = 1,
  #             remove = FALSE) +
  scale_fill_manual(name = "", values = c("Observed" = "red")) +
  scale_colour_manual("", breaks = c("Modelled", "Fast decay pool", "Slow decay pool"),
                      values=c("Modelled" = 1,"Fast decay pool" = "gray","Slow decay pool" = 1)) +
  scale_linetype_manual("",  breaks = c("Modelled", "Fast decay pool", "Slow decay pool")
                        ,values=c("Modelled" = 1,"Fast decay pool" = 1,"Slow decay pool" = 2)) +
  theme(legend.position = c(0.50, 0.8),
        legend.key.width = unit(3, "line"),
        legend.key.height = unit(1, "line"),
        legend.text = element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.margin=unit(c(0.3,0.65,0.1,0.1),"cm"))
}
}

library(gridExtra)
gar <- grid.arrange(grobs=pp[1:7], ncol=2, top ="Gärsjöbäcken")
ggsave("tab3_gar.pdf",gar, width = 20, height = 30, units = "cm")
myck <- grid.arrange(grobs=pp[8:14], ncol=2, top ="Myckelmossbäcken")
ggsave("tab3_myck.pdf",myck, width = 20, height = 30, units = "cm")
val <- grid.arrange(grobs=pp[15:21], ncol=2, top ="Vallsjöbäcken")
ggsave("tab3_val.pdf",val, width = 20, height = 30, units = "cm")
lad <- grid.arrange(grobs=pp[22:28], ncol=2, top ="Ladängsbäcken")
ggsave("tab3_lad.pdf",lad, width = 20, height = 30, units = "cm")
