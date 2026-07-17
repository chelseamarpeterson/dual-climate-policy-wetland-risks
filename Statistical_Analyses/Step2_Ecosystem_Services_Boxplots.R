setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(reshape2)
library(tidyverse)
library(RColorBrewer)

################################################################################
# ecosystem services

# read in ES estimates by county and region
eco.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Ecosystem_Services.csv")

# scale carbon and flood storage volume by 1000
eco.df$Floodwater.storage.capacity = log(eco.df$Floodwater.storage.capacity/1000)
eco.df$Carbon.storage = eco.df$Carbon.storage/1000

# columns
colnames(eco.df) = c("county","region",
                     "Plant species richness",
                     "Herpetofauna\nspecies richness",
                     "Carbon storage (1,000 Mg)",
                     "Log[Floodwater storage\ncapacity (1,000 m3)]")

# melt
eco.melt.df = melt(eco.df, id.vars = c("county","region"))

# region order
region.order = c("Shawnee Hills/Coastal Plain",
                 "Southern Till Plain/Wabash River Border",
                 "Western Forest-Prairie",
                 "Grand Prairie",
                 "Wisconsin Driftless/Rock R. Hill Country",
                 "Northeastern Morainal")

# make boxplots
p.es.regions = ggplot(eco.melt.df,
                      aes(x=value,
                          y=factor(region, levels=region.order),
                          fill=variable)) + 
                      geom_boxplot() + 
                      facet_wrap(.~variable, 
                                 scales="free_x", ncol=4) +
                      labs(x="County-level ecosystem service estimate",y="") +
                      theme(legend.position = "none",
                            text = element_text(size=15)) +
                      scale_fill_manual(values = c("darkgreen","purple4",
                                                   "orangered3","royalblue4"))
p.es.regions
ggsave("Manuscript/Supp_Figures/FigureB2_Ecosystem_Service_Region_Distributions.jpeg", 
        plot=p.es.regions, width=24, height=16, units="cm", dpi=600)

################################################################################
# unprotected wetland area

# read in unprotected wetland area estimates
unpro.df = read.csv("Dual-Risk-Repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
unpro.df = unpro.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff","mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)
colnames(unpro.df) = c("county",water.reg.labels)

# join regions
unpro.region.join.df = left_join(eco.df[,c("county","region")], unpro.df, by="county")

# melt by county and region
unpro.melt = melt(unpro.region.join.df, id.vars=c("county","region"))

# plot by region and wetland flood-frequency cutoff
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
p.un.regions = ggplot(unpro.melt,
                      aes(x=value/1000,
                          y=factor(region, levels=region.order),
                          fill=factor(variable))) +
                      geom_boxplot() + xlim(0,13) +
                      scale_fill_manual(values=rev(blues)) +
                      theme(text = element_text(size=15)) +
                      scale_x_continuous(limits=c(0,15),
                                         breaks=seq(0,15,5),  
                        sec.axis = sec_axis(
                        transform = ~ . / 395845.4 * 100 * 1000,
                        name = "Percent of total state wetland area (%)")) +
                      labs(x="Unprotected non-WOTUS wetland area (1,000 ha)",y="",
                           fill="Wetland flood-frequency cutoff")
p.un.regions
ggsave("Manuscript/Supp_Figures/FigureA3_Unprotected_Wetland_Area_Region_Distributions.jpeg", 
       plot=p.un.regions, width=32, height=16, units="cm", dpi=600)

################################################################################
# climate extremes

# shared socioeconomic pathways
ssps = c("SSP245","SSP370")
ssp.labels = c("SSP2-4.5","SSP3-7.0")
n.s = length(ssps)

# time intervals
times = c("2041_2070","2071_2100")
time.labels = c("2041-2070","2071-2100")
n.t = length(times)

# extremes
extremes = c("Temp","Precip")
n.e = length(extremes)

# variables
var.list = list("Temp" = c("FD","TXge100F"),
                "Precip" = c("dry_spells","wet_spell"))
var.labels = list("Temp" = c("Frost days", 
                             "Days with max. temp. over\n100 deg F (38 deg C)"),
                  "Precip" = c("Consecutive dry days", 
                               "Consecutive wet days"))

# put extremes into dataframe
ex.df = data.frame(matrix(nrow=0, ncol=6))
colnames(ex.df) = c("ssp","time","variable","county","value","region")
for (i in 1:n.e) {
  ex = extremes[i]
  for (j in 1:n.s) {
    for (k in 1:n.t) {
      df = read.csv(paste(paste("Dual-Risk-Repo/County_Summaries/Step1",ex,"BMA_County_Ave", ssps[j], times[k], sep="_"), ".csv", sep=""))
      vars = var.list[[ex]]
      labels = var.labels[[ex]]
      for (l in 1:2) {
        ex.df.ijkl = data.frame(matrix(nrow=nrow(df), ncol=4))
        colnames(ex.df.ijkl) = c("ssp","time","variable","county") 
        ex.df.ijkl$ssp = ssp.labels[j]
        ex.df.ijkl$time = time.labels[k]
        ex.df.ijkl$county = df$NAME
        ex.df.ijkl$variable = labels[l]
        ex.df.ijkl = inner_join(ex.df.ijkl, eco.df[,c("county","region")], by="county", keep=F)
        ex.df.ijkl$value  = df[,vars[l]]
        ex.df = rbind(ex.df, ex.df.ijkl)
      }
    }
  }
}

# make column for ssp x time
ex.df$ssp_time = paste(ex.df$time, ex.df$ssp, sep=" x ")

# make boxplots
var.order = c("Frost days",
              "Days with max. temp. over\n100 deg F (38 deg C)",
              "Consecutive dry days", 
              "Consecutive wet days")
p.ex.regions = ggplot(ex.df,
                      aes(x=value,
                          y=region,
                          fill=ssp_time)) + 
                      geom_boxplot() +
                      facet_wrap(. ~ factor(variable, levels=var.order),
                                 scales = "free_x", ncol=2) +
                      scale_fill_brewer(palette = "Reds")  +
                      theme(text = element_text(size=15),
                            axis.text.y = element_blank()) +
                      labs(y="",x="Change in climate extreme (days)",
                           fill = "Time interval Shared\nSocioeconomic Pathway (SSP)")
p.ex.regions

ggsave("Manuscript/Supp_Figures/FigureC2_Climate_Extreme_Region_Distributions.jpeg", 
       plot=p.ex.regions, width=24, height=16, units="cm", dpi=600)

# write extremes to csv
write.csv(ex.df, "Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv", row.names=F)
