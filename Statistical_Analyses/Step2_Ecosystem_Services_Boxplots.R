setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(ggtext)

################################################################################
# ecosystem services

# read in ES estimates by county and region
eco.df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")

# take logarithm of flood storage capacity
eco.df$Floodwater.storage.capacity = log(eco.df$Floodwater.storage.capacity)

# columns
colnames(eco.df) = c("county","region",
                     "Plant species richness",
                     "Herpetofauna\nspecies richness",
                     "Carbon storage (1,000 Gg)",
                     "Log[Floodwater storage\ncapacity (1,000 m<sup>3</sup>)]")

# PCA on ES data
pca_result = prcomp(eco.df[,3:6], 
                    center = TRUE, scale = TRUE)
biplot(pca_result)

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
p.es.regions = ggplot(eco.melt.df) + 
                      geom_vline(xintercept=0, color="gray10") +
                      geom_boxplot(aes(x=value,
                                       y=factor(region, levels=region.order),
                                       fill=variable)) + 
                      facet_wrap(.~variable, 
                                 scales="free_x", ncol=2) +
                      labs(x="County-level ecosystem service estimate",y="") +
                      theme(legend.position = "none",
                            text = element_text(size=15),
                            strip.text = element_markdown()) +
                      scale_fill_manual(values = c("darkgreen","purple4",
                                                   "orangered3","royalblue4"))
p.es.regions
ggsave("Manuscript/Supp_Figures/AppendixB/FigureB1_Ecosystem_Service_Region_Distributions.jpeg", 
        plot=p.es.regions, width=28, height=18, units="cm", dpi=600)
#ggsave("Presentations/Poster_Figures/FigureB2_Ecosystem_Service_Region_Distributions.jpeg", 
#       plot=p.es.regions, width=36, height=10, units="cm", dpi=800)

################################################################################
# unprotected wetland area

# read in unprotected wetland area estimates
unpro.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
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
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA3_Unprotected_Wetland_Area_Region_Distributions.jpeg", 
       plot=p.un.regions, width=32, height=14, units="cm", dpi=800)

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
var.labels = list("Temp" = c("-&Delta;(Frost days)", 
                             "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])"),
                  "Precip" = c("&Delta;(Max. length dry spell)", 
                               "&Delta;(Max. length wet spell)"))
var.order = c("-&Delta;(Frost days)",
              "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
              "&Delta;(Max. length dry spell)", 
              "&Delta;(Max. length wet spell)")

# put extremes into dataframe
ex.df = data.frame(matrix(nrow=0, ncol=6))
colnames(ex.df) = c("ssp","time","variable","county","value","region")
for (i in 1:n.e) {
  ex = extremes[i]
  for (j in 1:n.s) {
    for (k in 1:n.t) {
      df = read.csv(paste(paste("dual-risk-repo/County_Summaries/Step1",ex,"BMA_County_Ave", ssps[j], times[k], sep="_"), ".csv", sep=""))
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
        if (labels[l] == "-&Delta;(Frost days)") {
          ex.df.ijkl$value  = -df[,vars[l]]
        } else {
          ex.df.ijkl$value  = df[,vars[l]]
        }
        ex.df = rbind(ex.df, ex.df.ijkl)
      }
    }
  }
}

# make column for ssp x time
ex.df$ssp_time = paste(ex.df$time, ex.df$ssp, sep=" &times; ")

# make boxplots
p.ex.regions = ggplot(ex.df) + 
                      geom_vline(xintercept=0, color="gray10") +
                      geom_boxplot(aes(x=value,
                                       y=factor(region, levels=region.order),
                                       fill=ssp_time)) +
                      facet_wrap(. ~ factor(variable, levels=var.order),
                                 scales = "free_x", ncol=2) +
                      scale_fill_brewer(palette = "Reds")  +
                      theme(text = element_text(size=15),
                            strip.text = element_markdown(),
                            legend.text = element_markdown(),
                            legend.title = element_markdown()) +
                      labs(y="",x="Change in climate extreme (days)",
                           fill = "Time interval &times; Shared<br>Socioeconomic Pathway (SSP)")
p.ex.regions

ggsave("Manuscript/Supp_Figures/AppendixC/FigureC1_Climate_Extreme_Regional_Distributions.jpeg", 
       plot=p.ex.regions, width=34, height=18, units="cm", dpi=800)

# write extremes to csv
write.csv(ex.df, "Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv", row.names=F)
