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

################################################################################
# unprotected wetland area

# wetland flood-frequency cutoffs
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)

# read in unprotected wetland area estimates
unpro.area.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
unpro.percent.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
unpro.area.df = unpro.area.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                                 "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
unpro.percent.df = unpro.percent.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                                       "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
colnames(unpro.area.df) = c("county",water.reg.labels)
colnames(unpro.percent.df) = c("county",water.reg.labels)

# join regions
unpro.area.region.join.df = left_join(eco.df[,c("county","region")], unpro.area.df, by="county")
unpro.perc.region.join.df = left_join(eco.df[,c("county","region")], unpro.percent.df, by="county")

# scale areas by 1,000
unpro.area.region.join.df[,water.reg.labels] = unpro.area.region.join.df[,water.reg.labels]/1000

# join area and percent dataframes
unpro.area.region.join.df$metric = "Unprotected wetland area (1,000 ha)"
unpro.perc.region.join.df$metric = "Unprotected wetland percentage (%)"
unpro.region.join.df = rbind(unpro.area.region.join.df, unpro.perc.region.join.df)

# melt by county and region
unpro.melt = melt(unpro.region.join.df, id.vars=c("county","region","metric"))

# plot by region and wetland flood-frequency cutoff
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
p.un.regions = ggplot(unpro.melt,
                      aes(x=value,
                          y=factor(region, levels=region.order),
                          fill=factor(variable))) +
                      geom_boxplot() + 
                      scale_fill_manual(values=rev(blues)) +
                      theme(text = element_text(size=15)) +
                      facet_wrap(.~metric, scales="free_x") +
                      labs(x="",y="",
                           fill="Wetland flood-duration cutoff")
p.un.regions
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA3_Unprotected_Wetland_Area_Percent_Region_Distributions.jpeg", 
       plot=p.un.regions, width=36, height=10, units="cm", dpi=800)

################################################################################
# converted wetland areas

# read in files
ag.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Ag_Area_Totals.csv")
dev.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Impervious_Area_Totals.csv")

# update colnames
colnames(ag.df)[colnames(ag.df) == "SUM_Polygon_Area_Ha_Geodesic"] = "log[Wetland area converted\nto row crops (ha)]"
colnames(dev.df)[colnames(dev.df) == "SUM_Polygon_Area_Ha_Geodesic"] = "log[Wetland area converted\nto impervious cover (ha)]"

# join data files
ag.dev.join = left_join(ag.df[,c("NAME","County_Group","log[Wetland area converted\nto row crops (ha)]")],
                        dev.df[,c("NAME","County_Group","log[Wetland area converted\nto impervious cover (ha)]")],
                        by=c("NAME","County_Group"))

# melt
ag.dev.melt = melt(ag.dev.join, id.vars=c("NAME","County_Group"))

# plot by variables
p.converted.regions = ggplot(ag.dev.melt,
                      aes(x=log(value),
                          y=factor(County_Group, levels=region.order))) +
                      geom_boxplot() + 
                      theme(text = element_text(size=15)) +
                      facet_wrap(.~variable) +
                      labs(x="",y="") + 
                      scale_x_continuous(limits=c(-2.5,9),
                                         breaks=seq(-2,8,2))
p.converted.regions
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA4_Converted_Wetland_Area_Region_Distributions.jpeg", 
       plot=p.converted.regions, width=28, height=10, units="cm", dpi=800)

################################################################################
# climate extremes

# read in climate data 
ex.df = read.csv("dual-risk-repo/County_Summaries/All_County_Climate_Extremes.csv")

# climate variables
clim.var.order = c("-&Delta;(Frost days)",
                   "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
                   "&Delta;(Max. length dry spell)", 
                   "&Delta;(Max. length wet spell)")

# make boxplots
p.ex.regions = ggplot(ex.df) + 
                      geom_vline(xintercept=0, color="gray10") +
                      geom_boxplot(aes(x=value,
                                       y=factor(region, levels=region.order),
                                       fill=ssp_time),
                                   position=position_dodge(0.85),
                                   outlier.size = 0.75,
                                   linewidth = 0.3) +
                      facet_wrap(. ~ factor(variable, levels=clim.var.order),
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

################################################################################
# combine ecosystem service, unprotected area and percentage, converted wetland area, and climate extreme data
# to run cluster analysis in arcpro

# scale ecosystem service data
eco.df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(eco.df[,3:6], center=T, scale =T)
colnames(scale.eco.df) = c("county","region","plantn","herpn","carbon","flood")

# scale unprotected area and percent data
scale.perc.df = unpro.percent.df
scale.area.df = unpro.area.df
scale.perc.df[,water.reg.labels] = scale(scale.perc.df[,water.reg.labels], center=T, scale =T)
scale.area.df[,water.reg.labels] = scale(scale.area.df[,water.reg.labels], center=T, scale =T)

# scale converted wetland area data
scale.ag.dev.df = ag.dev.join
colnames(scale.ag.dev.df) = c("county","region","row_crops","imp_cover")
scale.ag.dev.df[,3:4] = scale(scale.ag.dev.df[,3:4], center=T, scale =T)

# scale and subset climate data
ssps = c("SSP2-4.5","SSP3-7.0")
times = c("2041-2070","2071-2100")
climate.wide.df = pivot_wider(data = ex.df,
                              names_from = variable,
                              values_from = value)
scale.clim.df = climate.wide.df
scale.clim.df[,clim.var.order] = scale(scale.clim.df[,clim.var.order], center=T, scale=T)
sub.clim.data = subset(scale.clim.df, ssp == ssps[1] & time == times[1])

## join all data

# services and unprotected area
scale.eco.area.df = inner_join(scale.eco.df, 
                               scale.area.df[,c("county",water.reg.labels[3])], 
                               by="county")
colnames(scale.eco.area.df)[7] = "area_spf"

# add unprotected percentage
scale.eco.perc.df = inner_join(scale.eco.area.df, 
                               scale.perc.df[,c("county",water.reg.labels[3])], 
                               by="county")
colnames(scale.eco.perc.df)[8] = "percent_spf"

# add converted area
scale.eco.unpro.conv.df = inner_join(scale.eco.perc.df,
                                     scale.ag.dev.df,
                                     by=c("county","region"))

# add climate data
scale.eco.unpro.conv.clim.df = inner_join(scale.eco.unpro.conv.df, 
                                          sub.clim.data, 
                                          by=c("county","region"))
colnames(scale.eco.unpro.conv.clim.df)[14:17] = c("fd","mxt100F","mdsl","mwsl")


# write data to county summary folder
write.csv(scale.eco.unpro.conv.clim.df, 
          "dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Areas_Climate_Extremes_SSP245_2041_2070_Scaled.csv")

