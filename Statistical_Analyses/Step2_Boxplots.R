setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Dissertation/Ch5_Dual_Wetland_Risk")

library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(ggtext)

################################################################################
# ecosystem services

# read in ES estimates by county and region
eco_df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")

# take logarithm of flood storage capacity
eco_df$Floodwater.storage.capacity = log(eco_df$Floodwater.storage.capacity)

# columns
colnames(eco_df) = c("county","region",
                     "Plant species richness",
                     "Herpetofauna\nspecies richness",
                     "Carbon storage (1,000 Gg)",
                     "Log[Floodwater storage\ncapacity (1,000 m<sup>3</sup>)]")

# PCA on ES data
pca_result = prcomp(eco_df[,3:6], center = TRUE, scale = TRUE)
biplot(pca_result)

# melt
eco_melt_df = melt(eco_df, id.vars = c("county","region"))

# region order
region_order = c("Shawnee Hills/Coastal Plain",
                 "Southern Till Plain/Wabash River Border",
                 "Western Forest-Prairie",
                 "Grand Prairie",
                 "Wisconsin Driftless/Rock R. Hill Country",
                 "Northeastern Morainal")

# make boxplots
p_es_regions = ggplot(eco_melt_df) + 
                      geom_vline(xintercept=0, color="gray10") +
                      geom_boxplot(aes(x=value,
                                       y=factor(region, 
                                                levels=region_order),
                                       fill=variable)) + 
                      facet_wrap(.~variable, 
                                 scales="free_x", 
                                 ncol=2) +
                      labs(x="County-level ecosystem service estimate",
                           y="") +
                      theme(legend.position = "none",
                            text = element_text(size=15),
                            strip.text = element_markdown()) +
                      scale_fill_manual(values = c("darkgreen","purple4",
                                                   "orangered3","royalblue4"))
p_es_regions
ggsave("Manuscript/Supp_Figures/AppendixB/FigureB1_Ecosystem_Service_Region_Distributions.jpeg", 
        plot=p_es_regions, width=28, height=18, units="cm", dpi=600)

################################################################################
# unprotected wetland area

# wetland flood-frequency cutoffs
water_reg_labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n_w = length(water_reg_labels)

# read in unprotected wetland area estimates
unpro.area.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Areas.csv")
unpro.percent.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percents.csv")
unpro.area.df = unpro.area.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                                 "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
unpro.percent.df = unpro.percent.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                                       "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
colnames(unpro.area.df) = c("county",water_reg_labels)
colnames(unpro.percent.df) = c("county",water_reg_labels)

# join regions
unpro.area.region.join.df = left_join(eco_df[,c("county","region")], 
                                      unpro.area.df, by="county")
unpro.perc.region.join.df = left_join(eco_df[,c("county","region")], 
                                      unpro.percent.df, by="county")

# scale areas by 1,000
unpro.area.region.join.df[,water_reg_labels] = unpro.area.region.join.df[,water_reg_labels]/1000

# join area and percent dataframes
unpro.area.region.join.df$metric = "Unprotected wetland area (1,000 ha)"
unpro.perc.region.join.df$metric = "Unprotected wetland percentage (%)"
unpro.region.join.df = rbind(unpro.area.region.join.df,
                             unpro.perc.region.join.df)

# melt by county and region
unpro.melt = melt(unpro.region.join.df, id.vars=c("county","region","metric"))

# plot by region and wetland flood-frequency cutoff
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
p.un.regions = ggplot(unpro.melt,
                      aes(x=value,
                          y=factor(region, levels=region_order),
                          fill=factor(variable))) +
                      geom_boxplot() + 
                      scale_fill_manual(values=rev(blues)) +
                      theme(text = element_text(size=15)) +
                      facet_wrap(.~metric, scales="free_x") +
                      labs(x="",y="",
                           fill="Wetland flood-duration cutoff")
p.un.regions
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA4_Unprotected_Wetland_Area_Percent_Region_Distributions.jpeg", 
       plot=p.un.regions, width=36, height=10, units="cm", dpi=800)

################################################################################
# converted wetland areas

# read in files
converted.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Wetland_Area_Percents.csv")

# melt
converted.melt = melt(converted.df[,c("NAME","County_Group",
                                      "row_crop_area","imp_cover_area",
                                      "row_crop_percent","imp_cover_percent")], 
                      id.vars=c("NAME","County_Group"))

# new labels
converted.melt$label = 0
new_labels = c("Wetland area converted\nto row crops (ha)",
               "Wetland area converted\nto impervious cover (ha)",
               "Wetland area converted\nto row crops (%)",
               "Wetland area converted\nto impervious cover (%)")
old_labels = c("row_crop_area",
               "imp_cover_area",
               "row_crop_percent",
               "imp_cover_percent")
converted.melt$label = 0
for (i in 1:4) { converted.melt$label[converted.melt$variable == old_labels[i]] = new_labels[i] }

# plot by variables
p.converted.regions = ggplot(converted.melt,
                      aes(x=value,
                          y=factor(County_Group, levels=region_order))) +
                      geom_boxplot() + 
                      theme(text = element_text(size=15)) +
                      facet_wrap(.~label, scales="free_x") +
                      scale_x_continuous(labels = scales::label_comma()) +
                      labs(x="",y="") 
p.converted.regions
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA4_Converted_Wetland_Area_Region_Distributions.jpeg", 
       plot=p.converted.regions, width=24, height=14, units="cm", dpi=800)

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
                                       y=factor(region, levels=region_order),
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
eco_df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
scale_eco_df = eco_df
es_cols = c("Plant.species.richness","Herpetofauna.species.richness",
            "Carbon.storage","Floodwater.storage.capacity")
scale_eco_df[,es_cols] = scale(eco_df[,es_cols], center=T, scale =T)
colnames(scale_eco_df) = c("county","region","plantn","herpn","carbon","flood")

# scale unprotected area data
scale_area_df = unpro.area.df
scale_area_df[,water_reg_labels] = scale(scale_area_df[,water_reg_labels], 
                                         center=T, scale =T)

# scale unprotected percent data
scale_perc_df = unpro.percent.df
scale_perc_df[,water_reg_labels] = scale(scale_perc_df[,water_reg_labels], 
                                         center=T, scale =T)
# scale converted wetland area data
scaled_converted_df = converted.df[,c("NAME","row_crop_area","row_crop_percent",
                                      "imp_cover_area","imp_cover_percent")]
converted_cols = c("row_crop_area","row_crop_percent","imp_cover_area","imp_cover_percent")
colnames(scaled_converted_df)[1] = c("county")
scaled_converted_df[,converted_cols] = scale(scaled_converted_df[,converted_cols],
                                             center=T, scale =T)

# scale and subset climate data
ssps = c("SSP2-4.5","SSP3-7.0")
times = c("2041-2070","2071-2100")
n_s = length(ssps)
n_t = length(times)
for (i in 1:n_s) {
  for (j in 1:n_t) {
    climate_wide_df = pivot_wider(data = ex.df,
                                  names_from = variable,
                                  values_from = value)
    scale_clim_df = climate_wide_df
    scale_clim_df[,clim.var.order] = scale(scale_clim_df[,clim.var.order], center=T, scale=T)
    sub_clim_data = subset(scale_clim_df, ssp == ssps[i] & time == times[j])
    
    ## join all data
    
    # services and unprotected area
    scale_eco_area_df = inner_join(scale_eco_df, 
                                   scale_area_df[,c("county",water_reg_labels[3])], 
                                   by="county")
    colnames(scale_eco_area_df)[colnames(scale_eco_area_df) == water_reg_labels[3]] = "area_spf"
    
    # add unprotected percentage
    scale_eco_perc_df = inner_join(scale_eco_area_df, 
                                   scale_perc_df[,c("county",water_reg_labels[3])], 
                                   by="county")
    colnames(scale_eco_perc_df)[colnames(scale_eco_perc_df) == water_reg_labels[3]] = "percent_spf"
    
    # add converted area
    scale_eco_unpro_conv_df = inner_join(scale_eco_perc_df,
                                         scaled_converted_df,
                                         by=c("county"))
    
    # add climate data
    scale_eco_unpro_conv_clim_df = inner_join(scale_eco_unpro_conv_df, 
                                              sub_clim_data, 
                                              by=c("county","region"))
    colnames(scale_eco_unpro_conv_clim_df)[colnames(scale_eco_unpro_conv_clim_df) %in% clim.var.order] = c("fd","mxt100F","mdsl","mwsl")
    
    # write data to county summary folder
    write.csv(scale_eco_unpro_conv_clim_df, 
              paste("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Converted_Areas_Climate_Extremes",
                    ssps[i], times[j], "Scaled.csv", sep="_"))
  }
}

