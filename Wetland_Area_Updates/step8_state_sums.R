setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Dissertation/Ch5_Dual_Wetland_Risk")

library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
library(RColorBrewer)
library(extrafont)
extrafont::loadfonts(device = "win")

# NLCD years
years = c("2023","2024")
n_y = length(years)

# read in NWI polygon files
wr_list = list()
at_list = list()
state_wetland_areas = list()
for (i in 1:n_y) {
  wr_list[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step10_", years[i], "NLCD_WaterRegime.csv", sep=""))
  state_wetland_areas[[years[i]]] = sum(wr_list[[years[i]]]$Polygon_Area_Ha_Geodesic)
  at_list[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step11_", years[i], "NLCD_AreaThreshold.csv", sep=""))
}

# water regimes / Wetland Flood-Frequency Cutoffs (dropped "Seasonally Flooded/Saturated")
water_regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
water_reg_labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n_w = length(water_regimes)

# buffer distance scenarios
buf_dists_2023 = 10^(seq(0,2))
buf_dists_2024 = 5*c(1,4)
n_bd_2023 = length(buf_dists_2023)
n_bd_2024 = length(buf_dists_2024)

# buffer types
buf_type = c("uniform","variable")
n_bt = length(buf_type)

# flow permanence scenarios
perm_levels = seq(1,3)
n_p = length(perm_levels)

# counties with protection
pro_cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n_pc = length(pro_cnties)

# nhd versions
versions = c("nhd","brinkerhoff")
version_labels = c("NHD-based","Brinkerhoff-updated")
n_v = length(versions)

################################################################################
# total non-WOTUS area comparison

# read in exact numbers used in previous studies
stats_df_2023 = read.csv("NWI_Wetlands/Area_Summaries_2025_Study.csv")
area_stats_df_2023 = subset(stats_df_2023, stat == "area" & category == "total non-WOTUS")
percent_stats_df_2023 = subset(stats_df_2023, stat == "percent" & category == "total non-WOTUS")

# 2024 uniform buffer distances: create data frame with to estimate non-WOTUS areas
area_df_2024 = data.frame(matrix(nrow=n_w*n_p*n_bd_2024, ncol=7))
colnames(area_df_2024) = c("year","water_cutoff","perm_level","buf_dist","buf_type","version","area")
n = 1
nwi_df = at_list[["2024"]]
for (i in 1:n_w) {
  for (j in 1:n_p) {
    for (k in 1:n_bd_2024) {
      # assign policy scenario labels
      area_df_2024[n,"year"] = "2024"
      area_df_2024[n,"water_cutoff"] = water_regimes[i]
      area_df_2024[n,"perm_level"] = perm_levels[j]
      area_df_2024[n,"buf_dist"] = buf_dists_2024[k]
      area_df_2024[n,"buf_type"] = "uniform"
      area_df_2024[n,"version"] = "Brinkerhoff-updated"

      # get all water regimes up to the ith and then identify rows with insufficient flooding
      wrs_inds = !(nwi_df$WATER_REGI %in% water_regimes[1:i])
      
      # identify all wetland polygons that (1) don't meet the flood frequency cutoff,
      # (2) occur within a leveed area, or (3) don't intersect the WOTUS buffer
      buf_ws_col = paste("BRFIn", perm_levels[j], "_", buf_dists_2024[k], sep="")
      area_df_2024[n,"area"] = sum(nwi_df[which(wrs_inds | (nwi_df$Within_Lev == 1 | nwi_df[,buf_ws_col] == 0)),"Polygon_Area_Ha_Geodesic"])

      # increment counter
      n = n + 1
    }
  }
}

# estimate cumulative total non-WOTUS area at each wetland flood-frequency cutoff
area_stats_df_2024 = area_df_2024 %>%
                     group_by(water_cutoff) %>% 
                     summarize(mean = mean(area),
                               min = min(area),
                               max = max(area))
percent_stats_df_2024 = area_stats_df_2024
percent_stats_df_2024[,c("mean","min","max")] = percent_stats_df_2024[,c("mean","min","max")]/state_wetland_areas[[years[2]]]*100

# print total non-WOTUS area stats for table
wr_ind = 0
for (i in 1:n_w) { wr_ind[i] = which(area_stats_df_2024$water_cutoff == rev(water_regimes)[i]) }
round(data.frame(area_stats_df_2024[wr_ind, c("mean","min","max")]),1)
round(data.frame(area_stats_df_2024[wr_ind, c("mean","max","min")]) - data.frame(area_stats_df_2023[,c("mean","max","min")]),1)

# print non-WOTUS percent stats for table
wr_ind = 0
for (i in 1:n_w) { wr_ind[i] = which(percent_stats_df_2024$water_cutoff == rev(water_regimes)[i]) }
round(data.frame(percent_stats_df_2024[wr_ind, c("mean","min","max")]),2)
round(data.frame(percent_stats_df_2024[wr_ind, c("mean","max","min")]) - data.frame(percent_stats_df_2023[,c("mean","max","min")]),2)

# plot area comparison between each NHD version
colnames(area_stats_df_2023)[1] = "water_cutoff"
colnames(area_stats_df_2024)
area_stats_df_2023$version = "NHD-based"
area_stats_df_2024$version = "Brinkerhoff-updated"
plot_total_areas = rbind(area_stats_df_2023[,colnames(area_stats_df_2024)], 
                         area_stats_df_2024)
plot_total_areas$version[plot_total_areas$version == "Brinkerhoff-updated"] = "2024 NLCD x Brinkerhoff"
plot_total_areas$version[plot_total_areas$version == "NHD-based"] = "2023 NLCD x NHD"
p1 = ggplot(plot_total_areas, 
            aes(x=mean/1000, y=factor(water_cutoff, levels=water_regimes),
                group=version, color=version, linetype=version)) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data = plot_total_areas,
                        aes(xmin=min/1000, xmax=max/1000, 
                            group=version, color=version, linetype=version), 
                        alpha=0.1, linewidth=1) +
            scale_x_continuous(limits=c(240,400), 
                               labels=scales::comma,
                               name = "Total non-WOTUS wetland area (1,000 ha)",
                               sec.axis = sec_axis(
                                transform = ~ . / state_wetland_areas[["2024"]] * 100 * 1000,
                                name = "Percent of total state wetland area (%)",
                                breaks = seq(60,100, by=10))) +
            scale_color_manual(values = c("gray10","gray10")) +
            labs(y="Wetland flood-duration cutoff",
                 x="Total non-WOTUS wetland area (1,000 ha)",
                 group="",color="",linetype="") + 
            #annotate("text", x = -Inf, y = Inf, label = "a",
            #         hjust = -0.5, vjust = 1.5, 
            #         size = 8, family = "sans", fontface = "plain") +
            theme(legend.position="none",
                  text=element_text(size=12))
p1
 
################################################################################
# unprotected non-WOTUS area comparison

# read in exact numbers used in previous studies
stats_df_2023 = read.csv("NWI_Wetlands/Area_Summaries_2025_Study.csv")
unpro_area_stats_df_2023 = subset(stats_df_2023, stat == "area" & category == "unprotected non-WOTUS")
unpro_percent_stats_df_2023 = subset(stats_df_2023, stat == "percent" & category == "unprotected non-WOTUS")

gap_list = list()
for (i in 1:n_y) {
  # read in NWI files intersected with county and GAP layers
  gap_df = read.csv(paste("NWI_Wetlands/IL_WS_Step13_",years[i],"NLCD_GAP_Union_CntyIntersect.csv", sep=""))
  
  # create column that combines GAP and county information
  gap_df$Protected_Status = "Locally unprotected"
  for (j in seq(1,2)) {gap_df$Protected_Status[which(gap_df$GAP_Sts == j)] = "Managed for biodiversity"}
  gap_df$Protected_Status[which((gap_df$NAME %in% pro_cnties) & !(gap_df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  gap_df$Protected_Status[which(!(gap_df$NAME %in% pro_cnties) & (gap_df$GAP_Sts == 3))] = "Managed for multiple uses"
  gap_df$Protected_Status[which(!(gap_df$NAME %in% pro_cnties) & (gap_df$GAP_Sts == 4))] = "Locally unprotected"
  
  # add to list
  gap_list[[years[i]]] = gap_df
}

# sum unprotected non-WOTUS area for each policy scenario and NHD version
unpro_area_df_2024 = data.frame(matrix(nrow=n_w*n_p*n_bd_2024, ncol=6))
colnames(unpro_area_df_2024) = c("year","water_cutoff","perm_level","buf_dist","version","area")
n = 1
gap_df = gap_list[["2024"]]
gap_df_sub = gap_df[gap_df$Protected_Status == "Locally unprotected",]
for (i in 1:n_w) {
  for (j in 1:n_p) {
    for (k in 1:n_bd_2024) {
      unpro_area_df_2024[n,"year"] = "2024"
      unpro_area_df_2024[n,"water_cutoff"] = water_regimes[i]
      unpro_area_df_2024[n,"perm_level"] = perm_levels[j]
      unpro_area_df_2024[n,"buf_dist"] = buf_dists_2024[k]
      unpro_area_df_2024[n,"buf_type"] = "uniform"
      unpro_area_df_2024[n,"version"] = "Brinkerhoff-updated"
      wrs_inds = !(gap_df_sub$WATER_REGI %in% water_regimes[1:i])
      buf_ws_col = paste("BRFIn", perm_levels[j], "_", buf_dists_2024[k], sep="")
      unpro_area_df_2024[n,"area"] = sum(gap_df_sub[which(wrs_inds | (gap_df_sub$Within_Lev == 1 | gap_df_sub[,buf_ws_col] == 0)),"Polygon_Area_Ha_Geodesic"])
      n = n + 1 
    }
  }
}

# estimate cumulative unprotected non-WOTUS area at each wetland flood-frequency cutoff
unpro_area_stats_df_2024 = unpro_area_df_2024 %>%
                           group_by(water_cutoff) %>% 
                           summarize(mean = mean(area),
                                     min = min(area),
                                     max = max(area))
unpro_percent_stats_df_2024 = unpro_area_stats_df_2024
unpro_percent_stats_df_2024[,c("mean","min","max")] = unpro_percent_stats_df_2024[,c("mean","min","max")]/state_wetland_areas[[years[2]]]*100

# print area stats for table
wr_ind = 0
for (i in 1:n_w) { wr_ind[i] = which(unpro_area_stats_df_2024$water_cutoff == rev(water_regimes)[i]) }
round(data.frame(unpro_area_stats_df_2024[wr_ind, c("mean","min","max")]),1)
round(data.frame(unpro_area_stats_df_2024[wr_ind, c("mean","max","min")]) - data.frame(unpro_area_stats_df_2023[,c("mean","max","min")]),1)

# print percent stats for table
wr_ind = 0
for (i in 1:n_w) { wr_ind[i] = which(unpro_percent_stats_df_2024$water_cutoff == rev(water_regimes)[i]) }
round(data.frame(unpro_percent_stats_df_2024[wr_ind, c("mean","min","max")]),2)
round(data.frame(unpro_percent_stats_df_2024[wr_ind, c("mean","max","min")]) - data.frame(unpro_percent_stats_df_2023[, c("mean","max","min")]),2)

# plot area comparison between each NHD version
colnames(unpro_area_stats_df_2023)[1] = "water_cutoff"
unpro_area_stats_df_2023$version = "NHD-based"
unpro_area_stats_df_2024$version = "Brinkerhoff-updated"
plot_unpro_areas = rbind(unpro_area_stats_df_2023[,colnames(unpro_area_stats_df_2024)], 
                         unpro_area_stats_df_2024)
plot_unpro_areas$version[plot_unpro_areas$version == "Brinkerhoff-updated"] = "2024 NLCD x Brinkerhoff"
plot_unpro_areas$version[plot_unpro_areas$version == "NHD-based"] = "2023 NLCD x NHD"
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
p2 = ggplot(plot_unpro_areas, 
            aes(x=mean/1000, y=factor(water_cutoff, levels=water_regimes),
                group=version, color=version, linetype=version)) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data=plot_unpro_areas,
                        aes(xmin=min/1000, xmax=max/1000,
                            y=factor(water_cutoff, levels=water_regimes),
                            group=version, color=version, linetype=version), 
                        alpha=0.1,linewidth=1) +
            scale_x_continuous(limits=c(200,325), 
                               labels=scales::comma,
                               name = "Unprotected non-WOTUS wetland area (1,000 ha)",
                               sec.axis = sec_axis(
                                 transform = ~ . / state_wetland_areas[["2024"]] * 100 * 1000,
                                 name = "Percent of total state wetland area (%)")) +
            scale_color_manual(values = c("gray10","gray10")) +
            labs(y="", #"Wetland flood-frequency cutoff",
                 x="Unprotected non-WOTUS wetland area (1,000 ha)",
                 group="",color="",linetype="") + 
            #annotate("text", x = -Inf, y = Inf, label = "b",
            #         hjust = -0.5, vjust = 1.5, 
            #         size = 8, family = "sans", fontface = "plain") +
            theme(text=element_text(size=12),
                  axis.text.y = element_blank())
#axis.text.y = element_text(colour = rev(blues) 
p2

p1+p2

p3 = p1 + p2
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA3_Total_And_Unprotected_NonWOTUS_Area_Updated.jpeg", 
       plot=p3, width=30, height=9, units="cm", dpi=1000)

