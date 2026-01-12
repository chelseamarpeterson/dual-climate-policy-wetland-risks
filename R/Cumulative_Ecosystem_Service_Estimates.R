project_path = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project"
setwd(project_path)

library(ggplot2)
library(reshape2)
library(patchwork)
library(dplyr)
library(remotes)
library(ggpattern)
library(gridExtra)

# conversions
AcPerHa = 2.47105

# water regimes / Wetland Flood-Frequency Cutoffs
water.regimes = c("Permanently Flooded","Intermittently Exposed",
                  "Semipermanently Flooded","Seasonally Flooded/Saturated",
                  "Seasonally Flooded","Seasonally Saturated",
                  "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded/Saturated",
                     "Seasonally Flooded","Seasonally Saturated",
                     "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# stream flow permanence criteria
perm.levels = c("Perennial","Intermittent","Ephemeral")
perm.abrvs = c("1","2","3")
n.p = length(perm.abrvs)

# make vectors for buffer scenarios
buf.dists = c("1","10","20","100")
buf.cols = paste("buf", buf.dists, sep="")
n.b = length(buf.dists)

################################################################################
# Figure 1 & Table 1: calculate and plot area of unprotected wetlands

## step 1: read in gap intersect table
flood.df = read.csv("Results/IL_WS_Step12_GAP_Union_CntyIntersect_VolEst.csv")
nutrient.df = read.csv("Results/IL_WS_Step12_GAP_Union_CntyIntersect_NutrientFloodplain_Est.csv")
wetland.df = left_join(flood.df[,-which(colnames(flood.df) %in% c("Area_Ha","Area_Acres"))], 
                       nutrient.df[,c("GAP_County_ID","Area_Ha","Area_Acres",
                                      "Restoration_Cost_ACEP","SUM_Total_Nutrient_Benefit",
                                      "SUM_Area_Ha_FEMA_100y","SUM_Area_Ha_FEMA_500y","SUM_Area_Ha_NotFloodplain",
                                      "Flood_Benefit_Dollars","Flood_Benefit_MetersCubed")])

# check area totals
sum(wetland.df$Area_Acres); sum(wetland.df$Area_Ha)
sum(nutrient.df$Area_Acres); sum(nutrient.df$Area_Ha)

## step 2: specify counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

## step 3: create column that combines GAP and county information
wetland.df$Protected_Status = rep("Unprotected", nrow(wetland.df))
for (i in seq(1,2)) {wetland.df$Protected_Status[which(wetland.df$GAP_Sts == i)] = "Managed for biodiversity"}
wetland.df$Protected_Status[which((wetland.df$NAME %in% pro.cnties) & !(wetland.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
wetland.df$Protected_Status[which(!(wetland.df$NAME %in% pro.cnties) & (wetland.df$GAP_Sts == 3))] = "Managed for multiple uses"
wetland.df$Protected_Status[which(!(wetland.df$NAME %in% pro.cnties) & (wetland.df$GAP_Sts == 4))] = "Unprotected"

## step 4: sum total unprotected non-WOTUS area and volume
unpro.area.df = data.frame(matrix(nrow=n.w*n.p*n.b, ncol=13))
sum.cols.full = c("Area_Ha","Area_Acres","Restoration_Cost_ACEP","SUM_Total_Nutrient_Benefit",
                  "SUM_Area_Ha_FEMA_100y","SUM_Area_Ha_FEMA_500y","SUM_Area_Ha_NotFloodplain",
                  "MinVol","Flood_Benefit_MetersCubed","Flood_Benefit_Dollars")
sum.cols.short = c("area_ha","area_ac","restoration_cost","nutrient_benefit",
                   "area_ha_100y","area_ha_500y","area_ha_nonfloodplain",
                   "vol_gis_m3","vol_epa_m3","flood_benefit")
n.sum.cols = length(sum.cols.full)
colnames(unpro.area.df) = c("water_cutoff","perm_level","buf_dist",sum.cols.short)
n = 1
wetland.df.sub = wetland.df[which(wetland.df$Protected_Status == "Unprotected"),]
for (j in 1:n.w) {
  for (k in 1:n.p) {
    for (b in 1:n.b) {
      unpro.area.df[n,"water_cutoff"] = water.regimes[j]
      unpro.area.df[n,"perm_level"] = perm.levels[k]
      unpro.area.df[n,"buf_dist"] = buf.dists[b]
      buf.ws.col = paste("Waters_Intersect", perm.abrvs[k], buf.dists[b], sep="_")
      wrs = water.regimes[1:j]
      wrs.inds = !(wetland.df.sub$WATER_REGI %in% wrs)
      for (c in 1:n.sum.cols) {
        all.nonwotus.ind = which(wrs.inds | (wetland.df.sub$Within_Levee == 1 | wetland.df.sub[,buf.ws.col] == 0))
        unpro.area.df[n,sum.cols.short[c]] = sum(wetland.df.sub[all.nonwotus.ind,sum.cols.full[c]])
      }
      n = n + 1
    }
  }
}

## step 5: calculate minimum, mean, and maximum for each water regime and gap category
# https://www.epa.gov/sites/default/files/2016-02/documents/functionsvaluesofwetlands.pdf
unpro.stats.df = unpro.area.df %>%
                 group_by(water_cutoff) %>% 
                 summarize(mean_restoration_cost = mean(restoration_cost),
                           min_restoration_cost = min(restoration_cost),
                           max_restoration_cost = max(restoration_cost),
                           mean_gis_m3 = mean(vol_gis_m3), 
                           min_gis_m3 = min(vol_gis_m3), 
                           max_gis_m3 = max(vol_gis_m3),
                           mean_epa_m3 = mean(vol_epa_m3), 
                           min_epa_m3 = min(vol_epa_m3), 
                           max_epa_m3 = max(vol_epa_m3),
                           mean_nutrient_dollars = mean(nutrient_benefit),
                           min_nutrient_dollars = min(nutrient_benefit),
                           max_nutrient_dollars = max(nutrient_benefit),
                           mean_flood_dollars_total = mean(flood_benefit),
                           min_flood_dollars_total = min(flood_benefit),
                           max_flood_dollars_total = max(flood_benefit),
                           mean_flood_dollars_100y = mean(flood_benefit*(area_ha_100y/area_ha)),
                           min_flood_dollars_100y = min(flood_benefit*(area_ha_100y/area_ha)),
                           max_flood_dollars_100y = max(flood_benefit*(area_ha_100y/area_ha)),
                           mean_flood_dollars_500y = mean(flood_benefit*(area_ha_500y/area_ha)),
                           min_flood_dollars_500y = min(flood_benefit*(area_ha_500y/area_ha)),
                           max_flood_dollars_500y = max(flood_benefit*(area_ha_500y/area_ha)),
                           mean_flood_dollars_nonfloodplain = mean(flood_benefit*(area_ha_nonfloodplain/area_ha)),
                           min_flood_dollars_nonfloodplain = min(flood_benefit*(area_ha_nonfloodplain/area_ha)),
                           max_flood_dollars_nonfloodplain = max(flood_benefit*(area_ha_nonfloodplain/area_ha)))

# add label for water regime
unpro.stats.df$water_label = rep(0, nrow(unpro.stats.df))
for (i in 1:n.w) { unpro.stats.df$water_label[which(unpro.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }

# flood value in dollars
flood.df.all = data.frame(matrix(nrow=0,ncol=5))
types = c("100y","500y","nonfloodplain")
type.labels = c("100-year floodplain","500-year floodplain","Outside floodplain")
stats = c("mean","min","max")
colnames(flood.df.all) = c("water_label",stats,"type")
stat.cols = paste(stats, "flood_dollars", sep="_")
n.types = length(types)
for (i in 1:n.types) {
  type.stat.cols = paste(stat.cols, types[i], sep="_")
  flood.type.df = unpro.stats.df[,c("water_label",type.stat.cols)]
  colnames(flood.type.df) = c("water_label",stats)
  flood.type.df$type = type.labels[i]
  flood.df.all = rbind(flood.df.all, flood.type.df)
}

p1 = ggplot(flood.df.all,
            aes(x=mean/(10^6), 
                y=factor(water_label, levels=water.reg.labels),
                group=type, 
                color=type)) +
            geom_line(linewidth=0.8) +
            geom_ribbon(data=flood.df.all,
                        aes(xmin=min/(10^6), 
                            xmax=max/(10^6),
                            group=type,
                            color=type), 
                            alpha=0.1, linewidth=0.9) +
            theme(text = element_text(size=12)) +
            labs(y="Wetland Flood-Frequency Cutoff",
                 x="Flood Control Benefit of Unprotected Wetlands\n(Millions of dollars)",
                 group="Location",color="Location")
p1

# compare unit-area volume estimate with GIS estimate
unpro.stats.df.unit = unpro.stats.df[,c("water_label","mean_epa_m3","min_epa_m3","max_epa_m3")]
unpro.stats.df.gis = unpro.stats.df[,c("water_label","mean_gis_m3","min_gis_m3","max_gis_m3")]
colnames(unpro.stats.df.unit)[2:4] = c("mean","min","max")
colnames(unpro.stats.df.gis)[2:4] = c("mean","min","max")
unpro.stats.df.unit$method = "Unit-area estimate"
unpro.stats.df.gis$method = "DEM-based estimate"
unpro.stats.df.vol = rbind(unpro.stats.df.unit, unpro.stats.df.gis)
p2 = ggplot(unpro.stats.df.vol,
       aes(x=mean/(10^6), 
           y=factor(water_label, levels=water.reg.labels),
           group=method,
           color=method)) +
        geom_line(linewidth=0.8) + xlim(0,4500) +
        geom_ribbon(data=unpro.stats.df.vol,
                    aes(xmin=min/(10^6), 
                        xmax=max/(10^6),
                        group=method,
                        color=method), 
                    alpha=0.1, linewidth=0.9) +
        labs(y="",
             x="Flood Control Benefit of Unprotected Wetlands\n(Millions of meters cubed)",
             group="Estimation method",color="Estimation method") +
        theme(text = element_text(size=12),
              axis.text.y = element_blank())
p2

# nutrient removal value in dollars
unpro.nutrient.df = unpro.stats.df[,c("water_label",
                                      "mean_nutrient_dollars",
                                      "min_nutrient_dollars",
                                      "max_nutrient_dollars")]
unpro.flood.df = unpro.stats.df[,c("water_label",
                                   "mean_flood_dollars_total",
                                   "min_flood_dollars_total",
                                   "max_flood_dollars_total")]

unpro.nutrient.df$service = "Nutrient removal benefit"
unpro.flood.df$service = "Flood control benefit"
colnames(unpro.nutrient.df) = c("water_label","mean","min","max","service")
colnames(unpro.flood.df) = c("water_label","mean","min","max","service")
unpro.cost.sums.df = rbind(unpro.nutrient.df, unpro.flood.df)
p3 = ggplot(unpro.cost.sums.df,
            aes(x=mean/(10^6), 
                y=factor(water_label, levels=water.reg.labels),
                group=service, color=service)) +
            geom_line(linewidth=0.8) +
            geom_ribbon(data=unpro.cost.sums.df,
                        aes(xmin=min/(10^6), 
                            xmax=max/(10^6),
                            group=service, color=service), 
                        alpha=0.1, linewidth=0.9) +
            theme(text = element_text(size=12)) +
            xlim(0,600) +
            labs(y="Wetland Flood-Frequency Cutoff",
                 x="Cost or Benefit of Unprotected Wetlands\n(Millions of dollars)",
                 group="Ecosystem service",color="Ecosystem service")
p3

unpro.cost.df = unpro.stats.df[,c("water_label",
                                  "mean_restoration_cost",
                                  "min_restoration_cost",
                                  "max_restoration_cost")]
unpro.cost.df$cost = "Restoration cost"
colnames(unpro.cost.df) = c("water_label","mean","min","max","cost")
p4 = ggplot(unpro.cost.df,
            aes(x=mean/(10^6), 
                y=factor(water_label, levels=water.reg.labels),
                group=cost, color=cost)) +
            geom_line(linewidth=0.8) +
            geom_ribbon(data=unpro.cost.df,
                        aes(xmin=min/(10^6), 
                            xmax=max/(10^6),
                            group=cost, color=cost), 
                        alpha=0.1, linewidth=0.9) +
            theme(text = element_text(size=12),
                  axis.text.y = element_blank()) +
            xlim(0,3500) +
            labs(y="",
                 x="Cost or Benefit of Unprotected Wetlands\n(Millions of dollars)",
                 group="Economic impact",color="Economic impact")
p4

p5 = (p1+p2)/(p3+p4)
p5
ggsave("Dual-Risk-Repo/Figures/Figure_Nutrient_Flood_Benefit_of_Unprotected_Wetlands.jpeg", 
       plot = p5, width = 40, height = 24, units="cm", dpi=600)
