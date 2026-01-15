setwd("F:/Wetland_Climate_Impacts/Flood_Volume_Estimation")

library(dplyr)
library(ggplot2)
library(patchwork)

################################################################################
# metadata

# water regimes / Wetland Flood-Frequency Cutoffs
water.regimes = c("Permanently Flooded","Intermittently Exposed",
                  "Semipermanently Flooded","Seasonally Flooded/Saturated",
                  "Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded/Saturated",
                     "Seasonally Flooded")
water.reg.abrvs = c("PF","IE","SPF","SFS","SF")
n.w = length(water.regimes)

# stream flow permanence criteria
perm.levels = c("Perennial","Intermittent")
perm.abrvs = c("1","2")
n.p = length(perm.abrvs)

# make vectors for buffer scenarios
buf.dists = c("1","10","20")
buf.cols = paste("buf", buf.dists, sep="")
n.b = length(buf.dists)

# read in flood dataframe
wetland.df = read.csv("IL_WS_Step12_GAP_Union_CntyInt_FloodVol.csv")

# update columns
nhd.col.base = paste("Waters_Intersect", perm.abrvs, sep="_")
brf.col.base = paste("Brinkerhoff_Intersect", perm.abrvs, sep="_")
nhd.cols = c()
brf.cols = c()
for (i in 1:n.b) {
  nhd.cols = c(nhd.cols, paste(nhd.col.base, buf.dists[i], sep="_"))
  brf.cols = c(brf.cols, paste(brf.col.base, buf.dists[i], sep="_"))
}
colnames(wetland.df)[37:48] = nhd.cols
colnames(wetland.df)[49:60] = brf.cols

# counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

################################################################################
# sum up non-jurisdictional flood storage volume and compare with unit-area estimate

# unit-area estimate of flood storage volume
wetland.df$unit_flood_lower = wetland.df$Area_Acres * 1 * (10^6) / 264.172
wetland.df$unit_flood_upper = wetland.df$Area_Acres * 1.5 * (10^6) / 264.172

# update column name for GIS-based estimate of flood storage volume
colnames(wetland.df)[which(colnames(wetland.df) == "flood_volu")] = "gis_flood_vol_m3"

# make protection level column
wetland.df$Protection_Level = rep("Unprotected", nrow(wetland.df))
for (i in seq(1,2)) {wetland.df$Protection_Level[which(wetland.df$GAP_Sts == i)] = "Managed for biodiversity"}
wetland.df$Protection_Level[which((wetland.df$NAME %in% pro.cnties) & !(wetland.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
wetland.df$Protection_Level[which(!(wetland.df$NAME %in% pro.cnties) & (wetland.df$GAP_Sts == 3))] = "Managed for multiple uses"
wetland.df$Protection_Level[which(!(wetland.df$NAME %in% pro.cnties) & (wetland.df$GAP_Sts == 4))] = "Unprotected"

# make protection status column
wetland.df$Protection_Status = 1 - 1*(wetland.df$Protection_Level == "Unprotected")

# sum total and unprotected non-jursidicational floodwaters
sum.cols = c("Area_Ha","unit_flood_lower","unit_flood_upper","gis_flood_vol_m3")
n.sum.cols = length(sum.cols)
nw.sum.df = data.frame(matrix(nrow=n.w*n.p*n.b, ncol=3+n.sum.cols))
up.nw.sum.df = data.frame(matrix(nrow=n.w*n.p*n.b, ncol=3+n.sum.cols))
colnames(nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
colnames(up.nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
n = 1
wetland.df.sub = wetland.df[which(wetland.df$Protection_Status == 0),]
for (i in 1:n.w) {
  for (j in 1:n.p) {
    for (k in 1:n.b) {
      nw.sum.df[n,"water_cutoff"] = water.reg.labels[i]
      nw.sum.df[n,"perm_level"] = perm.levels[j]
      nw.sum.df[n,"buf_dist"] = buf.dists[k]
      up.nw.sum.df[n,"water_cutoff"] = water.regimes[i]
      up.nw.sum.df[n,"perm_level"] = perm.levels[j]
      up.nw.sum.df[n,"buf_dist"] = buf.dists[k]
      buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
      wrs.inds1 = !(wetland.df$WATER_REGI %in% water.regimes[1:i])
      wrs.inds2 = !(wetland.df.sub$WATER_REGI %in% water.regimes[1:i])
      for (c in 1:n.sum.cols) {
        nonwotus.ind1 = which(wrs.inds1 | (wetland.df$Within_Lev == 1 | wetland.df[,buf.ws.col] == 0))
        nonwotus.ind2 = which(wrs.inds2 | (wetland.df.sub$Within_Lev == 1 | wetland.df.sub[,buf.ws.col] == 0))
        nw.sum.df[n, sum.cols[c]] = sum(wetland.df[nonwotus.ind1, sum.cols[c]])
        up.nw.sum.df[n, sum.cols[c]] = sum(wetland.df.sub[nonwotus.ind2, sum.cols[c]])
      }
      n = n + 1
    }
  }
}

## calculate summary statistics for total and unprotected non-wotus areas & volumes

# total
nw.stats.df.gis = nw.sum.df %>%
                  group_by(water_cutoff) %>% 
                  summarize(mean_vol = mean(gis_flood_vol_m3),
                            min_vol = min(gis_flood_vol_m3),
                            max_vol = max(gis_flood_vol_m3))
nw.stats.df.gis$type = "Total Non-WOTUS"
nw.stats.df.unit = nw.sum.df %>%
                   group_by(water_cutoff) %>% 
                   summarize(mean_vol = mean(unit_flood_lower),
                             min_vol = min(unit_flood_lower),
                             max_vol = max(unit_flood_lower))
nw.stats.df.unit$type = "Total Non-WOTUS"

# unprotected
up.nw.stats.df.gis = up.nw.sum.df %>%
                     group_by(water_cutoff) %>% 
                     summarize(mean_vol = mean(gis_flood_vol_m3),
                               min_vol = min(gis_flood_vol_m3),
                               max_vol = max(gis_flood_vol_m3))
up.nw.stats.df.gis$type = "Unprotected Non-WOTUS"
up.nw.stats.df.unit = up.nw.sum.df %>%
                      group_by(water_cutoff) %>% 
                      summarize(mean_vol = mean(unit_flood_lower),
                                min_vol = min(unit_flood_lower),
                                max_vol = max(unit_flood_lower))
up.nw.stats.df.unit$type = "Unprotected Non-WOTUS"

# combine dataframes
gis.stats.df = rbind(nw.stats.df.gis, up.nw.stats.df.gis)
unit.stats.df = rbind(nw.stats.df.unit, up.nw.stats.df.unit)

# compare total unprotected volumes 
p1 = ggplot(gis.stats.df,
            aes(y=factor(water_cutoff, levels=water.reg.labels), 
                x=mean_vol/(10^6), 
                group=type,
                color=type,
                linetype=type)) + 
            geom_line() +
            geom_ribbon(data=gis.stats.df,
                        aes(xmin=min_vol/(10^6), 
                            xmax=max_vol/(10^6), 
                            group=type,
                            color=type,
                            linetype=type), alpha=0.2) +
       labs(x="Millions of meters cubed (m3)",
            y="Wetland Flood-Frequency Cutoff",
            title="DEM-based estimate",
            group="",color="",linetype="") +
       scale_x_continuous(limits=c(1.5,6.5),breaks=seq(2,6)) + 
       theme(text=element_text(size=15))
p1
p2 = ggplot(unit.stats.df, 
            aes(y=factor(water_cutoff, levels=water.reg.labels), 
                x=mean_vol/(10^9), 
                group=type,
                color=type,
                linetype=type)) + 
            geom_line() +
            geom_ribbon(data=unit.stats.df,
                        aes(xmin=min_vol/(10^9), 
                            xmax=max_vol/(10^9), 
                            group=type,
                            color=type,
                            linetype=type), alpha=0.2) +
            labs(x="Billions of meters cubed (m3)",y="",
                 title="Unit-area estimate",
                 group="",color="",linetype="") +
            scale_x_continuous(limits=c(2,4)) +
            theme(axis.text.y=element_blank(),
                  text=element_text(size=15))
p2
p3 = p1 + p2 + plot_layout(guides="collect")
p3
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/FigureB1_GIS_UnitArea_Flood_Volume_Comparison.jpeg", 
       plot=p3, width=32, height=14, units="cm", dpi = 600)

################################################################################
# sum up unprotected non-WOTUS flood storage volume by county for both the GIS
# and unit-area estimate
setwd("F:/Wetland_Climate_Impacts/Flood_Volume_Estimation")

# county names
all.counties = sort(unique(wetland.df$NAME))
n.ct = length(all.counties)

# loop through sum of DEM-based and unit-area estimates of flood storage volume
sum.cols.long = c("Wetland_Area","DEM_Vol_Estimate","Unit_Estimate_Lower","Unit_Estimate_Upper")
sum.cols.short = c("Area_Ha","gis_flood_vol_m3","unit_flood_lower","unit_flood_upper")
n.sum.cols = length(sum.cols.long)
for (j in 1:n.w) {
  county.df = data.frame(matrix(nrow=n.ct, ncol=6))
  colnames(county.df) = c("Name","Water_Cutoff","Wetland_Area","DEM_Vol_Estimate",
                          "Unit_Estimate_Lower","Unit_Estimate_Upper")
  county.df$Water_Cutoff = water.reg.labels[j]
  for (i in 1:n.ct) {
    cnty.i = all.counties[i]
    county.df[i,"Name"] = cnty.i
    county.rows = wetland.df[which(wetland.df$NAME == cnty.i & wetland.df$Protection_Status == 0),]
    buf.ws.col = paste("Brinkerhoff_Intersect","1","1", sep="_")
    wrs.inds = !(county.rows$WATER_REGI %in% water.regimes[1:j])
    for (c in 1:n.sum.cols) {
      nonwotus.ind = which(wrs.inds | (county.rows$Within_Lev == 1 | county.rows[,buf.ws.col] == 0))
      county.df[i, sum.cols.long[c]] = sum(county.rows[nonwotus.ind, sum.cols.short[c]])
    }
  }
  county.df$Lower_Difference = county.df$Unit_Estimate_Lower - county.df$DEM_Vol_Estimate
  county.df$Upper_Difference = county.df$Unit_Estimate_Upper - county.df$DEM_Vol_Estimate
  write.csv(county.df, 
            paste(paste("County_Volume_Sums",
                        water.reg.abrvs[j],sep="_"),".csv",sep=""),
            row.names=F)
}

