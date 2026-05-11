setwd("F:/Wetland_Climate_Impacts/Flood_Volume_Estimation")

library(dplyr)
library(ggplot2)
library(patchwork)

################################################################################
# metadata

# water regimes / Wetland Flood-Frequency Cutoffs
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                  "Seasonally Flooded/Saturated","Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                     "Seasonally Flooded/Saturated","Seasonally Flooded")
water.reg.abrvs = c("PF","IE","SPF","SFS","SF")
n.w = length(water.regimes)

# stream flow permanence criteria
perm.levels = c("Perennial","Intermittent","Ephemeral")
perm.abrvs = c("1","2","3")
n.p = length(perm.abrvs)

# make vectors for buffer scenarios
buf.dists = c("1","10","20","100")
buf.cols = paste("buf", buf.dists, sep="")
n.b = length(buf.dists)

# read in area threshold dataframe with volume estimates
area.threshold.df = read.csv("IL_WS_Step21_AreaThreshold_FloodVol.csv")

# update columns
nhd.col.base = paste("Waters_Intersect", perm.abrvs, sep="_")
brf.col.base = paste("Brinkerhoff_Intersect", perm.abrvs, sep="_")
nhd.cols = c()
brf.cols = c()
for (i in 1:n.b) {
  nhd.cols = c(nhd.cols, paste(nhd.col.base, buf.dists[i], sep="_"))
  brf.cols = c(brf.cols, paste(brf.col.base, buf.dists[i], sep="_"))
}
colnames(area.threshold.df)[34:45] = nhd.cols
colnames(area.threshold.df)[46:57] = brf.cols

# fix polygon area columns
colnames(area.threshold.df)[58:59] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(area.threshold.df)[61:62] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# unit-area estimate of flood storage volume
area.threshold.df$unit_flood_lower = area.threshold.df$Polygon_Area_Ac_Geodesic * 1 * (10^6) / 264.172
area.threshold.df$unit_flood_upper = area.threshold.df$Polygon_Area_Ac_Geodesic * 1.5 * (10^6) / 264.172

# update column name for GIS-based estimate of flood storage volume
colnames(area.threshold.df)[which(colnames(area.threshold.df) == "flood_volume_m3")] = "gis_flood_vol_m3"

################################################################################
# estimate total flood storage volume based on the Step10_WaterRegime shapefile

# read in water regime dataframe with volume estimates
water.regime.df = read.csv("IL_WS_Step20_WaterRegime_FloodVol.csv")

# fix polygon area columns
colnames(water.regime.df)[58:59] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(water.regime.df)[61:62] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# unit-area estimate of flood storage volume
water.regime.df$unit_flood_lower = water.regime.df$Polygon_Area_Ac_Geodesic * 1 * (10^6) / 264.172
water.regime.df$unit_flood_upper = water.regime.df$Polygon_Area_Ac_Geodesic * 1.5 * (10^6) / 264.172

# update column name for GIS-based estimate of flood storage volume
colnames(water.regime.df)[which(colnames(water.regime.df) == "flood_volume_m3")] = "gis_flood_vol_m3"

# calculate total flood storage volume for Illinois
total_gis_flood_vol_m3 = sum(water.regime.df$gis_flood_vol_m3)
total_unit_flood_vol_m3 = sum(water.regime.df$unit_flood_lower)

################################################################################
# sum up non-jurisdictional flood storage volume and compare with unit-area estimate

# make protection level column
area.threshold.df$Protection_Level = "Unprotected"
for (i in seq(1,2)) { area.threshold.df$Protection_Level[which(area.threshold.df$GAP_Sts == i)] = "Managed for biodiversity" }
area.threshold.df$Protection_Level[which((area.threshold.df$NAME %in% pro.cnties) & !(area.threshold.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
area.threshold.df$Protection_Level[which(!(area.threshold.df$NAME %in% pro.cnties) & (area.threshold.df$GAP_Sts == 3))] = "Managed for multiple uses"
area.threshold.df$Protection_Level[which(!(area.threshold.df$NAME %in% pro.cnties) & (area.threshold.df$GAP_Sts == 4))] = "Unprotected"

# make protection status column
area.threshold.df$Protection_Status = 1 - 1*(area.threshold.df$Protection_Level == "Unprotected")

# sum total and unprotected non-jurisdictional flood storage volume
sum.cols = c("Polygon_Area_Ha_Geodesic","unit_flood_lower","unit_flood_upper","gis_flood_vol_m3")
n.sum.cols = length(sum.cols)
nw.sum.df = data.frame(matrix(nrow=n.w*(n.p-1)*(n.b-2), ncol=3+n.sum.cols))
up.nw.sum.df = data.frame(matrix(nrow=n.w*(n.p-1)*(n.b-2), ncol=3+n.sum.cols))
colnames(nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
colnames(up.nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
n = 1
wetland.df.sub = area.threshold.df[which(area.threshold.df$Protection_Status == 0),]
for (i in 1:n.w) {
  for (j in 1:(n.p-1)) {
    for (k in 1:(n.b-2)) {
      nw.sum.df[n,"water_cutoff"] = water.reg.labels[i]
      nw.sum.df[n,"perm_level"] = perm.levels[j]
      nw.sum.df[n,"buf_dist"] = buf.dists[k]
      up.nw.sum.df[n,"water_cutoff"] = water.regimes[i]
      up.nw.sum.df[n,"perm_level"] = perm.levels[j]
      up.nw.sum.df[n,"buf_dist"] = buf.dists[k]
      buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
      wrs.inds1 = !(area.threshold.df$WATER_REGI %in% water.regimes[1:i])
      wrs.inds2 = !(wetland.df.sub$WATER_REGI %in% water.regimes[1:i])
      for (c in 1:n.sum.cols) {
        nonwotus.ind1 = which(wrs.inds1 | (area.threshold.df$Within_Lev == 1 | area.threshold.df[,buf.ws.col] == 0))
        nonwotus.ind2 = which(wrs.inds2 | (wetland.df.sub$Within_Lev == 1 | wetland.df.sub[,buf.ws.col] == 0))
        nw.sum.df[n, sum.cols[c]] = sum(area.threshold.df[nonwotus.ind1, sum.cols[c]])
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
       labs(x="Flood Storage Volume (millions m3)",
            y="Wetland Flood-Frequency Cutoff",
            title="DEM-Based Estimate",
            group="",color="",linetype="") +
       scale_x_continuous(limits=c(3,7),breaks=seq(3,8)) + 
       theme(text=element_text(size=14))
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
            labs(x="Flood Storage Volume (billions m3)",y="",
                 title="Unit-Area Estimate",
                 group="",color="",linetype="") +
            scale_x_continuous(limits=c(2,4)) +
            theme(axis.text.y=element_blank(),
                  text=element_text(size=14))
p2
p3 = p1 + p2 + plot_layout(guides="collect")
p3
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/FloodVolume/FigureB5_GIS_UnitArea_Flood_Volume_Absolute_Comparison.jpeg", 
       plot=p3, width=34, height=14, units="cm", dpi = 600)

# print flood storage volumes for table
wr.reg.inds = rep(0, n.w)
for (i in 1:n.w) { wr.reg.inds[i] = which(nw.stats.df.gis$water_cutoff == rev(water.reg.labels)[i]) }
signif(gis.stats.df[gis.stats.df$type == "Unprotected Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]/(10^6),4)
signif(gis.stats.df[gis.stats.df$type == "Total Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]/(10^6),4)

signif(unit.stats.df[unit.stats.df$type == "Unprotected Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]/(10^9),4)
signif(unit.stats.df[unit.stats.df$type == "Total Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]/(10^9),4)


# calculate flood storage volumes as percentages
gis.percent.stats.df = gis.stats.df
unit.percent.stats.df = unit.stats.df
gis.percent.stats.df[,c("mean_vol","min_vol","max_vol")] = gis.stats.df[,c("mean_vol","min_vol","max_vol")]/total_gis_flood_vol_m3
unit.percent.stats.df[,c("mean_vol","min_vol","max_vol")] = unit.stats.df[,c("mean_vol","min_vol","max_vol")]/total_unit_flood_vol_m3

# compare floodwater volumes as percentages
p1 = ggplot(gis.percent.stats.df,
            aes(y=factor(water_cutoff, levels=water.reg.labels), 
                x=mean_vol*100, 
                group=type,
                color=type,
                linetype=type)) + 
            geom_line() +
            geom_ribbon(data=gis.percent.stats.df,
                        aes(xmin=min_vol*100, 
                            xmax=max_vol*100, 
                            group=type,
                            color=type,
                            linetype=type), alpha=0.2) +
            labs(x="Percent of Total Flood Storage Volume (%)",
                 y="Wetland Flood-Frequency Cutoff",
                 title="DEM-Based Estimate",
                 group="",color="",linetype="") +
            #scale_x_continuous(limits=c(3,7),breaks=seq(3,8)) + 
            theme(text=element_text(size=14))
p1
p2 = ggplot(unit.percent.stats.df, 
            aes(y=factor(water_cutoff, levels=water.reg.labels), 
                x=mean_vol*100, 
                group=type,
                color=type,
                linetype=type)) + 
            geom_line() +
            geom_ribbon(data=unit.percent.stats.df,
                        aes(xmin=min_vol*100, 
                            xmax=max_vol*100, 
                            group=type,
                            color=type,
                            linetype=type), alpha=0.2) +
            labs(x="Percent of Total Flood Storage Volume (%)",
                 y="",
                 title="Unit-Area Estimate",
                 group="",color="",linetype="") +
            #scale_x_continuous(limits=c(2,4)) +
            theme(axis.text.y=element_blank(),
                  text=element_text(size=14))
p2
p3 = p1 + p2 + plot_layout(guides="collect")
p3
ggsave("Supp_Figures/FloodVolume/FigureB6_GIS_UnitArea_Flood_Volume_Percent_Comparison.jpeg", 
       plot=p3, width=34, height=14, units="cm", dpi = 600)

# print percent flood storage volumes for table
signif(gis.percent.stats.df[gis.percent.stats.df$type == "Unprotected Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]*100,3)
signif(gis.percent.stats.df[gis.percent.stats.df$type == "Total Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]*100,3)

signif(unit.percent.stats.df[unit.percent.stats.df$type == "Unprotected Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]*100,3)
signif(unit.percent.stats.df[unit.percent.stats.df$type == "Total Non-WOTUS",][wr.reg.inds,c("mean_vol","min_vol","max_vol")]*100,3)

################################################################################
# sum up total, total non-WOTUS, unprotected non-WOTUS, and protected non-WOTUS 
# flood storage volume by county for both the GIS and unit-area estimate

setwd("F:/Wetland_Climate_Impacts/Flood_Volume_Estimation")

# county names
all.counties = sort(unique(area.threshold.df$NAME))
n.ct = length(all.counties)

# loop through sum of DEM-based and unit-area estimates of flood storage volume
sum.cols.long = c("DEM_Vol","Unit_Vol_Low","Unit_Vol_High")
sum.cols.short = c("gis_flood_vol_m3","unit_flood_lower","unit_flood_upper")
n.sum.cols = length(sum.cols.long)
tnw.county.df.all = data.frame(matrix(nrow=0, ncol=5))
unw.county.df.all = data.frame(matrix(nrow=0, ncol=5))
colnames(tnw.county.df.all) = c("Name","Water_Cutoff","DEM_Vol","Unit_Vol_Low","Unit_Vol_High")
colnames(unw.county.df.all) = c("Name","Water_Cutoff","DEM_Vol","Unit_Vol_Low","Unit_Vol_High")
for (j in 1:n.w) {
  county.df.tnw = data.frame(matrix(nrow=n.ct, ncol=5))
  county.df.unw = data.frame(matrix(nrow=n.ct, ncol=5))
  colnames(county.df.tnw) = c("Name","Water_Cutoff","DEM_Vol","Unit_Vol_Low","Unit_Vol_High")
  colnames(county.df.unw) = c("Name","Water_Cutoff","DEM_Vol","Unit_Vol_Low","Unit_Vol_High")
  county.df.tnw$Water_Cutoff = water.reg.labels[j]
  county.df.unw$Water_Cutoff = water.reg.labels[j]
  for (i in 1:n.ct) {
    cnty.i = all.counties[i]
    county.df.tnw[i,"Name"] = cnty.i
    county.df.unw[i,"Name"] = cnty.i
    county.rows.tnw = area.threshold.df[which(area.threshold.df$NAME == cnty.i),]
    county.rows.unw = area.threshold.df[which(area.threshold.df$NAME == cnty.i & area.threshold.df$Protection_Status == 0),]
    buf.ws.col = paste("Brinkerhoff_Intersect","1","1", sep="_")
    wrs.inds.tnw = !(county.rows.tnw$WATER_REGI %in% water.regimes[1:j])
    wrs.inds.unw = !(county.rows.unw$WATER_REGI %in% water.regimes[1:j])
    for (c in 1:n.sum.cols) {
      nonwotus.ind.tnw = which(wrs.inds.tnw | (county.rows.tnw$Within_Lev == 1 | county.rows.tnw[,buf.ws.col] == 0))
      nonwotus.ind.unw = which(wrs.inds.unw | (county.rows.unw$Within_Lev == 1 | county.rows.unw[,buf.ws.col] == 0))
      county.df.tnw[i, sum.cols.long[c]] = sum(county.rows.tnw[nonwotus.ind.tnw, sum.cols.short[c]])
      county.df.unw[i, sum.cols.long[c]] = sum(county.rows.unw[nonwotus.ind.unw, sum.cols.short[c]])
    }                              
  }
  tnw.county.df.all = rbind(tnw.county.df.all, county.df.tnw)
  unw.county.df.all = rbind(unw.county.df.all, county.df.unw)
}

# sum total DEM- and unit-area-based estimates for each county
county.sums = water.regime.df %>%
              group_by(NAME) %>% 
              summarize(total_gis_vol_m3 = sum(gis_flood_vol_m3),
                        total_unit_vol_m3_lower = sum(unit_flood_lower),
                        total_unit_vol_m3_upper = sum(unit_flood_upper))
colnames(county.sums)[1] = "Name"

# join estimates at semipermanently flooded cutoff
spf.df.tnw = tnw.county.df.all[tnw.county.df.all$Water_Cutoff == "Semipermanently Flooded",]
spf.df.unw = unw.county.df.all[unw.county.df.all$Water_Cutoff == "Semipermanently Flooded",]
colnames(spf.df.tnw) = c("Name","Water_Cutoff",paste("TNW", colnames(spf.df.tnw)[3:5], sep="_"))
colnames(spf.df.unw) = c("Name","Water_Cutoff",paste("UPNW", colnames(spf.df.unw)[3:5], sep="_"))
cnty.volume.df = left_join(spf.df.tnw[,c("Name",colnames(spf.df.tnw)[3:5])],
                           spf.df.unw[,c("Name",colnames(spf.df.unw)[3:5])], 
                           by="Name")
cnty.volume.df = left_join(cnty.volume.df,
                           county.sums, 
                           by="Name")

# calculate percentages at semipermanently flooded cutoff
cnty.volume.df$TNW_DEM_Vol_Percent = cnty.volume.df$TNW_DEM_Vol / cnty.volume.df$total_gis_vol_m3 * 100
cnty.volume.df$TNW_Unit_Vol_Low_Percent = cnty.volume.df$TNW_Unit_Vol_Low / cnty.volume.df$total_unit_vol_m3_lower * 100
cnty.volume.df$TNW_Unit_Vol_High_Percent = cnty.volume.df$TNW_Unit_Vol_High / cnty.volume.df$total_unit_vol_m3_upper * 100

cnty.volume.df$UPNW_DEM_Vol_Percent = cnty.volume.df$UPNW_DEM_Vol / cnty.volume.df$total_gis_vol_m3 * 100
cnty.volume.df$UPNW_Unit_Vol_Low_Percent = cnty.volume.df$UPNW_Unit_Vol_Low / cnty.volume.df$total_unit_vol_m3_lower * 100
cnty.volume.df$UPNW_Unit_Vol_High_Percent = cnty.volume.df$UPNW_Unit_Vol_High / cnty.volume.df$total_unit_vol_m3_upper * 100

write.csv(cnty.volume.df, "F:/Wetland_Climate_Impacts/Flood_Volume_Estimation/SPF_County_Volume_Totals.csv",
          row.names = F)
