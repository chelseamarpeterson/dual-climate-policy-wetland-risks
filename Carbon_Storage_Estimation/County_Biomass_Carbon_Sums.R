setwd("F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

library(tidyverse)
library(patchwork)

# read in csv file with biomass C stocks
df.wr = read.csv("IL_WS_Step20_WaterRegime_Filter_Biomass_Cstocks.csv")
df.at = read.csv("IL_WS_Step21_AreaThreshold_Filter_Biomass_Cstocks.csv")

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

# update columns
nhd.col.base = paste("Waters_Intersect", perm.abrvs, sep="_")
brf.col.base = paste("Brinkerhoff_Intersect", perm.abrvs, sep="_")
nhd.cols = c()
brf.cols = c()
for (i in 1:n.b) {
  nhd.cols = c(nhd.cols, paste(nhd.col.base, buf.dists[i], sep="_"))
  brf.cols = c(brf.cols, paste(brf.col.base, buf.dists[i], sep="_"))
}
colnames(df.at)[34:45] = nhd.cols
colnames(df.at)[46:57] = brf.cols
colnames(df.wr)[34:45] = nhd.cols
colnames(df.wr)[46:57] = brf.cols

# fix polygon area columns
colnames(df.at)[58:59] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(df.at)[61:62] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")
colnames(df.wr)[58:59] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(df.wr)[61:62] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# make protection level column
df.at$Protection_Level = "Unprotected"
for (i in seq(1,2)) { df.at$Protection_Level[df.at$GAP_Sts == i] = "Managed for biodiversity" }
df.at$Protection_Level[(df.at$NAME %in% pro.cnties) & !(df.at$GAP_Sts %in% c(1,2))] = "County stormwater ordinance"
df.at$Protection_Level[!(df.at$NAME %in% pro.cnties) & (df.at$GAP_Sts == 3)] = "Managed for multiple uses"
df.at$Protection_Level[!(df.at$NAME %in% pro.cnties) & (df.at$GAP_Sts == 4)] = "Unprotected"

df.wr$Protection_Level = "Unprotected"
for (i in seq(1,2)) { df.wr$Protection_Level[df.wr$GAP_Sts == i] = "Managed for biodiversity" }
df.wr$Protection_Level[(df.wr$NAME %in% pro.cnties) & !(df.wr$GAP_Sts %in% c(1,2))] = "County stormwater ordinance"
df.wr$Protection_Level[!(df.wr$NAME %in% pro.cnties) & (df.wr$GAP_Sts == 3)] = "Managed for multiple uses"
df.wr$Protection_Level[!(df.wr$NAME %in% pro.cnties) & (df.wr$GAP_Sts == 4)] = "Unprotected"

# make protection status column
df.at$Protection_Status = 1 - 1*(df.at$Protection_Level == "Unprotected")
df.wr$Protection_Status = 1 - 1*(df.wr$Protection_Level == "Unprotected")

# sum total and unprotected non-jurisdictional carbon storage
sum.cols = c("Biomass_Ctotal_Point","Biomass_Ctotal_Zonal")
n.sum.cols = length(sum.cols)
nw.sum.df = data.frame(matrix(nrow=n.w*(n.p-1)*(n.b-2), ncol=3+n.sum.cols))
up.nw.sum.df = data.frame(matrix(nrow=n.w*(n.p-1)*(n.b-2), ncol=3+n.sum.cols))
colnames(nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
colnames(up.nw.sum.df) = c("water_cutoff","perm_level","buf_dist",sum.cols)
n = 1
df.at.sub = df.at[df.at$Protection_Status == 0,]
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
      wrs.inds1 = !(df.at$WATER_REGI %in% water.regimes[1:i])
      wrs.inds2 = !(df.at.sub$WATER_REGI %in% water.regimes[1:i])
      for (c in 1:n.sum.cols) {
        nonwotus.ind1 = which(wrs.inds1 | (df.at$Within_Lev == 1 | df.at[,buf.ws.col] == 0))
        nonwotus.ind2 = which(wrs.inds2 | (df.at.sub$Within_Lev == 1 | df.at.sub[,buf.ws.col] == 0))
        nw.sum.df[n, sum.cols[c]] = sum(df.at[nonwotus.ind1, sum.cols[c]])
        up.nw.sum.df[n, sum.cols[c]] = sum(df.at.sub[nonwotus.ind2, sum.cols[c]])
      }
      n = n + 1
    }
  }
}

## calculate summary statistics for total and unprotected non-wotus areas & volumes

# total
nw.stats.df.zonal = nw.sum.df %>%
                    group_by(water_cutoff) %>% 
                    summarize(mean_carbon = mean(Biomass_Ctotal_Zonal),
                              min_carbon = min(Biomass_Ctotal_Zonal),
                              max_carbon = max(Biomass_Ctotal_Zonal))
nw.stats.df.zonal$type = "Total Non-WOTUS"
nw.stats.df.zonal$method = "Zonal"

nw.stats.df.point = nw.sum.df %>%
                    group_by(water_cutoff) %>% 
                    summarize(mean_carbon = mean(Biomass_Ctotal_Point),
                              min_carbon = min(Biomass_Ctotal_Point),
                              max_carbon = max(Biomass_Ctotal_Point))
nw.stats.df.point$type = "Total Non-WOTUS"
nw.stats.df.point$method = "Point"

# unprotected
up.nw.stats.df.zonal = up.nw.sum.df %>%
                       group_by(water_cutoff) %>% 
                       summarize(mean_carbon = mean(Biomass_Ctotal_Zonal),
                                 min_carbon = min(Biomass_Ctotal_Zonal),
                                 max_carbon = max(Biomass_Ctotal_Zonal))
up.nw.stats.df.zonal$type = "Unprotected Non-WOTUS"
up.nw.stats.df.zonal$method = "Zonal"

up.nw.stats.df.point = up.nw.sum.df %>%
                       group_by(water_cutoff) %>% 
                       summarize(mean_carbon = mean(Biomass_Ctotal_Point),
                                 min_carbon = min(Biomass_Ctotal_Point),
                                 max_carbon = max(Biomass_Ctotal_Point))
up.nw.stats.df.point$type = "Unprotected Non-WOTUS"
up.nw.stats.df.point$method = "Point"

# combine sum dataframes
stats.df.point = rbind(nw.stats.df.point, up.nw.stats.df.point)
stats.df.zonal = rbind(nw.stats.df.zonal, up.nw.stats.df.zonal)

# calculate percentages
total.carbon.storage.point = sum(df.wr$Biomass_Ctotal_Point)
total.carbon.storage.zonal = sum(df.wr$Biomass_Ctotal_Zonal)

stats = c("mean_carbon","min_carbon","max_carbon")
percent.df.point = stats.df.point
percent.df.zonal = stats.df.zonal
percent.df.point[,stats] = stats.df.point[,stats]/total.carbon.storage.point*100
percent.df.zonal[,stats] = stats.df.zonal[,stats]/total.carbon.storage.zonal*100

## compare absolute total and unprotected non-WOTUS wetland biomass carbon storage 

p.total = ggplot(stats.df.zonal,
                 aes(y=factor(water_cutoff, levels=water.reg.labels), 
                     x=mean_carbon/(10^6), 
                     group=type, color=type, linetype=type)) + 
                 geom_line() +
                 geom_ribbon(aes(xmin=min_carbon/(10^6), 
                                 xmax=max_carbon/(10^6), 
                                 group=type, color=type, linetype=type), alpha=0.2) +
                 labs(x="Total biomass carbon storage (billions kg)",
                      y="Wetland Flood-Frequency Cutoff",
                      group="",color="",linetype="") +
                 scale_x_continuous(limits=c(0,20),breaks=seq(0,20,5)) + 
                 theme(text=element_text(size=14))
p.total

p.percent = ggplot(percent.df.zonal,
                   aes(y=factor(water_cutoff, levels=water.reg.labels), 
                       x=mean_carbon, 
                       group=type, color=type, linetype=type)) + 
                   geom_line() +
                   geom_ribbon(aes(xmin=min_carbon, 
                                   xmax=max_carbon, 
                                   group=type, color=type, linetype=type), alpha=0.2) +
                   labs(x="Percent of total biomass carbon storage (%)",
                        y="",group="",color="",linetype="") +
                   scale_x_continuous(limits=c(59,100),breaks=seq(60,100,10)) + 
                   theme(text=element_text(size=14),
                         axis.text.y=element_blank())
p.percent

p.combo = p.total + p.percent + plot_layout(guides="collect")
p.combo
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/CarbonStorage/FigureC1_Carbon_Storage_Totals.jpeg", 
       plot=p.combo, width=32, height=10, units="cm", dpi = 600)

################################################################################
# sum up total, total non-WOTUS, unprotected non-WOTUS, and protected non-WOTUS 
# flood storage volume by county for both the zonal statistics and point-based estimate

setwd("F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

# county names
all.counties = sort(unique(df.at$NAME))
n.ct = length(all.counties)

# loop through sum of DEM-based and unit-area estimates of flood storage volume
sum.cols.long = c("Biomass_Ctotal_Point","Biomass_Ctotal_Zonal")
sum.cols.short = c("tot_carbon_point","tot_carbon_zonal")
n.sum.cols = length(sum.cols.long)
tnw.county.df.all = data.frame(matrix(nrow=0, ncol=4))
unw.county.df.all = data.frame(matrix(nrow=0, ncol=4))
colnames(tnw.county.df.all) = c("Name","Water_Cutoff",sum.cols.short)
colnames(unw.county.df.all) = c("Name","Water_Cutoff",sum.cols.short)
for (j in 1:n.w) {
  county.df.tnw = data.frame(matrix(nrow=n.ct, ncol=4))
  county.df.unw = data.frame(matrix(nrow=n.ct, ncol=4))
  colnames(county.df.tnw) = c("Name","Water_Cutoff",sum.cols.short)
  colnames(county.df.unw) = c("Name","Water_Cutoff",sum.cols.short)
  county.df.tnw$Water_Cutoff = water.reg.labels[j]
  county.df.unw$Water_Cutoff = water.reg.labels[j]
  for (i in 1:n.ct) {
    cnty.i = all.counties[i]
    county.df.tnw[i,"Name"] = cnty.i
    county.df.unw[i,"Name"] = cnty.i
    county.rows.tnw = df.at[df.at$NAME == cnty.i,]
    county.rows.unw = df.at[df.at$NAME == cnty.i & df.at$Protection_Status == 0,]
    buf.ws.col = paste("Brinkerhoff_Intersect","1","1", sep="_")
    wrs.inds.tnw = !(county.rows.tnw$WATER_REGI %in% water.regimes[1:j])
    wrs.inds.unw = !(county.rows.unw$WATER_REGI %in% water.regimes[1:j])
    for (c in 1:n.sum.cols) {
      nonwotus.ind.tnw = which(wrs.inds.tnw | (county.rows.tnw$Within_Lev == 1 | county.rows.tnw[,buf.ws.col] == 0))
      nonwotus.ind.unw = which(wrs.inds.unw | (county.rows.unw$Within_Lev == 1 | county.rows.unw[,buf.ws.col] == 0))
      county.df.tnw[i, sum.cols.short[c]] = sum(county.rows.tnw[nonwotus.ind.tnw, sum.cols.long[c]])
      county.df.unw[i, sum.cols.short[c]] = sum(county.rows.unw[nonwotus.ind.unw, sum.cols.long[c]])
    }                              
  }
  tnw.county.df.all = rbind(tnw.county.df.all, county.df.tnw)
  unw.county.df.all = rbind(unw.county.df.all, county.df.unw)
}

# sum total DEM- and unit-area-based estimates for each county
county.sums = df.wr %>%
              group_by(NAME) %>% 
              summarize(county_carbon_zonal = sum(Biomass_Ctotal_Zonal),
                        county_carbon_point = sum(Biomass_Ctotal_Point))
colnames(county.sums)[1] = "Name"

# join estimates at semipermanently flooded cutoff
spf.df.tnw = tnw.county.df.all[tnw.county.df.all$Water_Cutoff == "Semipermanently Flooded",]
spf.df.unw = unw.county.df.all[unw.county.df.all$Water_Cutoff == "Semipermanently Flooded",]
colnames(spf.df.tnw) = c("Name","Water_Cutoff",paste("TNW", sum.cols.short, sep="_"))
colnames(spf.df.unw) = c("Name","Water_Cutoff",paste("UPNW", sum.cols.short, sep="_"))
cnty.carbon.df = left_join(spf.df.tnw[,c("Name",paste("TNW",sum.cols.short, sep="_"))], 
                           spf.df.unw[,c("Name",paste("UPNW",sum.cols.short, sep="_"))], 
                           by="Name")
cnty.carbon.df = left_join(cnty.carbon.df, county.sums, by="Name")

# calculate percentages at semipermanently flooded cutoff
cnty.carbon.df$TNW_per_carbon_zonal = cnty.carbon.df$TNW_tot_carbon_zonal / cnty.carbon.df$county_carbon_zonal * 100
cnty.carbon.df$TNW_per_carbon_point = cnty.carbon.df$TNW_tot_carbon_point / cnty.carbon.df$county_carbon_point * 100
cnty.carbon.df$UPNW_per_carbon_zonal = cnty.carbon.df$UPNW_tot_carbon_zonal / cnty.carbon.df$county_carbon_zonal * 100
cnty.carbon.df$UPNW_per_carbon_point = cnty.carbon.df$UPNW_tot_carbon_point/ cnty.carbon.df$county_carbon_point * 100

# normalize absolute columns to 1000's of tonnes
total.cols = c("TNW_tot_carbon_zonal","TNW_tot_carbon_point","UPNW_tot_carbon_zonal",
               "UPNW_tot_carbon_point","county_carbon_zonal","county_carbon_point")
cnty.carbon.df[,total.cols] = cnty.carbon.df[,total.cols]/1000 #(10^3 kg/1 tonne) * (10^3 g/1 kg) * (1 Mg/10^6 g) = Mg
write.csv(cnty.carbon.df, "F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/SPF_County_Carbon_Totals.csv",
          row.names = F)