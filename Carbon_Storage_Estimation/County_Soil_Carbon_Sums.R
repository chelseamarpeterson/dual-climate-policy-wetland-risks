setwd("F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

library(tidyverse)

# read in csv file with biomass C stocks
df.wr = read.csv("IL_WS_Step20_WaterRegime_Filter_SOC_MOC_Database_Stocks.csv")
df.at = read.csv("IL_WS_Step21_AreaThreshold_Filter_SOC_MOC_Database_Stocks.csv")

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

# estimate POC
df.at$POC_Total = df.at$SOC_Total - df.at$MOC_Total
df.wr$POC_Total = df.wr$SOC_Total - df.wr$MOC_Total

# sum total and unprotected non-jurisdictional carbon storage
sum.cols = c("SOC_Total","MOC_Total","POC_Total")
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
nw.stats.df.soc = nw.sum.df %>%
                    group_by(water_cutoff) %>% 
                    summarize(mean_carbon = mean(SOC_Total),
                              min_carbon = min(SOC_Total),
                              max_carbon = max(SOC_Total))
nw.stats.df.soc$type = "Total Non-WOTUS"
nw.stats.df.soc$stock = "SOC"

nw.stats.df.poc = nw.sum.df %>%
                  group_by(water_cutoff) %>% 
                  summarize(mean_carbon = mean(POC_Total),
                            min_carbon = min(POC_Total),
                            max_carbon = max(POC_Total))
nw.stats.df.poc$type = "Total Non-WOTUS"
nw.stats.df.poc$stock = "POC"

nw.stats.df.moc = nw.sum.df %>%
                  group_by(water_cutoff) %>% 
                  summarize(mean_carbon = mean(MOC_Total),
                            min_carbon = min(MOC_Total),
                            max_carbon = max(MOC_Total))
nw.stats.df.moc$type = "Total Non-WOTUS"
nw.stats.df.moc$stock = "MOC"

nw.stats.df = rbind(nw.stats.df.soc, nw.stats.df.poc, nw.stats.df.moc)

# unprotected
up.nw.stats.df.soc = up.nw.sum.df %>%
                     group_by(water_cutoff) %>% 
                     summarize(mean_carbon = mean(SOC_Total),
                               min_carbon = min(SOC_Total),
                               max_carbon = max(SOC_Total))
up.nw.stats.df.soc$type = "Unprotected Non-WOTUS"
up.nw.stats.df.soc$stock = "SOC"

up.nw.stats.df.poc = up.nw.sum.df %>%
                     group_by(water_cutoff) %>% 
                     summarize(mean_carbon = mean(POC_Total),
                               min_carbon = min(POC_Total),
                               max_carbon = max(POC_Total))
up.nw.stats.df.poc$type = "Unprotected Non-WOTUS"
up.nw.stats.df.poc$stock = "POC"

up.nw.stats.df.moc = up.nw.sum.df %>%
                     group_by(water_cutoff) %>% 
                     summarize(mean_carbon = mean(MOC_Total),
                               min_carbon = min(MOC_Total),
                               max_carbon = max(MOC_Total))
up.nw.stats.df.moc$type = "Unprotected Non-WOTUS"
up.nw.stats.df.moc$stock = "MOC"

up.nw.stats.df = rbind(up.nw.stats.df.soc, up.nw.stats.df.poc, up.nw.stats.df.moc)
all.stats.df = rbind(nw.stats.df, up.nw.stats.df)

## compare absolute total and unprotected non-WOTUS wetland biomass carbon storage 

p.absolute = ggplot(all.stats.df,
                    aes(y=factor(water_cutoff, levels=water.reg.labels), 
                        x=mean_carbon/(10^6), 
                        group=type, color=type, linetype=type)) + 
                    geom_line() +
                    geom_ribbon(aes(xmin=min_carbon/(10^6), 
                                    xmax=max_carbon/(10^6), 
                                    group=type, color=type, linetype=type), alpha=0.2) +
                    labs(x="Total soil organic carbon storage (billions kg)",
                         y="Wetland Flood-Frequency Cutoff",
                         group="",color="",linetype="") +
                    scale_x_continuous(limits=c(0,50),breaks=seq(0,50,10)) + 
                    theme(text=element_text(size=14)) +
                    facet_wrap(.~stock)

setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/CarbonStorage/FigureC3_Soil_Carbon_Storage_Totals.jpeg", 
       plot=p.absolute, width=30, height=10, units="cm", dpi = 600)

## compare relative total and unprotected non-WOTUS wetland biomass carbon storage 
total.SOC = sum(df.wr$SOC_Total)
stats = c("mean_carbon","min_carbon","max_carbon")
all.percent.df = all.stats.df 
all.percent.df[,stats] = all.percent.df[,stats]/total.SOC*100

# zonal estimates
p.percent = ggplot(all.percent.df,
                   aes(y=factor(water_cutoff, levels=water.reg.labels), 
                       x=mean_carbon, 
                       group=type, color=type, linetype=type)) + 
                   geom_line() +
                   geom_ribbon(aes(xmin=min_carbon, 
                                   xmax=max_carbon, 
                                   group=type, color=type, linetype=type), alpha=0.2) +
                   labs(x="Percent of total SOC storage (%)",
                        y="Wetland Flood-Frequency Cutoff",
                        group="",color="",linetype="") +
                   scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25)) + 
                   theme(text=element_text(size=14)) +
                   facet_wrap(.~stock)
p.percent

setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/CarbonStorage/FigureC4_Soil_Carbon_Storage_Percents.jpeg", 
       plot=p.percent, width=30, height=10, units="cm", dpi = 600)

################################################################################
# sum up total, total non-WOTUS, unprotected non-WOTUS, and protected non-WOTUS 
# flood storage volume by county for both the zonal statistics and point-based estimate

setwd("F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

# county names
all.counties = sort(unique(df.at$NAME))
n.ct = length(all.counties)

# loop through sum of DEM-based and unit-area estimates of flood storage volume
sum.cols.long = c("SOC_Total","MOC_Total","POC_Total")
sum.cols.short = c("SOC_Total","MOC_Total","POC_Total")
n.sum.cols = length(sum.cols.long)
tnw.county.df.all = data.frame(matrix(nrow=0, ncol=(2+length(sum.cols.long))))
unw.county.df.all = data.frame(matrix(nrow=0, ncol=(2+length(sum.cols.long))))
colnames(tnw.county.df.all) = c("Name","Water_Cutoff",sum.cols.short)
colnames(unw.county.df.all) = c("Name","Water_Cutoff",sum.cols.short)
for (j in 1:n.w) {
  county.df.tnw = data.frame(matrix(nrow=n.ct, ncol=(2+length(sum.cols.long))))
  county.df.unw = data.frame(matrix(nrow=n.ct, ncol=(2+length(sum.cols.long))))
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
              summarize(county_SOC = sum(SOC_Total),
                        county_POC = sum(POC_Total),
                        county_MOC = sum(MOC_Total))
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
cnty.carbon.df$TNW_SOC_Percent = cnty.carbon.df$TNW_SOC_Total / cnty.carbon.df$county_SOC * 100
cnty.carbon.df$TNW_POC_Percent = cnty.carbon.df$TNW_POC_Total / cnty.carbon.df$county_POC * 100
cnty.carbon.df$TNW_MOC_Percent = cnty.carbon.df$TNW_MOC_Total / cnty.carbon.df$county_MOC * 100
cnty.carbon.df$UPNW_SOC_Percent = cnty.carbon.df$UPNW_SOC_Total / cnty.carbon.df$county_SOC * 100
cnty.carbon.df$UPNW_POC_Percent = cnty.carbon.df$UPNW_POC_Total / cnty.carbon.df$county_POC * 100
cnty.carbon.df$UPNW_MOC_Percent = cnty.carbon.df$UPNW_MOC_Total / cnty.carbon.df$county_MOC * 100

# normalize absolute columns to 1000's of tonnes
total.cols = c("TNW_SOC_Total","TNW_POC_Total","TNW_MOC_Total",
               "UPNW_SOC_Total","UPNW_POC_Total","UPNW_MOC_Total",
               "county_SOC","county_POC","county_MOC")
cnty.carbon.df[,total.cols] = cnty.carbon.df[,total.cols]/(10^3)
write.csv(cnty.carbon.df, "F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/SPF_County_Soil_Organic_Carbon_Totals.csv",
          row.names = F)
