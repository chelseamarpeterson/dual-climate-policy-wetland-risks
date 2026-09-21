setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_Dual_Wetland_Risk")

library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
library(RColorBrewer)
library(extrafont)
extrafont::loadfonts(device = "win")

# NLCD years
years = c("2023","2024")
n.y = length(years)

# read in NWI polygon files
wr_list = list()
at_list = list()
state_wetland_areas = list()
for (i in 1:n.y) {
  wr_list[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step10_",years[i],"NLCD_WaterRegime.csv",sep=""))
  state_wetland_areas[[years[i]]] = sum(wr_list[[years[i]]]$Polygon_Area_Ha_Geodesic)
  at_list[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step11_",years[i],"NLCD_AreaThreshold.csv",sep=""))
}

# water regimes / Wetland Flood-Frequency Cutoffs (dropped "Seasonally Flooded/Saturated")
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.regimes)

# stream flow permanence criteria
perm.levels = c("Perennial","Intermittent")
perm.abrvs = c("1","2")
n.p = length(perm.abrvs)

# make vectors for buffer scenarios
buf.dists = c("1","10")
buf.cols = paste("buf", buf.dists, sep="")
n.b = length(buf.dists)

# nhd versions
versions = c("nhd","brinkerhoff")
version.labs = c("NHD-based","Brinkerhoff-updated")
n.v = length(versions)

################################################################################
# total non-WOTUS area comparison

#  create data frame with to estimate non-WOTUS areas
area.df = data.frame(matrix(nrow=n.y*n.w*n.p*n.b*n.v, ncol=6))
colnames(area.df) = c("year","water_cutoff","perm_level","buf_dist","version","area")
n = 1
for (h in 1:n.y) {
  nwi.df = at_list[[years[h]]]
  for (i in 1:n.w) {
    for (j in 1:n.p) {
      for (k in 1:n.b) {
        for (l in 1:n.v) {
          # assign policy scenario labels
          area.df[n,"year"] = years[h]
          area.df[n,"water_cutoff"] = water.regimes[i]
          area.df[n,"perm_level"] = perm.levels[j]
          area.df[n,"buf_dist"] = buf.dists[k]
          area.df[n,"version"] = version.labs[l]

          # get all water regimes up to the ith and then identify rows with insufficient flooding
          wrs.inds = !(nwi.df$WATER_REGI %in% water.regimes[1:i])
          
          # identify all wetland polygons that (1) don't meet the flood frequency cutoff,
          # (2) occur within a leveed area, or (3) don't intersect the WOTUS buffer
          if (years[h] == "2023") {
            if (versions[l] == "nhd") {
              buf.ws.col = paste("Waters_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
            } else if (versions[l] == "brinkerhoff") {
              buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
            }
            area.df[n,"area"] = sum(nwi.df[which(wrs.inds | (nwi.df$Within_Levee == 1 | nwi.df[,buf.ws.col] == 0)),"Polygon_Area_Ha_Geodesic"])
          } else if (years[h] == "2024") {
            if (versions[l] == "nhd") {
              buf.ws.col = paste("NHDIn", perm.abrvs[j], "_", buf.dists[k], sep="")
            } else if (versions[l] == "brinkerhoff") {
              buf.ws.col = paste("BRFIn", perm.abrvs[j], "_", buf.dists[k], sep="")
            }
            area.df[n,"area"] = sum(nwi.df[which(wrs.inds | (nwi.df$Within_Lev == 1 | nwi.df[,buf.ws.col] == 0)),"Polygon_Area_Ha_Geodesic"])
          }
          
          # increment counter
          n = n + 1
        }
      }
    }
  }
}

# estimate cumulative total non-WOTUS area at each wetland flood-frequency cutoff
area.stats.df = area.df %>%
                group_by(year, water_cutoff, version) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))
percent.stats.df = area.stats.df
for (i in 1:n.y) {
  percent.stats.df[percent.stats.df$year == years[i],c("mean","min","max")] = percent.stats.df[percent.stats.df$year == years[i],c("mean","min","max")]/state_wetland_areas[[years[2]]]*100
}

# print total non-WOTUS area stats for table
areas.2023 = area.stats.df[area.stats.df$year == "2023" & area.stats.df$version == "NHD-based",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(areas.2023$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(areas.2023[wr.ind, c("mean","min","max")]),1)

areas.2024 = area.stats.df[area.stats.df$year == "2024" & area.stats.df$version == "Brinkerhoff-updated",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(areas.2024$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(areas.2024[wr.ind, c("mean","min","max")]),1)
round(data.frame(areas.2024[wr.ind, c("mean","max","min")]) - data.frame(areas.2023[wr.ind, c("mean","max","min")]),1)

# print non-WOTUS percent stats for table
percents.2023 = percent.stats.df[percent.stats.df$year == "2023" & percent.stats.df$version == "NHD-based",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(percents.2023$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(percents.2023[wr.ind, c("mean","min","max")]),1)

percents.2024 = percent.stats.df[percent.stats.df$year == "2024" & percent.stats.df$version == "Brinkerhoff-updated",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(percents.2024$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(percents.2024[wr.ind, c("mean","min","max")]),1)
round(data.frame(percents.2024[wr.ind, c("mean","max","min")]) - data.frame(percents.2023[wr.ind, c("mean","max","min")]),1)

# plot area comparison between each NHD version
plot.total.areas = rbind(areas.2023, areas.2024)
plot.total.areas$version[plot.total.areas$version == "Brinkerhoff-updated"] = "2024 NLCD x Brinkerhoff"
plot.total.areas$version[plot.total.areas$version == "NHD-based"] = "2023 NLCD x NHD"
p1 = ggplot(plot.total.areas, 
            aes(x=mean/1000, y=factor(water_cutoff, levels=water.regimes),
                group=version, color=version, linetype=version)) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data = plot.total.areas,
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

# specify counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

gap_list = list()
for (i in 1:n.y) {
  # read in NWI files intersected with county and GAP layers
  gap.df = read.csv(paste("NWI_Wetlands/IL_WS_Step13_",years[i],"NLCD_GAP_Union_CntyIntersect.csv", sep=""))
  
  # create column that combines GAP and county information
  gap.df$Protected_Status = "Locally unprotected"
  for (j in seq(1,2)) {gap.df$Protected_Status[which(gap.df$GAP_Sts == j)] = "Managed for biodiversity"}
  gap.df$Protected_Status[which((gap.df$NAME %in% pro.cnties) & !(gap.df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 3))] = "Managed for multiple uses"
  gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 4))] = "Locally unprotected"
  
  # add to list
  gap_list[[years[i]]] = gap.df
}

# sum unprotected non-WOTUS area for each policy scenario and NHD version
unpro.area.df = data.frame(matrix(nrow=n.y*n.w*n.p*n.b*n.v, ncol=6))
colnames(unpro.area.df) = c("year","water_cutoff","perm_level","buf_dist","version","area")
n = 1
for (h in 1:n.y) {
  gap.df = gap_list[[years[h]]]
  gap.df.sub = gap.df[gap.df$Protected_Status == "Locally unprotected",]
  for (i in 1:n.w) {
    for (j in 1:n.p) {
      for (k in 1:n.b) {
        for (l in 1:n.v) {
          unpro.area.df[n,"year"] = years[h]
          unpro.area.df[n,"water_cutoff"] = water.regimes[i]
          unpro.area.df[n,"perm_level"] = perm.levels[j]
          unpro.area.df[n,"buf_dist"] = buf.dists[k]
          unpro.area.df[n,"version"] = version.labs[l]
          wrs.inds = !(gap.df.sub$WATER_REGI %in% water.regimes[1:i])
          if (years[h] == "2023") {
            if (versions[l] == "nhd") {
              buf.ws.col = paste("Waters_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
            } else if (versions[l] == "brinkerhoff") {
              buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
            }
            unpro.area.df[n,"area"] = sum(gap.df.sub[which(wrs.inds | (gap.df.sub$Within_Levee == 1 | gap.df.sub[,buf.ws.col] == 0)),"Polygon_Area_Ha_Geodesic"])
          } else if (years[h] == "2024") {
            if (versions[l] == "nhd") {
              buf.ws.col = paste("NHDIn", perm.abrvs[j], "_", buf.dists[k], sep="")
            } else if (versions[l] == "brinkerhoff") {
              buf.ws.col = paste("BRFIn", perm.abrvs[j], "_", buf.dists[k], sep="")
            }
            unpro.area.df[n,"area"] = sum(gap.df.sub[which(wrs.inds | (gap.df.sub$Within_Lev == 1 | gap.df.sub[,buf.ws.col] == 0)),"Polygon_Area_Ha_Geodesic"])
          }
          n = n + 1 
        }
      }
    }
  }
}

# estimate cumulative unprotected non-WOTUS area at each wetland flood-frequency cutoff
unpro.area.stats.df = unpro.area.df %>%
                      group_by(year, water_cutoff, version) %>% 
                      summarize(mean = mean(area),
                                min = min(area),
                                max = max(area))
unpro.percent.stats.df = unpro.area.stats.df
for (i in 1:n.y) {
  unpro.percent.stats.df[unpro.percent.stats.df$year == years[i],c("mean","min","max")] = unpro.percent.stats.df[unpro.percent.stats.df$year == years[i],c("mean","min","max")]/state_wetland_areas[[years[2]]]*100
}

# print area stats for table
unpro.areas.2023 = unpro.area.stats.df[unpro.area.stats.df$year == "2023" & unpro.area.stats.df$version == "NHD-based",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(unpro.areas.2023$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.areas.2023[wr.ind, c("mean","min","max")]),1)

unpro.areas.2024 = unpro.area.stats.df[unpro.area.stats.df$year == "2024" & unpro.area.stats.df$version == "Brinkerhoff-updated",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(unpro.areas.2024$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.areas.2024[wr.ind, c("mean","min","max")]),1)
round(data.frame(unpro.areas.2024[wr.ind, c("mean","max","min")]) - data.frame(unpro.areas.2023[wr.ind, c("mean","max","min")]),1)

# print percent stats for table
unpro.percents.2023 = unpro.percent.stats.df[unpro.percent.stats.df$year == "2023" & unpro.percent.stats.df$version == "NHD-based",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(unpro.percents.2023$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.percents.2023[wr.ind, c("mean","min","max")]),1)

unpro.percents.2024 = unpro.percent.stats.df[unpro.percent.stats.df$year == "2024" & unpro.percent.stats.df$version == "Brinkerhoff-updated",]
wr.ind = 0
for (i in 1:n.w) { wr.ind[i] = which(unpro.percents.2024$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.percents.2024[wr.ind, c("mean","min","max")]),1)
round(data.frame(unpro.percents.2024[wr.ind, c("mean","max","min")]) - data.frame(unpro.percents.2023[wr.ind, c("mean","max","min")]),1)

# plot area comparison between each NHD version
plot.unpro.areas = rbind(unpro.areas.2023, unpro.areas.2024)
plot.unpro.areas$version[plot.unpro.areas$version == "Brinkerhoff-updated"] = "2024 NLCD x Brinkerhoff"
plot.unpro.areas$version[plot.unpro.areas$version == "NHD-based"] = "2023 NLCD x NHD"
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
p2 = ggplot(plot.unpro.areas, 
            aes(x=mean/1000, y=factor(water_cutoff, levels=water.regimes),
                group=version, color=version, linetype=version)) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data=plot.unpro.areas,
                        aes(xmin=min/1000, xmax=max/1000,
                            y=factor(water_cutoff, levels=water.regimes),
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
ggsave("Manuscript/Supp_Figures/AppendixA/FigureA2_Total_And_Unprotected_NonWOTUS_Area.jpeg", 
       plot=p3, width=30, height=9, units="cm", dpi=1000)

