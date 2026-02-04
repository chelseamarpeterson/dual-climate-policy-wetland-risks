setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/NWI_Wetlands")

library(ggplot2)
library(dplyr)
library(patchwork)

# read in NWI polygon file before small (<0.1 ac) wetlands were removed
nwi.df.all = read.csv("IL_WS_Step10_WaterRegime.csv")
total.state.wetland.area = sum(nwi.df.all$Polygon_Area_Ha_Geodesic)

# read in NWI polygon file intersected with GAP and county layers
nwi.df = read.csv("IL_WS_Step11_AreaThreshold.csv")
  
# water regimes / Wetland Flood-Frequency Cutoffs
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                  "Seasonally Flooded/Saturated","Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                     "Seasonally Flooded/Saturated","Seasonally Flooded")
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
area.df = data.frame(matrix(nrow=n.w*n.p*n.b*n.v, ncol=5))
colnames(area.df) = c("water_cutoff","perm_level","buf_dist","version","area")
n = 1
for (i in 1:n.w) {
  for (j in 1:n.p) {
    for (k in 1:n.b) {
      for (l in 1:n.v) {
        # assign policy scenario labels
        area.df[n,"water_cutoff"] = water.regimes[i]
        area.df[n,"perm_level"] = perm.levels[j]
        area.df[n,"buf_dist"] = buf.dists[k]
        area.df[n,"version"] = version.labs[l]
        
        # create column for flow permanence and buffer distance combination
        if (versions[l] == "nhd") {
          buf.ws.col = paste("Waters_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
        } else if (versions[l] == "brinkerhoff") {
          buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
        }
        
        # get all water regimes up to the ith and then identify rows with insufficient flooding
        wrs.inds = !(nwi.df$WATER_REGI %in% water.regimes[1:i])
        
        # identify all wetland polygons that (1) don't meet the flood frequency cutoff,
        # (2) occur within a leveed area, or (3) don't intersect the WOTUS buffer
        area.df[n,"area"] = sum(nwi.df[which(wrs.inds | (nwi.df$Within_Levee == 1 | nwi.df[,buf.ws.col] == 0)),"Polygon_Area_Ha_Geodesic"])
        
        # increment counter
        n = n + 1
      }
    }
  }
}

# estimate cumulative total non-WOTUS area at each wetland flood-frequency cutoff
area.stats.df = area.df %>%
                group_by(water_cutoff, version) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))
percent.stats.df = area.stats.df
percent.stats.df[,c("mean","min","max")] = percent.stats.df[,c("mean","min","max")]/total.state.wetland.area*100

# print area stats for table
nhd.areas = area.stats.df[which(area.stats.df$version == "NHD-based"),]
brf.areas = area.stats.df[which(area.stats.df$version == "Brinkerhoff-updated"),]
wr.ind = rep(0,n.w)
for (i in 1:n.w) { wr.ind[i] = which(nhd.areas$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(nhd.areas[wr.ind,c("mean","min","max")]),1)
round(data.frame(brf.areas[wr.ind,c("mean","min","max")]),1)

# print percent stats for table
nhd.percents = percent.stats.df[which(percent.stats.df$version == "NHD-based"),]
brf.percents = percent.stats.df[which(percent.stats.df$version == "Brinkerhoff-updated"),]
wr.ind = rep(0,n.w)
for (i in 1:n.w) { wr.ind[i] = which(nhd.percents$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(nhd.percents[wr.ind,c("mean","min","max")]),1)
round(data.frame(brf.percents[wr.ind,c("mean","min","max")]),1)

# plot area comparison between each NHD version
p1 = ggplot(area.stats.df, 
            aes(x=mean, 
                y=factor(water_cutoff, levels=water.regimes),
                group=factor(version, levels=version.labs),
                color=factor(version, levels=version.labs),
                linetype=factor(version, levels=version.labs))) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data = area.stats.df,
                        aes(xmin=min, xmax=max, 
                            group=factor(version, levels=version.labs),
                            color=factor(version, levels=version.labs),
                            linetype=factor(version, levels=version.labs)), 
                        alpha=0.1,linewidth=1) +
            scale_x_continuous(limits=c(250000,400000), labels=scales::comma) +
            labs(y="Wetland Flood-Frequency Cutoff",
                 x="Total non-WOTUS wetland area (ha)",
                 group="",color="",linetype="") + 
            theme(legend.position="none",
                  text=element_text(size=15))
p1
p2 = ggplot(percent.stats.df, 
            aes(x=mean, 
                y=factor(water_cutoff, levels=water.regimes),
                group=factor(version, levels=version.labs),
                color=factor(version, levels=version.labs),
                linetype=factor(version, levels=version.labs))) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data = percent.stats.df,
                        aes(xmin=min, xmax=max, 
                            group=factor(version, levels=version.labs),
                            color=factor(version, levels=version.labs),
                            linetype=factor(version, levels=version.labs)), 
                        alpha=0.1,linewidth=1) +
            scale_x_continuous(limits=c(60,100), labels=scales::comma) +
            labs(y="",x="Total non-WOTUS wetland percent (%)",
                 group="",color="",linetype="") + 
            theme(axis.text.y=element_blank(),
                  text=element_text(size=15))
p2
p3 = p1 + p2
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/FigureA2_NHD_Brinkerhoff_NonWOTUS_Comparison.jpeg", 
       plot=p3, width=38, height=14, units="cm", dpi=600)

################################################################################
# unprotected non-WOTUS area comparison
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/NWI_Wetlands")

# read in NWI files intersected with county and GAP layers
gap.df = read.csv("IL_WS_Step12_GAP_Union_CntyIntersect.csv")

# specify counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# create column that combines GAP and county information
gap.df$Protected_Status = rep("Unprotected", nrow(gap.df))
for (i in seq(1,2)) {gap.df$Protected_Status[which(gap.df$GAP_Sts == i)] = "Managed for biodiversity"}
gap.df$Protected_Status[which((gap.df$NAME %in% pro.cnties) & !(gap.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 3))] = "Managed for multiple uses"
gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 4))] = "Unprotected"

# sum unprotected non-WOTUS area in each gap category
unpro.area.df = data.frame(matrix(nrow=n.w*n.p*n.b*n.v, ncol=5))
colnames(unpro.area.df) = c("water_cutoff","perm_level","buf_dist","version","area")
n = 1
for (i in 1:n.w) {
  for (j in 1:n.p) {
    for (k in 1:n.b) {
      for (l in 1:n.v) {
        gap.df.sub = gap.df[which(gap.df$Protected_Status == "Unprotected"),]
        unpro.area.df[n,"water_cutoff"] = water.regimes[i]
        unpro.area.df[n,"perm_level"] = perm.levels[j]
        unpro.area.df[n,"buf_dist"] = buf.dists[k]
        unpro.area.df[n,"version"] = version.labs[l]
        if (versions[l] == "nhd") {
          buf.ws.col = paste("Waters_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
        } else if (versions[l] == "brinkerhoff") {
          buf.ws.col = paste("Brinkerhoff_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
        }
        wrs.inds = !(gap.df.sub$WATER_REGI %in% water.regimes[1:i])
        unpro.area.df[n,"area"] = sum(gap.df.sub[which(wrs.inds | (gap.df.sub$Within_Levee == 1 | gap.df.sub[,buf.ws.col] == 0)),"Area_Ha"])
        n = n + 1 
      }
    }
  }
}

# estimate cumulative unprotected non-WOTUS area at each wetland flood-frequency cutoff
unpro.area.stats.df = unpro.area.df %>%
                      group_by(water_cutoff, version) %>% 
                      summarize(mean = mean(area),
                                min = min(area),
                                max = max(area))
unpro.percent.stats.df = unpro.area.stats.df
unpro.percent.stats.df[,c("mean","min","max")] = unpro.percent.stats.df[,c("mean","min","max")]/total.state.wetland.area*100

# print area stats for table
unpro.nhd.areas = unpro.area.stats.df[which(unpro.area.stats.df$version == "NHD-based"),]
unpro.brf.areas = unpro.area.stats.df[which(unpro.area.stats.df$version == "Brinkerhoff-updated"),]
wr.ind = rep(0,n.w)
for (i in 1:n.w) { wr.ind[i] = which(unpro.nhd.areas$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.nhd.areas[wr.ind,c("mean","min","max")]),1)
round(data.frame(unpro.brf.areas[wr.ind,c("mean","min","max")]),1)

# print percent stats for table
unpro.nhd.percents = unpro.percent.stats.df[which(unpro.percent.stats.df$version == "NHD-based"),]
unpro.brf.percents = unpro.percent.stats.df[which(unpro.percent.stats.df$version == "Brinkerhoff-updated"),]
wr.ind = rep(0,n.w)
for (i in 1:n.w) { wr.ind[i] = which(unpro.nhd.percents$water_cutoff == rev(water.regimes)[i]) }
round(data.frame(unpro.nhd.percents[wr.ind,c("mean","min","max")]),1)
round(data.frame(unpro.brf.percents[wr.ind,c("mean","min","max")]),1)

# plot area comparison between each NHD version
p1 = ggplot(unpro.area.stats.df, 
            aes(x=mean, 
                y=factor(water_cutoff, levels=water.regimes),
                group=factor(version, levels=version.labs),
                color=factor(version, levels=version.labs),
                linetype=factor(version, levels=version.labs))) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data=unpro.area.stats.df,
                        aes(xmin=min, xmax=max, 
                            group=factor(version, levels=version.labs),
                            color=factor(version, levels=version.labs),
                            linetype=factor(version, levels=version.labs)), 
                        alpha=0.1,linewidth=1) +
            scale_x_continuous(limits=c(200000,300000), labels=scales::comma) +
            labs(y="Wetland Flood-Frequency Cutoff",
                 x="Unprotected non-WOTUS wetland area (ha)",
                 group="",color="",linetype="") + 
            theme(legend.position="none",
                  text=element_text(size=15))
p2 = ggplot(unpro.percent.stats.df, 
            aes(x=mean, 
                y=factor(water_cutoff, levels=water.regimes),
                group=factor(version, levels=version.labs),
                color=factor(version, levels=version.labs),
                linetype=factor(version, levels=version.labs))) + 
            geom_line(linewidth=1) + 
            geom_ribbon(data=unpro.percent.stats.df,
                        aes(xmin=min, xmax=max, 
                            group=factor(version, levels=version.labs),
                            color=factor(version, levels=version.labs),
                            linetype=factor(version, levels=version.labs)), 
                        alpha=0.1,linewidth=1) +
            scale_x_continuous(limits=c(50,80), labels=scales::comma) +
            labs(y="",x="Unprotected non-WOTUS wetland percent (%)",
                 group="",color="",linetype="") + 
            theme(axis.text.y=element_blank(),
                  text=element_text(size=15))
p3 = p1 + p2
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Manuscript")
ggsave("Supp_Figures/FigureA3_NHD_Brinkerhoff_UnprotectedNonWOTUS_Comparison.jpeg", 
       plot=p3, width=38, height=14, units="cm", dpi = 600)
