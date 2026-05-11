
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

################################################################################
# step 1: metadata

# conversion
AcPerHa = 2.47105

# read in wetland table
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project")

# check area sums
#sum(ws.df$Polygon_Area_Ha_Geodesic)
#sum(ws.df$Polygon_Area_Ac_Geodesic)
#sum(ws.df$Polygon_Area_Ha_Planar)
#sum(ws.df$Polygon_Area_Ac_Planar)

# nlcd years
years = c("2023","2024")
n.y = length(years)

# nhd versions
versions = c("nhd","brinkerhoff")
version.labs = c("NHD-based","Brinkerhoff-updated")
n.v = length(versions)

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded/Saturated","Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded/Saturated","Seasonally Flooded")
n.w = length(water.regimes)

# buffer distance scenarios
buf.dists = c(1,10)
n.b = length(buf.dists)

# flow permanence scenarios
perm.levels = seq(1,2)
n.p = length(perm.levels)

# all counties
all.counties = sort(unique(ws.df$NAME))
n.c = length(all.counties)

# counties with protection
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# protected status categories
pro.cats = sort(unique(ws.df$protected_status))
n.cats = length(pro.cats)

################################################################################
# step 2: estimate area with different types of protection

# sum wetland area in each protection status category by county and water cutoff
county.area.df = data.frame(matrix(nrow=n.y*n.v*n.c*n.w*n.p*n.b, ncol=14))
colnames(county.area.df) = c("year","version","NAME","water_cutoff","stream_perm","buf_dist",
                             "wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                             "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                             "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                             "wotus_county_ordinance_area","nonwotus_county_ordinance_area")
n = 1
for (y in 1:n.y) {
  # read in wetland dataframe for corresponding year
  ws.df = read.csv(paste("NWI_Wetlands/IL_WS_Step13_",years[y],"NLCD_GAP_Union_CntyIntersect.csv",sep=""))
  
  # created new protected status column
  ws.df$protected_status = rep("Unprotected", nrow(ws.df))
  for (i in seq(1,2)) {ws.df$protected_status[which(ws.df$GAP_Sts == i)] = "Managed for biodiversity"}
  ws.df$protected_status[which((ws.df$NAME %in% pro.cnties) & !(ws.df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  ws.df$protected_status[which(!(ws.df$NAME %in% pro.cnties) & ws.df$GAP_Sts == 3)] = "Managed for multiple uses"
  ws.df$protected_status[which(!(ws.df$NAME %in% pro.cnties) & (ws.df$GAP_Sts == 4))] = "Locally unprotected"
  
  # create column based on protection status
  ws.df$not_protected = 1*(ws.df$protected_status == "Unprotected")
  for (v in 1:n.v) {
    version = versions[v]
    for (i in 1:n.c) {
      cnty.df.sub = ws.df[ws.df$NAME == all.counties[i],]
      for (j in 1:n.w) {
        for (k in 1:n.p) {
          for (b in 1:n.b) {
            county.area.df[n,"year"] = years[y]
            county.area.df[n,"version"] = version
            county.area.df[n,"NAME"] = all.counties[i]
            county.area.df[n,"water_cutoff"] = water.regimes[j]
            county.area.df[n,"stream_perm"] = perm.levels[k]
            county.area.df[n,"buf_dist"] = buf.dists[b]
            wrs = water.regimes[1:j]
            wrs.inds = !(cnty.df.sub$WATER_REGI %in% wrs)
            if (years[y] == "2023") {
              if (version == "nhd") {
                buf.ws.col = paste("Waters_Intersect", perm.levels[k], buf.dists[b], sep="_")
              } else {
                buf.ws.col = paste("Brinkerhoff_Intersect", perm.levels[k], buf.dists[b], sep="_")
              }
              non.jurisdictional.inds = (wrs.inds | (cnty.df.sub$Within_Levee == 1 | cnty.df.sub[,buf.ws.col] == 0))
            } else if (years[y] == "2024") {
              if (version == "nhd") {
                buf.ws.col = paste("NHDIn", perm.levels[k], "_", buf.dists[b], sep="")
              } else {
                buf.ws.col = paste("BRFIn", perm.levels[k], "_", buf.dists[b], sep="")
              }
              non.jurisdictional.inds = (wrs.inds | (cnty.df.sub$Within_Lev == 1 | cnty.df.sub[,buf.ws.col] == 0))
            }
            county.area.df[n,"wotus_locally_unprotected_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 0 & cnty.df.sub$not_protected == 1,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_locally_unprotected_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 1 & cnty.df.sub$not_protected == 1,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_managed_biodiversity_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 0 & cnty.df.sub$protected_status == "Managed for biodiversity","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_managed_biodiversity_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 1 & cnty.df.sub$protected_status == "Managed for biodiversity","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_managed_multuses_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 0 & cnty.df.sub$protected_status == "Managed for multiple uses","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_managed_multuses_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 1 & cnty.df.sub$protected_status == "Managed for multiple uses","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_county_ordinance_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 0 & cnty.df.sub$protected_status == "County permitting and mitigation","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_county_ordinance_area"] = sum(cnty.df.sub[non.jurisdictional.inds == 1 & cnty.df.sub$protected_status == "County permitting and mitigation","Polygon_Area_Ha_Geodesic"])
            n = n + 1
          }
        }
      }
    }
  }
}

# calculate statistics
protection_groups = c("wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                      "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                      "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                      "wotus_county_ordinance_area","nonwotus_county_ordinance_area")
area_cols = c("WOT_UP_AR","NWOT_UP_AR",
              "WOT_MB_AR","NWOT_MB_AR",
              "WOT_MU_AR","NWOT_MU_AR",
              "WOT_CO_AR","NWOT_CO_AR")
n.g = length(protection_groups)
for (y in 1:n.y) {
  for (k in 1:n.g) {
    # calculate mean, min, and max areas across scenarios for each county and water regime
    group.area.df = county.area.df[county.area.df$year == years[y],c("version","NAME","water_cutoff","stream_perm","buf_dist", protection_groups[k])] 
    colnames(group.area.df)[6] = "area"
    area.stats.sum = group.area.df %>% 
                     group_by(version, NAME, water_cutoff) %>%
                     summarize(mean = mean(area),
                               min = min(area),
                               max = max(area))
    
    # reshape to get areas for each county in the columns
    wr.abrevs = c("PF","IE","SPF","SFS","SF")
    for (i in 1:n.w) { area.stats.sum$water_cutoff[area.stats.sum$water_cutoff == water.regimes[i]] = wr.abrevs[i] }
    area.wide.df = data.frame(matrix(nrow=n.c, ncol=0))
    area.wide.df$NAME = all.counties
    for (i in 1:n.v) {
      v = versions[i]
      for (j in 1:n.w) {
        wr.rows = subset(area.stats.sum, water_cutoff == wr.abrevs[j] & area.stats.sum$version == v)
        colnames(wr.rows)[4:6] = paste(colnames(wr.rows)[4:6],wr.abrevs[j],v,sep="_")
        area.wide.df = left_join(area.wide.df, 
                                 wr.rows[,c("NAME",colnames(wr.rows)[4:6])], 
                                 by=c("NAME"))
      }
    }
  write.csv(area.wide.df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2",years[y],protection_groups[k],"county_wetland_area_totals.csv",sep="_"), 
            row.names=F)
  }
}

# calculate differences for each scenario between Brinkerhoff + NLCD 2024 & NHD + NLCD 2023
stats = c("mean","min","max")
n.s = length(stats)
for (i in 1:n.g) {
  diff.df = data.frame(matrix(nrow=n.c, ncol=0))
  diff.df$NAME = all.counties
  nhd_2023_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2023",protection_groups[i],"county_wetland_area_totals.csv",sep="_"))
  brf_2024_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2024",protection_groups[i],"county_wetland_area_totals.csv",sep="_"))
  for (j in 1:n.s) {
    for (k in 1:n.w) {
      diff_col = paste(stats[j], wr.abrevs[k], "diff", sep="_")
      new.col = data.frame(matrix(nrow=n.c, ncol=1))
      colnames(new.col) = c(diff_col)
      new.col$NAME = all.counties
      nhd_2023_col = paste(stats[j], wr.abrevs[k], "nhd", sep="_")
      brf_2024_col = paste(stats[j], wr.abrevs[k], "brinkerhoff", sep="_")
      new.col[,diff_col] = brf_2024_df[,brf_2024_col] - nhd_2023_df[,nhd_2023_col]
      diff.df = left_join(diff.df, new.col, by="NAME")
    }
  }
  write.csv(diff.df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_differences/step2",protection_groups[i],"county_wetland_area_differences.csv",sep="_"), row.names=F)
}