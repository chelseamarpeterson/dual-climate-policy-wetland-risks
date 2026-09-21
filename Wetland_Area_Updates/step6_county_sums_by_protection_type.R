setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

################################################################################
# step 1: metadata

# conversion
AcPerHa = 2.47105

# nlcd years
years = c("2023","2024")
n.y = length(years)

# read in wetland tables
gap_wetland_dfs = list()
tot_wetland_dfs = list()
for (i in 1:n.y) {
  gap_wetland_dfs[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step13_",years[i],"NLCD_GAP_Union_CntyIntersect.csv",sep=""))
  tot_wetland_dfs[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step10_",years[i],"NLCD_WaterRegime_CntyIntersect.csv",sep=""))
}

# nhd versions
versions = c("nhd","brinkerhoff")
version.labs = c("NHD-based","Brinkerhoff-updated")
n.v = length(versions)

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed",
                  "Semipermanently Flooded","Seasonally Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded")
wr.abrevs = c("PF","IE","SPF","SF")
n.w = length(water.regimes)

# buffer distance scenarios
buf.dists = c(1,10)
n.b = length(buf.dists)

# flow permanence scenarios
perm.levels = seq(1,2)
n.p = length(perm.levels)

# counties with protection
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# all counties
all.counties = sort(unique(gap_wetland_dfs[[years[1]]]$NAME))
n.c = length(all.counties)

################################################################################
# step 2: estimate area with different types of protection

# sum wetland area in each protection status category by county and water cutoff
county.area.df = data.frame(matrix(nrow=n.y*n.v*n.c*n.w*n.p*n.b, ncol=18))
colnames(county.area.df) = c("year","version","NAME","water_cutoff","stream_perm","buf_dist","total_area",
                             "wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                             "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                             "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                             "wotus_county_ordinance_area","nonwotus_county_ordinance_area",
                             "total_nonwotus_behind_levee_area","total_nonwotus_isolated_area",
                             "total_nonwotus_too_dry_area")
n = 1
for (y in 1:n.y) {
  # read in wetland dataframe for corresponding year
  gap.df = gap_wetland_dfs[[years[y]]]
  tot.df = tot_wetland_dfs[[years[y]]]
  
  # created new protected status column
  gap.df$protected_status = "Locally unprotected"
  for (i in seq(1,2)) {gap.df$protected_status[which(gap.df$GAP_Sts == i)] = "Managed for biodiversity"}
  gap.df$protected_status[which((gap.df$NAME %in% pro.cnties) & !(gap.df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  gap.df$protected_status[which(!(gap.df$NAME %in% pro.cnties) & gap.df$GAP_Sts == 3)] = "Managed for multiple uses"
  gap.df$protected_status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 4))] = "Locally unprotected"
  
  # create column based on protection status
  gap.df$not_protected = 1*(gap.df$protected_status == "Locally unprotected")
  for (v in 1:n.v) {
    version = versions[v]
    for (i in 1:n.c) {
      gap.df.sub = gap.df[gap.df$NAME == all.counties[i],]
      tot.df.sub = tot.df[tot.df$NAME == all.counties[i],]
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
            wrs.inds = !(gap.df.sub$WATER_REGI %in% wrs)
            if (years[y] == "2023") {
              if (version == "nhd") {
                buf.ws.col = paste("Waters_Intersect", perm.levels[k], buf.dists[b], sep="_")
              } else {
                buf.ws.col = paste("Brinkerhoff_Intersect", perm.levels[k], buf.dists[b], sep="_")
              }
              non.jurisdictional.inds = (wrs.inds | (gap.df.sub$Within_Levee == 1 | gap.df.sub[,buf.ws.col] == 0))
            } else if (years[y] == "2024") {
              if (version == "nhd") {
                buf.ws.col = paste("NHDIn", perm.levels[k], "_", buf.dists[b], sep="")
              } else {
                buf.ws.col = paste("BRFIn", perm.levels[k], "_", buf.dists[b], sep="")
              }
              non.jurisdictional.inds = (wrs.inds | (gap.df.sub$Within_Lev == 1 | gap.df.sub[,buf.ws.col] == 0))
            }
            county.area.df[n,"total_area"] = sum(tot.df.sub[,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_locally_unprotected_area"] = sum(gap.df.sub[non.jurisdictional.inds == 0 & gap.df.sub$not_protected == 1,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_locally_unprotected_area"] = sum(gap.df.sub[non.jurisdictional.inds == 1 & gap.df.sub$not_protected == 1,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_managed_biodiversity_area"] = sum(gap.df.sub[non.jurisdictional.inds == 0 & gap.df.sub$protected_status == "Managed for biodiversity","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_managed_biodiversity_area"] = sum(gap.df.sub[non.jurisdictional.inds == 1 & gap.df.sub$protected_status == "Managed for biodiversity","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_managed_multuses_area"] = sum(gap.df.sub[non.jurisdictional.inds == 0 & gap.df.sub$protected_status == "Managed for multiple uses","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_managed_multuses_area"] = sum(gap.df.sub[non.jurisdictional.inds == 1 & gap.df.sub$protected_status == "Managed for multiple uses","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"wotus_county_ordinance_area"] = sum(gap.df.sub[non.jurisdictional.inds == 0 & gap.df.sub$protected_status == "County permitting and mitigation","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"nonwotus_county_ordinance_area"] = sum(gap.df.sub[non.jurisdictional.inds == 1 & gap.df.sub$protected_status == "County permitting and mitigation","Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"total_nonwotus_behind_levee_area"] = sum(gap.df.sub[gap.df.sub$Within_Levee == 1,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"total_nonwotus_isolated_area"] = sum(gap.df.sub[gap.df.sub[,buf.ws.col] == 0,"Polygon_Area_Ha_Geodesic"])
            county.area.df[n,"total_nonwotus_too_dry_area"] = sum(gap.df.sub[wrs.inds,"Polygon_Area_Ha_Geodesic"])
            n = n + 1
          }
        }
      }
    }
  }
}

# calculate total areas by protection category
area.cols.long = c("wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                   "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                   "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                   "wotus_county_ordinance_area","nonwotus_county_ordinance_area",
                   "total_nonwotus_behind_levee_area","total_nonwotus_isolated_area",
                   "total_nonwotus_too_dry_area")
area.cols.short = c("WOT_UP_AR","NWOT_UP_AR",
                    "WOT_MB_AR","NWOT_MB_AR",
                    "WOT_MU_AR","NWOT_MU_AR",
                    "WOT_CO_AR","NWOT_CO_AR",
                    "TNWOT_LEV","TNWOT_ISO",
                    "TNWOT_DRY")
n.ac = length(area.cols.long)
for (y in 1:n.y) {
  for (k in 1:n.ac) {
    # calculate mean, min, and max areas across scenarios for each county and water regime
    group.area.df = county.area.df[county.area.df$year == years[y],
                                   c("version","NAME","water_cutoff","stream_perm",
                                     "buf_dist", area.cols.long[k], "total_area")] 
    colnames(group.area.df)[6:7] = c("partial_area","total_area")
    area.stats = group.area.df %>% 
                 group_by(version, NAME, water_cutoff) %>%
                 summarize(mean = mean(partial_area),
                           min = min(partial_area),
                           max = max(partial_area))
    percent.stats = group.area.df %>% 
                    group_by(version, NAME, water_cutoff) %>%
                    summarize(mean = mean(partial_area/total_area*100),
                              min = min(partial_area/total_area*100),
                              max = max(partial_area/total_area*100))
    
    # reshape to get areas for each county in the columns
    for (i in 1:n.w) { area.stats$water_cutoff[area.stats$water_cutoff == water.regimes[i]] = wr.abrevs[i] }
    for (i in 1:n.w) { percent.stats$water_cutoff[percent.stats$water_cutoff == water.regimes[i]] = wr.abrevs[i] }
    
    area.wide.df = data.frame(matrix(nrow=n.c, ncol=0))
    percent.wide.df = data.frame(matrix(nrow=n.c, ncol=0))
    area.wide.df$NAME = all.counties
    percent.wide.df$NAME = all.counties
    for (i in 1:n.v) {
      v.i = versions[i]
      for (j in 1:n.w) {
        area.wr.rows.ij = subset(area.stats, water_cutoff == wr.abrevs[j] & version == v.i)
        percent.wr.rows.ij = subset(percent.stats, water_cutoff == wr.abrevs[j] & version == v.i)
        colnames(area.wr.rows.ij)[4:6] = paste(colnames(area.wr.rows.ij)[4:6], wr.abrevs[j], v.i, sep="_")
        colnames(percent.wr.rows.ij)[4:6] = paste(colnames(percent.wr.rows.ij)[4:6], wr.abrevs[j], v.i, sep="_")
        area.wide.df = left_join(area.wide.df, 
                                 area.wr.rows.ij[,c("NAME",colnames(area.wr.rows.ij)[4:6])], 
                                 by=c("NAME"))
        percent.wide.df = left_join(percent.wide.df, 
                                    percent.wr.rows.ij[,c("NAME",colnames(percent.wr.rows.ij)[4:6])], 
                                    by=c("NAME"))
      }
    }
    write.csv(area.wide.df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2",
                                  years[y],area.cols.long[k],"county_wetland_area_totals.csv",sep="_"), 
              row.names=F)
    write.csv(percent.wide.df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_percentages/step2",
                                  years[y],area.cols.long[k],"county_wetland_area_percents.csv",sep="_"), 
              row.names=F)
  }
}

# calculate absolute area differences for each scenario between Brinkerhoff + NLCD 2024 & NHD + NLCD 2023
stats = c("mean","min","max")
n.s = length(stats)
for (i in 1:n.ac) {
  diff.df = data.frame(matrix(nrow=n.c, ncol=0))
  diff.df$NAME = all.counties
  nhd_2023_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2023",
                               area.cols.long[i],"county_wetland_area_totals.csv",sep="_"))
  brf_2024_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2024",
                               area.cols.long[i],"county_wetland_area_totals.csv",sep="_"))
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
  write.csv(diff.df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_differences/step2",
                           area.cols.long[i],"county_wetland_area_differences.csv",sep="_"), row.names=F)
}

# calculate percent area differences for each scenario between Brinkerhoff + NLCD 2024 & NHD + NLCD 2023
for (i in 1:n.ac) {
  diff.df = data.frame(matrix(nrow=n.c, ncol=0))
  diff.df$NAME = all.counties
  nhd_2023_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_percentages/step2_2023",
                               area.cols.long[i],"county_wetland_area_percents.csv",sep="_"))
  brf_2024_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_percentages/step2_2024",
                               area.cols.long[i],"county_wetland_area_percents.csv",sep="_"))
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
  write.csv(diff.df, paste("Dual-Risk-Repo/County_Summaries/wetland_percentage_differences/step2",
                           area.cols.long[i],"county_wetland_percent_differences.csv",sep="_"), row.names=F)
}
