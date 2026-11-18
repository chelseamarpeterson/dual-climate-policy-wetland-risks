setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Dissertation/Ch5_Dual_Wetland_Risk")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

################################################################################
# metadata

# conversion
ac_per_ha = 2.47105

# nlcd years
years = c("2023","2024")
n_y = length(years)

# read in wetland tables
gap_wetland_dfs = list()
tot_wetland_dfs = list()
for (i in 1:n_y) {
  gap_wetland_dfs[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step13_",years[i],
                                               "NLCD_GAP_Union_CntyIntersect.csv",sep=""))
  tot_wetland_dfs[[years[i]]] = read.csv(paste("NWI_Wetlands/IL_WS_Step10_",years[i],
                                               "NLCD_WaterRegime_CntyIntersect.csv",sep=""))
}

# nhd versions
versions = c("nhd","brinkerhoff")
version_labels = c("NHD-based","Brinkerhoff-updated")
n_v = length(versions)

# water regimes
water_regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
water_regime_labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
wr_abrevs = c("PF","IE","SPF","SF")
n_w = length(water_regimes)

# buffer distance scenarios
buf_dists = c(1,5,10,20,100)
n_bd = length(buf_dists)

# buffer types
buf_type = c("uniform","variable")
n_bt = length(buf_type)

# flow permanence scenarios
perm_levels = seq(1,3)
n_p = length(perm_levels)

# counties with protection
pro_cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n_cp = length(pro_cnties)

# all counties
all_counties = sort(unique(gap_wetland_dfs[[years[1]]]$NAME))
n_c = length(all_counties)

################################################################################
# estimate area with different types of protection

# sum wetland area in each protection status category by county and water cutoff
county_area_df_uniform = data.frame(matrix(nrow=n_y*n_v*n_w*n_bd*n_p, ncol=19))
colnames(county_area_df_uniform) = c("year","version","NAME","water_cutoff",
                                     "stream_perm","buf_dist","buf_type","total_area",
                                     "wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                                     "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                                     "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                                     "wotus_county_ordinance_area","nonwotus_county_ordinance_area",
                                     "total_nonwotus_behind_levee_area","total_nonwotus_isolated_area",
                                     "total_nonwotus_too_dry_area")
n = 1
for (y in 1:n_y) {
  # read in wetland dataframe for corresponding year
  gap_df = gap_wetland_dfs[[years[y]]]
  tot_df = tot_wetland_dfs[[years[y]]]
  
  # created new protected status column
  gap_df$protected_status = "Locally unprotected"
  for (i in 1:2) { gap_df$protected_status[gap_df$GAP_Sts == i] = "Managed for biodiversity" }
  gap_df$protected_status[(gap_df$NAME %in% pro_cnties) & (!(gap_df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  gap_df$protected_status[(!(gap_df$NAME %in% pro_cnties)) & (gap_df$GAP_Sts == 3)] = "Managed for multiple uses"
  gap_df$protected_status[(!(gap_df$NAME %in% pro_cnties)) & (gap_df$GAP_Sts == 4)] = "Locally unprotected"
  
  # create column based on protection status
  gap_df$not_protected = 1*(gap_df$protected_status == "Locally unprotected")
  for (v in 1:n_v) {
    for (i in 1:n_c) {
      gap_df_sub = gap_df[gap_df$NAME == all_counties[i],]
      tot_df_sub = tot_df[tot_df$NAME == all_counties[i],]
      for (j in 1:n_w) {
        for (k in 1:n_p) {
          for (b in 1:n_bd) {
            county_area_df_uniform[n,"year"] = years[y]
            county_area_df_uniform[n,"version"] = versions[v]
            county_area_df_uniform[n,"NAME"] = all_counties[i]
            county_area_df_uniform[n,"water_cutoff"] = water_regimes[j]
            county_area_df_uniform[n,"stream_perm"] = perm_levels[k]
            county_area_df_uniform[n,"buf_dist"] = buf_dists[b]
            county_area_df_uniform[n,"buf_type"] = "uniform"
            wrs = water_regimes[1:j]
            wrs_inds = !(gap_df_sub$WATER_REGI %in% wrs)
            if (versions[v] == "nhd") {
              buf_ws_col = paste("NHDIn", perm_levels[k], "_", buf_dists[b], sep="")
            } else {
              buf_ws_col = paste("BRFIn", perm_levels[k], "_", buf_dists[b], sep="")
            }
            non_jurisdictional_inds = (wrs_inds | (gap_df_sub$Within_Lev == 1 | gap_df_sub[,buf_ws_col] == 0))
            county_area_df_uniform[n,"total_area"] = sum(tot_df_sub[,"Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"wotus_locally_unprotected_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$not_protected == 1,
                                                                             "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"nonwotus_locally_unprotected_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$not_protected == 1,
                                                                                "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"wotus_managed_biodiversity_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "Managed for biodiversity",
                                                                              "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"nonwotus_managed_biodiversity_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "Managed for biodiversity",
                                                                                 "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"wotus_managed_multuses_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "Managed for multiple uses",
                                                                          "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"nonwotus_managed_multuses_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "Managed for multiple uses",
                                                                             "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"wotus_county_ordinance_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "County permitting and mitigation",
                                                                          "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"nonwotus_county_ordinance_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "County permitting and mitigation",
                                                                             "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"total_nonwotus_behind_levee_area"] = sum(gap_df_sub[gap_df_sub$Within_Lev == 1,
                                                                                  "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"total_nonwotus_isolated_area"] = sum(gap_df_sub[gap_df_sub[,buf_ws_col] == 0,
                                                                              "Polygon_Area_Ha_Geodesic"])
            county_area_df_uniform[n,"total_nonwotus_too_dry_area"] = sum(gap_df_sub[wrs_inds,"Polygon_Area_Ha_Geodesic"])
            n = n + 1
          }
        }
      }
    }
  }
}
sum(county_area_df_uniform$total_nonwotus_behind_levee_area)

# add rows for variable buffer
county_area_df_variable = data.frame(matrix(nrow=n_y*n_v*n_w*n_p, ncol=19))
colnames(county_area_df_variable) = c("year","version","NAME","water_cutoff",
                                      "stream_perm","buf_dist","buf_type","total_area",
                                      "wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                                      "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                                      "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                                      "wotus_county_ordinance_area","nonwotus_county_ordinance_area",
                                      "total_nonwotus_behind_levee_area","total_nonwotus_isolated_area",
                                      "total_nonwotus_too_dry_area")
n = 1
for (y in 1:n_y) {
  # read in wetland dataframe for corresponding year
  gap_df = gap_wetland_dfs[[years[y]]]
  tot_df = tot_wetland_dfs[[years[y]]]
  
  # created new protected status column
  gap_df$protected_status = "Locally unprotected"
  for (i in 1:2) { gap_df$protected_status[gap_df$GAP_Sts == i] = "Managed for biodiversity" }
  gap_df$protected_status[(gap_df$NAME %in% pro_cnties) & (!(gap_df$GAP_Sts %in% c(1,2)))] = "County permitting and mitigation"
  gap_df$protected_status[(!(gap_df$NAME %in% pro_cnties)) & (gap_df$GAP_Sts == 3)] = "Managed for multiple uses"
  gap_df$protected_status[(!(gap_df$NAME %in% pro_cnties)) & (gap_df$GAP_Sts == 4)] = "Locally unprotected"
  
  # create column based on protection status
  gap_df$not_protected = 1*(gap_df$protected_status == "Locally unprotected")
  for (v in 1:n_v) {
    for (i in 1:n_c) {
      gap_df_sub = gap_df[gap_df$NAME == all_counties[i],]
      tot_df_sub = tot_df[tot_df$NAME == all_counties[i],]
      for (j in 1:n_w) {
        for (k in 1:n_p) {
          county_area_df_variable[n,"year"] = years[y]
          county_area_df_variable[n,"version"] = versions[v]
          county_area_df_variable[n,"NAME"] = all_counties[i]
          county_area_df_variable[n,"water_cutoff"] = water_regimes[j]
          county_area_df_variable[n,"stream_perm"] = perm_levels[k]
          county_area_df_variable[n,"buf_dist"] = 20
          county_area_df_variable[n,"buf_type"] = "variable"
          wrs = water_regimes[1:j]
          wrs_inds = !(gap_df_sub$WATER_REGI %in% wrs)
          if (versions[v] == "nhd") {
            buf_ws_col = paste("NHDIn", perm_levels[k], "_", "20V", sep="")
          } else {
            buf_ws_col = paste("BRFIn", perm_levels[k], "_", "20V", sep="")
          }
          non_jurisdictional_inds = (wrs_inds | (gap_df_sub$Within_Lev == 1 | gap_df_sub[,buf_ws_col] == 0))
          county_area_df_variable[n,"total_area"] = sum(tot_df_sub[,"Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"wotus_locally_unprotected_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$not_protected == 1,
                                                                                      "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"nonwotus_locally_unprotected_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$not_protected == 1,
                                                                                         "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"wotus_managed_biodiversity_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "Managed for biodiversity",
                                                                                       "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"nonwotus_managed_biodiversity_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "Managed for biodiversity",
                                                                                          "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"wotus_managed_multuses_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "Managed for multiple uses",
                                                                                   "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"nonwotus_managed_multuses_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "Managed for multiple uses",
                                                                                      "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"wotus_county_ordinance_area"] = sum(gap_df_sub[non_jurisdictional_inds == 0 & gap_df_sub$protected_status == "County permitting and mitigation",
                                                                                   "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"nonwotus_county_ordinance_area"] = sum(gap_df_sub[non_jurisdictional_inds == 1 & gap_df_sub$protected_status == "County permitting and mitigation",
                                                                                      "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"total_nonwotus_behind_levee_area"] = sum(gap_df_sub[gap_df_sub$Within_Lev == 1,
                                                                                        "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"total_nonwotus_isolated_area"] = sum(gap_df_sub[gap_df_sub[,buf_ws_col] == 0,
                                                                                    "Polygon_Area_Ha_Geodesic"])
          county_area_df_variable[n,"total_nonwotus_too_dry_area"] = sum(gap_df_sub[wrs_inds,
                                                                                    "Polygon_Area_Ha_Geodesic"])
          n = n + 1
        }
      }
    }
  }
}

# combine dataframes
county_area_df = rbind(county_area_df_uniform, county_area_df_variable)

# filter out scenarios that we did not consider in 2023
county_area_df_2023_uniform = subset(county_area_df, (year == "2023" & version == "nhd") & ((buf_dist %in% c(1,10,100)) & buf_type == "uniform"))
county_area_df_2023_variable = subset(county_area_df, (year == "2023" & version == "nhd") & ((buf_dist %in% c(20)) & buf_type == "variable"))
county_area_df_2024_uniform = subset(county_area_df, (year == "2024" & version == "brinkerhoff") & ((buf_dist %in% c(5,20)) & buf_type == "uniform"))
county_area_combo = rbind(county_area_df_2023_uniform, county_area_df_2023_variable, county_area_df_2024_uniform)

# calculate total areas by protection category
area_cols_long = c("wotus_locally_unprotected_area","nonwotus_locally_unprotected_area",
                   "wotus_managed_biodiversity_area","nonwotus_managed_biodiversity_area",
                   "wotus_managed_multuses_area","nonwotus_managed_multuses_area",
                   "wotus_county_ordinance_area","nonwotus_county_ordinance_area",
                   "total_nonwotus_behind_levee_area","total_nonwotus_isolated_area",
                   "total_nonwotus_too_dry_area")
area_cols_short = c("WOT_UP_AR","NWOT_UP_AR",
                    "WOT_MB_AR","NWOT_MB_AR",
                    "WOT_MU_AR","NWOT_MU_AR",
                    "WOT_CO_AR","NWOT_CO_AR",
                    "TNWOT_LEV","TNWOT_ISO",
                    "TNWOT_DRY")
n_ac = length(area_cols_long)
for (y in 1:n_y) {
  for (k in 1:n_ac) {
    # calculate mean, min, and max areas across scenarios for each county and water regime
    year_area_df = county_area_combo[county_area_combo$year == years[y],
                                      c("version","NAME","water_cutoff","stream_perm",
                                        "buf_dist", area_cols_long[k], "total_area")]
    colnames(year_area_df)[6:7] = c("partial_area","total_area")
    year_area_stats = year_area_df %>% 
                      group_by(version, NAME, water_cutoff) %>%
                      summarize(mean = mean(partial_area),
                                min = min(partial_area),
                                max = max(partial_area))
    year_percent_stats = year_area_df %>% 
                         group_by(version, NAME, water_cutoff) %>%
                         summarize(mean = mean(partial_area/total_area*100),
                                   min = min(partial_area/total_area*100),
                                   max = max(partial_area/total_area*100))
    
    # reshape to get areas for each county in the columns
    for (i in 1:n_w) { year_area_stats$water_cutoff[year_area_stats$water_cutoff == water_regimes[i]] = wr_abrevs[i] }
    for (i in 1:n_w) { year_percent_stats$water_cutoff[year_percent_stats$water_cutoff == water_regimes[i]] = wr_abrevs[i] }
    
    area_wide_df = data.frame(matrix(nrow=n_c, ncol=0))
    percent_wide_df = data.frame(matrix(nrow=n_c, ncol=0))
    area_wide_df$NAME = all_counties
    percent_wide_df$NAME = all_counties
    v_i = versions[y]
    for (j in 1:n_w) {
      area_wr_rows_ij = subset(year_area_stats, water_cutoff == wr_abrevs[j] & version == v_i)
      percent_wr_rows_ij = subset(year_percent_stats, water_cutoff == wr_abrevs[j] & version == v_i)
      colnames(area_wr_rows_ij)[4:6] = paste(colnames(area_wr_rows_ij)[4:6], wr_abrevs[j], v_i, sep="_")
      colnames(percent_wr_rows_ij)[4:6] = paste(colnames(percent_wr_rows_ij)[4:6], wr_abrevs[j], v_i, sep="_")
      area_wide_df = left_join(area_wide_df, 
                               area_wr_rows_ij[,c("NAME",colnames(area_wr_rows_ij)[4:6])], 
                               by=c("NAME"))
      percent_wide_df = left_join(percent_wide_df, 
                                  percent_wr_rows_ij[,c("NAME",colnames(percent_wr_rows_ij)[4:6])], 
                                  by=c("NAME"))
    }
    write.csv(area_wide_df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2",
                                  years[y], area_cols_long[k], "county_wetland_area_totals.csv", sep="_"), 
              row.names=F)
    write.csv(percent_wide_df, paste("Dual-Risk-Repo/County_Summaries/wetland_area_percents/step2",
                                  years[y], area_cols_long[k], "county_wetland_area_percents.csv", sep="_"), 
              row.names=F)
    year_area_subset = subset(year_area_df, buf_dist==20 & stream_perm==1 & water_cutoff == "Semipermanently Flooded")
    write.csv(year_area_subset[,c("NAME","total_area")], 
              paste("Dual-Risk-Repo/County_Summaries/", "county_wetland_area_sums_", years[y], ".csv", sep=""))
  }
}

# calculate absolute area differences for each scenario between Brinkerhoff + NLCD 2024 & NHD + NLCD 2023
stats = c("mean","min","max")
n_s = length(stats)
for (i in 1:n_ac) {
  diff_df = data.frame(matrix(nrow=n_c, ncol=0))
  diff_df$NAME = all_counties
  nhd_2023_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2023",
                               area_cols_long[i], "county_wetland_area_totals.csv", sep="_"))
  brf_2024_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_totals/step2_2024",
                               area_cols_long[i], "county_wetland_area_totals.csv", sep="_"))
  for (j in 1:n_s) {
    for (k in 1:n_w) {
      diff_col = paste(stats[j], wr_abrevs[k], "diff", sep="_")
      new_col = data.frame(matrix(nrow=n_c, ncol=1))
      colnames(new_col) = c(diff_col)
      new_col$NAME = all_counties
      nhd_2023_col = paste(stats[j], wr_abrevs[k], "nhd", sep="_")
      brf_2024_col = paste(stats[j], wr_abrevs[k], "brinkerhoff", sep="_")
      new_col[,diff_col] = brf_2024_df[,brf_2024_col] - nhd_2023_df[,nhd_2023_col]
      diff_df = left_join(diff_df, new_col, by="NAME")
    }
  }
  write.csv(diff_df, 
            paste("Dual-Risk-Repo/County_Summaries/wetland_area_differences/step2",
                  area_cols_long[i],"county_wetland_area_differences.csv", sep="_"), 
            row.names=F)
}

# calculate percent area differences for each scenario between Brinkerhoff + NLCD 2024 & NHD + NLCD 2023
for (i in 1:n_ac) {
  diff_df = data.frame(matrix(nrow=n_c, ncol=0))
  diff_df$NAME = all_counties
  nhd_2023_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_percents/step2_2023",
                               area_cols_long[i],"county_wetland_area_percents.csv", sep="_"))
  brf_2024_df = read.csv(paste("Dual-Risk-Repo/County_Summaries/wetland_area_percents/step2_2024",
                               area_cols_long[i],"county_wetland_area_percents.csv", sep="_"))
  for (j in 1:n_s) {
    for (k in 1:n_w) {
      diff_col = paste(stats[j], wr_abrevs[k], "diff", sep="_")
      new_col = data.frame(matrix(nrow=n_c, ncol=1))
      colnames(new_col) = c(diff_col)
      new_col$NAME = all_counties
      nhd_2023_col = paste(stats[j], wr_abrevs[k], "nhd", sep="_")
      brf_2024_col = paste(stats[j], wr_abrevs[k], "brinkerhoff", sep="_")
      new_col[,diff_col] = brf_2024_df[,brf_2024_col] - nhd_2023_df[,nhd_2023_col]
      diff_df = left_join(diff_df, new_col, by="NAME")
    }
  }
  write.csv(diff_df, 
            paste("Dual-Risk-Repo/County_Summaries/wetland_percentage_differences/step2",
                  area_cols_long[i],"county_wetland_percent_differences.csv",sep="_"), 
            row.names=F)
}
