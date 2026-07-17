setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/NWCA_Model")

library(tidyverse)

# read in 2011/16 data
df.2011 = read.csv("Site_Info/nwca2011_siteinfo.csv")
df.2016 = read.csv("Site_Info/nwca2016_siteinfo.csv")

# subset to Illinois data
il.2011 = subset(df.2011, PSTL_CODE == "IL")
il.2016 = subset(df.2016, PSTL_CODE == "IL" & EVAL_CAT == "Target_Sampled")

# isolate key columns
il.sub.2011 = il.2011 %>% select(SITE_ID, VISIT_NO, LAT_ANALYS, LON_ANALYS, WETCLS_GRP, WETCLS_EVL)
il.sub.2016 = il.2016 %>% select(SITE_ID, VISIT_NO, LAT_ANALYS, LON_ANALYS, WETCLS_GRP, WETCLS_EVL)

# make year column
il.sub.2011$YEAR = 2011
il.sub.2016$YEAR = 2016

# combine dataframes
il.site.df = rbind(il.sub.2011, il.sub.2016)
write.csv(il.site.df, "Site_Info/Illinois_Site_Info.csv", row.names=F)

# read in soil data
soil.2011 = read.csv("Soil_Chemistry/nwca2011_soilchem.csv")
soil.2016 = read.csv("Soil_Chemistry/nwca2016_soilchem.csv")

# isolate Illinois data
il.soil.2011 = subset(soil.2011, STATE == "IL")[,c("SITE_ID","VISIT_NO","DEPTH","BULK_DEN_DBF","TOT_CARBON")]
il.soil.2016 = subset(soil.2016, STATE == "IL")[,c("SITE_ID","VISIT_NO","HORIZON","HORIZON_DEPTH","DBF_SOIL_CORE_DBF_AVG","C_TOT_ELEM_ANLYS")]

# 2016: reorder horizons
il.soil.2016 = il.soil.2016 %>% arrange(SITE_ID, VISIT_NO, HORIZON_DEPTH)

# 2016: unique siteID-visitNO combinations
il.soil.2016 = il.soil.2016 %>% unite("SITE_ID_VISIT_NO","SITE_ID","VISIT_NO", sep="-", remove=F)
site_visits = unique(sort(il.soil.2016$SITE_ID_VISIT_NO))
n_sv = length(site_visits)
il.soil.2016$HORIZON_CENTER_DEPTH = 0
for (i in 1:n_sv) {
  depths_i = il.soil.2016[il.soil.2016$SITE_ID_VISIT_NO == site_visits[i],"HORIZON_DEPTH"]
  n_i = length(depths_i)
  if (n_i > 1) {
    il.soil.2016[il.soil.2016$SITE_ID_VISIT_NO == site_visits[i],"HORIZON_CENTER_DEPTH"] = (c(0,depths_i[1:(n_i-1)]) + depths_i)/2
  } else {
    il.soil.2016[il.soil.2016$SITE_ID_VISIT_NO == site_visits[i],"HORIZON_CENTER_DEPTH"] = depths_i/2
  }
}

# update column names
colnames(il.soil.2011) = c("SITE_ID","VISIT_NO","DEPTH","BD","TC")
il.soil.2016 = il.soil.2016 %>% select(SITE_ID, VISIT_NO, HORIZON_CENTER_DEPTH, DBF_SOIL_CORE_DBF_AVG, C_TOT_ELEM_ANLYS)
colnames(il.soil.2016) = c("SITE_ID","VISIT_NO","DEPTH","BD","TC")
il.soil.2011$YEAR = 2011
il.soil.2016$YEAR = 2016

# combine soil dataframes
il.soil.df = rbind(il.soil.2011, il.soil.2016)

# join lat/lon and vegetation data to soil dataframe
il.soil.df = left_join(il.soil.df, il.site.df, by=c("SITE_ID","VISIT_NO","YEAR"))

# remova NA total carbon rows then write to file
il.soil.df = subset(il.soil.df, !is.na(il.soil.df$TC))
write.csv(il.soil.df, "Soil_Chemistry/Illinois_NWCA_TC_Points.csv", row.names=F)
