setwd("F:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

library(tidyverse)
library(patchwork)

# read in csv file with biomass C stocks
df.wr = read.csv("IL_WS_Step18_2024NLCD_WaterRegime_Biomass_Cstocks.csv")

# fix polygon area columns
colnames(df.wr)[28:29] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(df.wr)[30:31] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# sum total DEM- and unit-area-based estimates for each county
county.sums = df.wr %>%
              group_by(NAME) %>% 
              summarize(Biomass_Carbon = sum(Biomass_Ctotal_Zonal))
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