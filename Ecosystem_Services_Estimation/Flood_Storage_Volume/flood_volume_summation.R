setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_Dual_Wetland_Risk/Wetland_Climate_Impacts/Flood_Volume_Estimation")

library(dplyr)
library(ggplot2)
library(patchwork)

################################################################################
# metadata

# read in water regime dataframe with volume estimates
water.regime.df = read.csv("IL_WS_Step18_2024NLCD_WaterRegime_FloodVol.csv")

# fix polygon area columns
colnames(water.regime.df)[28:29] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(water.regime.df)[30:31] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# calculate total flood storage volume for Illinois
total_gis_flood_vol_m3 = sum(water.regime.df$flood_volume_m3)

# county names
all.counties = sort(unique(water.regime.df$NAME))
n.ct = length(all.counties)

# sum total DEM- and unit-area-based estimates for each county
county.sums = water.regime.df %>%
              group_by(NAME) %>% 
              summarize(DEM_VolTot = sum(flood_volume_m3))

# calculate percentages of state total
county.sums$DEM_VolPer = county.sums$DEM_VolTot / total_gis_flood_vol_m3 * 100

# write county totals and percentages to csv
write.csv(county.sums, "County_Volume_Totals.csv", row.names=F)
