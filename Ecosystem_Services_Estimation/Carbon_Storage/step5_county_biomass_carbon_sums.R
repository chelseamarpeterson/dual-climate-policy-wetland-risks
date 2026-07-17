setwd("D:/Wetland_Climate_Impacts/Carbon_Storage_Estimation")

library(tidyverse)
library(patchwork)

# read in csv file with biomass C stocks
df.wr = read.csv("IL_WS_Step18_2024NLCD_WaterRegime_SoilBiomass_Cstorage.csv")
sum(is.na(df.wr$StockC_Mg))

# fix polygon area columns
colnames(df.wr)[28:29] = c("Polygon_Area_Ha_Geodesic","Polygon_Area_Ha_Planar")
colnames(df.wr)[30:31] = c("Polygon_Area_Ac_Geodesic","Polygon_Area_Ac_Planar")

# sum total DEM- and unit-area-based estimates for each county
county.sums = df.wr %>%
              group_by(NAME) %>% 
              summarize(Total_Carbon_Mg = sum(StockC_Mg))
colnames(county.sums)[1] = "Name"

# write to file
write.csv(county.sums, 
          "D:/Wetland_Climate_Impacts/Carbon_Storage_Estimation/County_Carbon_Totals.csv",
          row.names = F)