setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project/Dual-Risk-Repo/County_Summaries")

library(tidyverse)
library(purrr)
library(ggfortify)
library(patchwork)
library(reshape2)

# script that combines files with county-level estimates of each wetland ecosystem service

# read in dataframes
county.df = read.csv("Step0_County_Names_Regions.csv")
plant.df = read.csv("Step3_County_Plant_Diversity_Totals.csv")
herp.df = read.csv("Step4_County_Herpetofauna_Diversity_Totals.csv")
soil.df = read.csv("Step6_County_Carbon_Storage_Totals.csv")
flood.df = read.csv("Step5_County_Flood_Storage_Volume_Totals.csv")

# join all
all.df = left_join(county.df[,c("NAME","County_Group")], plant.df[,c("NAME","plantn")]) # species richness
all.df = left_join(all.df, herp.df[,c("NAME","Tot_wetland_dependent")]) # wetland-dependent herps
all.df = left_join(all.df, soil.df[,c("NAME","Total_Carbon_Mg")]) # carbon storage
all.df = left_join(all.df, flood.df[,c("NAME","DEM_VolTot")]) # flood storage volume

# update service labels
colnames(all.df) = c("County","Region",
                     "Plant species richness",
                     "Herpetofauna species richness",
                     "Carbon storage",
                     "Floodwater storage capacity")

# write to file
write.csv(all.df, "All_County_Ecosystem_Services.csv", row.names=F)