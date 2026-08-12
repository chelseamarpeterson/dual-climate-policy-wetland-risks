setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project/Dual-Risk-Repo/County_Summaries")

library(tidyverse)
library(purrr)
library(ggfortify)
library(patchwork)
library(reshape2)

# script that combines files with county-level estimates of each wetland ecosystem service

# read in dataframes
county.df = read.csv("Step0_County_Names_Regions.csv")
eco.df = read.csv("Step3_County_Ecosystem_Service_Totals.csv")

# join all
all.df = left_join(county.df[,c("NAME","County_Group")], eco.df[,c("NAME","plantn")]) # species richness
all.df = left_join(all.df, eco.df[,c("NAME","Tot_wetland_dependent")]) # wetland-dependent herps
all.df = left_join(all.df, eco.df[,c("NAME","Total_Carbon_Gg")]) # carbon storage
all.df = left_join(all.df, eco.df[,c("NAME","DEM_VolTot")]) # flood storage volume

# update service labels
colnames(all.df) = c("County","Region",
                     "Plant species richness",
                     "Herpetofauna species richness",
                     "Carbon storage",
                     "Floodwater storage capacity")

# divide floodwater storage capacity and carbon storage by 1,000
all.df[,"Floodwater storage capacity"] = all.df[,"Floodwater storage capacity"]/1000 # 1,000 m3
all.df[,"Carbon storage"] = all.df[,"Carbon storage"]/1000 # 1,000 Gg

# write to file
write.csv(all.df, "All_County_Ecosystem_Services.csv", row.names=F)