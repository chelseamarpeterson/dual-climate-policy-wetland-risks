setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/NHD_Flowlines")

library(ggplot2)
library(dplyr)

# read in csvs
nhd.df = read.csv("NHDFlowline_Step8_MergeIsolatedFlowlines_Brinkerhoff_ExportTable.csv")

# calculate total length in each category
brf.sum = nhd.df %>%
          group_by(Brinkerhoff_Hydro_Class) %>%
          summarize(total_length_km = sum(Length_m_Geodesic)/1000)
nhd.sum = nhd.df %>%
          group_by(NHD_Hydro_Class) %>%
          summarize(total_length_km = sum(Length_m_Geodesic)/1000)

# print round totals
round(nhd.sum$total_length_km,1)
round(brf.sum$total_length_km,1)
round(brf.sum$total_length_km-nhd.sum$total_length_km,1)

# print non-perennial totals
round(sum(nhd.sum$total_length_km[-which(nhd.sum$NHD_Hydro_Class == 1)]),1)
round(sum(brf.sum$total_length_km[-which(brf.sum$Brinkerhoff_Hydro_Class == 1)]),1)
round(sum(brf.sum$total_length_km[-which(brf.sum$Brinkerhoff_Hydro_Class == 1)]) - sum(nhd.sum$total_length_km[-which(nhd.sum$NHD_Hydro_Class == 1)]),1)

# print round percents
round(nhd.sum$total_length_km/sum(nhd.sum$total_length_km)*100,1)
round(brf.sum$total_length_km/sum(brf.sum$total_length_km)*100,1)
round(brf.sum$total_length_km/sum(brf.sum$total_length_km)*100 - nhd.sum$total_length_km/sum(nhd.sum$total_length_km)*100,1)

# print non-perennial total percents
round(sum(nhd.sum$total_length_km[-which(nhd.sum$NHD_Hydro_Class == 1)])/sum(nhd.sum$total_length_km)*100,1)
round(sum(brf.sum$total_length_km[-which(brf.sum$Brinkerhoff_Hydro_Class == 1)])/sum(brf.sum$total_length_km)*100,1)
round(sum(brf.sum$total_length_km[-which(brf.sum$Brinkerhoff_Hydro_Class == 1)])/sum(brf.sum$total_length_km)*100 - sum(nhd.sum$total_length_km[-which(nhd.sum$NHD_Hydro_Class == 1)])/sum(nhd.sum$total_length_km)*100,1)

