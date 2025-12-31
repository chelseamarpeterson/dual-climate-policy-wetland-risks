setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/NHD_Flowlines")

library(ggplot2)
library(dplyr)

# read in csvs
brf.df = read.csv("NHDFlowline_Step8_MergeIsolatedFlowlines_Brinkerhoff_ExportTable.csv")
nhd.df = read.csv("NHDFlowline_Step8_MergeIsolatedFlowlines_ExportTable.csv")

# fill in hydro class with 4 for isolated flowlines
brf.df$Brinkerhoff_Hydro_Class[is.na(brf.df$Brinkerhoff_Hydro_Class)] = 4
nhd.df$Final_Hydro_Class[is.na(nhd.df$Final_Hydro_Class)] = 4

# calculate total length in each category
brf.sum = brf.df %>%
          group_by(Brinkerhoff_Hydro_Class) %>%
          summarize(total_length_km = sum(Length_Km))
nhd.sum = nhd.df %>%
          group_by(Final_Hydro_Class) %>%
          summarize(total_length_km = sum(Length_Km))

# print round totals
round(brf.sum$total_length_km,1)
round(nhd.sum$total_length_km,1)

# print round percents
round(brf.sum$total_length_km/sum(brf.sum$total_length_km)*100,1)
round(nhd.sum$total_length_km/sum(nhd.sum$total_length_km)*100,1)
