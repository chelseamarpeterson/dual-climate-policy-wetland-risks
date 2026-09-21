setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project/dual-risk-repo/County_Summaries")

library(tidyverse)
library(purrr)
library(ggfortify)
library(patchwork)
library(reshape2)

################################################################################
# combine ecosystem service data

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

################################################################################
# combine climate data

# shared socioeconomic pathways
ssps = c("SSP245","SSP370")
ssp.labels = c("SSP2-4.5","SSP3-7.0")
n.s = length(ssps)

# time intervals
times = c("2041_2070","2071_2100")
time.labels = c("2041-2070","2071-2100")
n.t = length(times)

# extremes
extremes = c("Temp","Precip")
n.e = length(extremes)

# variables
var.list = list("Temp" = c("FD","TXge100F"),
                "Precip" = c("dry_spells","wet_spell"))
var.labels = list("Temp" = c("-&Delta;(Frost days)", 
                             "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])"),
                  "Precip" = c("&Delta;(Max. length dry spell)", 
                               "&Delta;(Max. length wet spell)"))
var.order = c("-&Delta;(Frost days)",
              "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
              "&Delta;(Max. length dry spell)", 
              "&Delta;(Max. length wet spell)")

# put extremes into dataframe
colnames(all.df)[1:2] = c("county","region")
ex.df = data.frame(matrix(nrow=0, ncol=6))
colnames(ex.df) = c("ssp","time","variable","county","value","region")
for (i in 1:n.e) {
  ex = extremes[i]
  for (j in 1:n.s) {
    for (k in 1:n.t) {
      df = read.csv(paste(paste("Step1",ex,"BMA_County_Ave", ssps[j], times[k], sep="_"), ".csv", sep=""))
      vars = var.list[[ex]]
      labels = var.labels[[ex]]
      for (l in 1:2) {
        ex.df.ijkl = data.frame(matrix(nrow=nrow(df), ncol=4))
        colnames(ex.df.ijkl) = c("ssp","time","variable","county") 
        ex.df.ijkl$ssp = ssp.labels[j]
        ex.df.ijkl$time = time.labels[k]
        ex.df.ijkl$county = df$NAME
        ex.df.ijkl$variable = labels[l]
        ex.df.ijkl = inner_join(ex.df.ijkl, all.df[,c("county","region")], by="county", keep=F)
        if (labels[l] == "-&Delta;(Frost days)") {
          ex.df.ijkl$value  = -df[,vars[l]]
        } else {
          ex.df.ijkl$value  = df[,vars[l]]
        }
        ex.df = rbind(ex.df, ex.df.ijkl)
      }
    }
  }
}

# make column for ssp x time
ex.df$ssp_time = paste(ex.df$ssp, ex.df$time, sep=" &times; ")

# write extremes to csv
write.csv(ex.df, "All_County_Climate_Extremes.csv", row.names=F)

