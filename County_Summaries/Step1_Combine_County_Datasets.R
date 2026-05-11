setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/County_Summaries")

library(tidyverse)
library(purrr)
library(ggfortify)
library(patchwork)
library(reshape2)

# read in dataframes
precip.df = read.csv("Step1_County_Precip_Extremes_ssp585_2071_2100.csv")
temp.df = read.csv("Step1_County_Temp_Extremes_ssp585_2071_2100.csv")
wetland.df = read.csv("Step2_County_Unprotected_NonWOTUS_WetlandArea.csv")
plant.df = read.csv("Step3_County_Plant_Diversity_Summary.csv")
herp.df = read.csv("Step4_County_Herpetofauna_Diversity_Summary.csv")
flood.df = read.csv("Step5_County_Flood_Storage_Volume_Estimates.csv")
biomass.df = read.csv("Step6_County_Biomass_Carbon_Storage_Estimates.csv")
soil.df = read.csv("Step6_County_Database_Estimated_Soil_Carbon_Storage.csv")

# put all dataframes into a list
df_list = list(precip.df, temp.df, wetland.df, plant.df, herp.df, flood.df, biomass.df, soil.df)

# join all by county name
df = df_list %>% reduce(left_join, by = "NAME")

# sum county biomass and soil C
df$total_county_C = df$county_carbon_zonal + df$county_SOC

# update service labels
cutoffs = c("mean_SF_brinkerhoff","mean_SFS_brinkerhoff","mean_SPF_brinkerhoff","mean_IE_brinkerhoff","mean_PF_brinkerhoff")
services = c("n","Tot_wetland_dependent","total_gis_vol_m3","total_county_C")
service.labels = c("Wetland plants","Wetland herpetofauna","Flood storage volume","Carbon storage")
colnames(df)[which(colnames(df) %in% services)] = service.labels

# hierarchical clustering
dist_df = dist(scale(df[,service.labels], center=T, scale=T), method = "manhattan")
hc = hclust(dist_df)
plot(hc)
clusters = cutree(hc, k = 3)
clusters

# run pca
#pca_result <- prcomp(df[,cols], scale. = TRUE)
#summary(pca_result)
#df$cluster = factor(clusters)
#autoplot(pca_result, data=df, color='cluster', 
#         label = T, label.label = "NAME",
#         loadings=T,loadings.label = T,
#         frame = T, frame.type = "norm",
#         x=1, y=3)

# PCA on temperature variables
temp.cols = c("DTR","ID","TXge100F")
pca_result <- prcomp(df[,temp.cols], scale. = TRUE)
summary(pca_result)
df$cluster = factor(clusters)
p1 = autoplot(pca_result, x = 1, y = 2, loadings=T, loadings.label = T)
p1
p2 = autoplot(pca_result, x = 1, y = 3, loadings=T, loadings.label = T)
p1+p2

# PCA on precipitation variables
precip.cols = c("dry_spells","pr_anz90","pr_anz95","pr_anz99","pr_annual","pr_danz90",
                "pr_danz95","pr_danz99","pr_dge1in","pr_dge2in","pr_dge3in","pr_dge4in",
                "pr_dge5in","prmax1day","prmax5day","prmax10day","wet_spell","wet_days")
pca_result <- prcomp(df[,precip.cols], scale. = TRUE)
summary(pca_result)
df$cluster = factor(clusters)
p1 = autoplot(pca_result, x = 1, y = 2, loadings=T, loadings.label = T)
p1
p2 = autoplot(pca_result, x = 1, y = 3, loadings=T, loadings.label = T)
p1+p2

# linear evaluating of each ecosystem service versus protection status
df.services.versus.area = melt(df[,c(service.labels, "mean_SFS_brinkerhoff", "NAME")],
                              id.vars=c("NAME","mean_SFS_brinkerhoff"))
ggplot(df.services.versus.area[df.services.versus.area$variable %in% service.labels,],
       aes(x=mean_SFS_brinkerhoff,
           y=log(value))) + geom_point() +
       facet_wrap(.~variable, ncol=2, scales="free_y")





 ## plot histograms of climate extremes across clusters


# ecosystem services
ggplot(df.melt[df.melt$variable %in% service.labels,], 
       aes(x=cluster, 
           y=value)) + 
       geom_boxplot() +
       facet_wrap(.~variable, scales="free_y", ncol=4)

# ecosystem services
ggplot(df.melt[df.melt$variable %in% cutoffs,], 
       aes(y=variable, 
           x=value,
           fill=cluster)) + 
      geom_boxplot()

# climate extremes
ggplot(df.melt[df.melt$variable %in% c(precip.cols, temp.cols),], 
       aes(x=cluster, 
           y=value)) + 
  geom_boxplot() +
  facet_wrap(.~variable, scales="free_y", ncol=4)

