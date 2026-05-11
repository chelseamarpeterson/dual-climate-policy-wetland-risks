setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/County_Summaries")

library(tidyverse)
library(purrr)
library(ggfortify)
library(patchwork)
library(reshape2)

# ecosystem services
services = c("n","Tot_wetland_dependent","total_gis_vol_m3","total_county_C")
service.labels = c("Wetland plants","Wetland herpetofauna","Flood storage volume","Carbon storage")
n.s = length(services)

# wetland flood-frequency cutoffs
cutoffs = c("mean_SF_brinkerhoff","mean_SFS_brinkerhoff","mean_SPF_brinkerhoff","mean_IE_brinkerhoff","mean_PF_brinkerhoff")
cutoff.labels = c("Seasonally Flooded","Seasonally Flooded/Saturated","Semipermanently Flooded","Intermittently Exposed","Permanently Flooded")
n.c = length(cutoffs)

# groups
protection.groups = c("Only_WOTUS","Managed_Biodiversity_MultipleUses_NonWOTUS","County_Ordinances_NonWOTUS","Unprotected_NonWOTUS")
group.labels = c("Clean Water Act","Managed for biodiversity or multiple uses","County ordinance","Unprotected")
n.g = length(protection.groups)

# read in service dataframes
plant.df = read.csv("Step3_County_Plant_Diversity_Summary.csv")
herp.df = read.csv("Step4_County_Herpetofauna_Diversity_Summary.csv")
flood.df = read.csv("Step5_County_Flood_Storage_Volume_Estimates.csv")
biomass.df = read.csv("Step6_County_Biomass_Carbon_Storage_Estimates.csv")
soil.df = read.csv("Step6_County_Database_Estimated_Soil_Carbon_Storage.csv")

# put all dataframes into a list
census_cols = c("STATEFP","COUNTYFP","COUNTYNS","GEOID","GEOIDFQ","NAMELSAD","LSAD","CLASSFP","MTFCC",
                "CSAFP","CBSAFP","METDIVFP","FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON")
df_list = list(plant.df %>% select(-c("county", "Shape_Length", "Shape_Area", "Ordinance", census_cols)),  
               herp.df %>% select(-c("County", "Shape_Length", "Shape_Area", census_cols)), 
               flood.df %>% select(-c("Name_1", "Shape_Length", "Shape_Area", census_cols)),  
               biomass.df %>% select(-c("Name_1", "Shape_Length", "Shape_Area", census_cols)),  
               soil.df %>% select(-c(Name_1, Shape_Length, Shape_Area, census_cols)))

# calculate correlations for each ecosystem service
df.lm = data.frame(matrix(nrow=n.g*n.s*n.c, ncol=6))
colnames(df.lm) = c("group","service","cutoff","mean","lower","upper")
n = 1
for (i in 1:n.g) {
  # read in wetland area dataframe
  group = protection.groups[i]
  wetland.df = read.csv(paste("Step2",group,"County_Wetland_Area.csv", sep="_"))
  
  # add dataframe to list and join dataframes
  new_list = append(df_list, list(wetland.df))
  df = new_list %>% reduce(left_join, by = "NAME")
  
  # sum county biomass and soil C
  df$total_county_C = df$county_carbon_zonal + df$county_SOC
  
  # update service and cutoff labels
  colnames(df)[which(colnames(df) %in% services)] = service.labels
  colnames(df)[which(colnames(df) %in% cutoffs)] = cutoff.labels
  
  # reduce dataframe to minimum needed
  df.min = df %>% select(all_of(c("NAME",cutoff.labels,service.labels)))

  # divide columns by their means
  mean.cols = c(cutoff.labels,service.labels)
  df.means = colMeans(df.min[,mean.cols])
  n.cols = length(mean.cols)
  df.scale = df.min
  for (l in 1:n.cols) { df.scale[,mean.cols[l]] = df.min[,mean.cols[l]]/as.numeric(df.means[l]) }
  
  for (j in 1:n.s) {
    service = service.labels[j]
    for (k in 1:n.c) {
      cutoff = cutoff.labels[k]
      df.lm[n,"group"] = group.labels[i]
      df.lm[n,"service"] = service
      df.lm[n,"cutoff"] = cutoff
      formula = paste("log(`",service,"`) ~ `",cutoff,"`",sep="")
      lm.ijk = lm(formula, data=df.scale)
      ci.ijk = confint(lm.ijk, level = 0.95)
      df.lm[n,"mean"] = summary(lm.ijk)$coefficients[2,"Estimate"]
      df.lm[n,"lower"] = ci.ijk[2,"2.5 %"]
      df.lm[n,"upper"] = ci.ijk[2,"97.5 %"]
      n = n + 1
    }
  }
}




# plot effect sizes
ggplot(df.lm) +
       geom_point(aes(x=mean, 
                      y=factor(cutoff, levels=rev(cutoff.labels)),
                      color=service),
                  position=position_dodge(0.4)) +
       geom_errorbar(aes(xmin=lower, xmax=upper, 
                         y=factor(cutoff, levels=rev(cutoff.labels)),
                         color=service, linetype=service),
                     position=position_dodge(0.4),width=0.3) +
       geom_vline(xintercept=0) +
       facet_wrap(.~group, ncol=4, scales="free_x") +
       labs(y="Wetland flood frequency cutoff",
            x="Effect size",color="",linetype="")

ggplot(df.lm) +
        geom_point(aes(x=mean, 
                       y=factor(cutoff, levels=rev(cutoff.labels)),
                       color=group),
                   position=position_dodge(0.4)) +
        geom_errorbar(aes(xmin=lower, xmax=upper, 
                          y=factor(cutoff, levels=rev(cutoff.labels)),
                          color=group, linetype=group),
                      position=position_dodge(0.4),width=0.3) +
        geom_vline(xintercept=0) +
        facet_wrap(.~service, ncol=4, scales="free_x") +
        labs(y="Wetland flood frequency cutoff",
             x="Effect size",color="",linetype="")
  
## try running an RDA
wetland.df = read.csv(paste("Step2",protection.groups[1],"County_Wetland_Area.csv", sep="_"))[c("NAME","mean_SPF_brinkerhoff")]
colnames(wetland.df) = c("NAME", protection.groups[1])
for (i in 2:n.g) {
  add.df = read.csv(paste("Step2",protection.groups[i],"County_Wetland_Area.csv", sep="_"))[c("NAME","mean_SPF_brinkerhoff")]
  colnames(add.df) = c("NAME", protection.groups[i])
  wetland.df = left_join(wetland.df, add.df, by="NAME")
}


# add wetland dataframe to list and join dataframes
new_list = append(df_list, list(wetland.df))
df = new_list %>% reduce(left_join, by = "NAME")
df$total_C = df$county_carbon_zonal + df$county_SOC

x.cols = c("n","Tot_wetland_dependent","total_gis_vol_m3","total_C")
y.cols = protection.groups
library(vegan)
library(ggrepel)

y.data = data.frame(scale(df[,y.cols]))
x.data = data.frame(scale(df[,x.cols]))

# run the RDA
rda_model <- rda(y.data ~., data = x.data)
smry = summary(rda_model)
df1 = data.frame(scores(rda_model, display = "sites"))  
df2 = data.frame(scores(rda_model, display = "species"))
df3 = data.frame(rda_model$CCA$biplot[,1:2])

# variance explained by each RDA axis
var.rda1 = round(smry$cont$importance["Proportion Explained","RDA1"]*100,1)
var.rda2 = round(smry$cont$importance["Proportion Explained","RDA2"]*100,1)
var.rda1.label = paste("RDA1 (",var.rda1,"%)",sep="")
var.rda2.label = paste("RDA2 (",var.rda2,"%)",sep="")

# make biplot
rda.plot <- ggplot(data=df1) + 
            geom_point(aes(x=RDA1, y=RDA2),size=3.5) + 
            #geom_hline(yintercept=0, linetype="dotted") +
            #geom_vline(xintercept=0, linetype="dotted") +
            geom_segment(data=df3, 
                         aes(x=0, xend=RDA1, y=0, yend=RDA2), 
                         color="blue", 
                         arrow=arrow(length=unit(0.01,"npc"))) +
            geom_segment(data=df2, 
                         aes(x=0, xend=RDA1, y=0, yend=RDA2), 
                         color="red",  
                         arrow=arrow(length=unit(0.01,"npc"))) +
            geom_label_repel(data=df3, 
                             aes(x=RDA1, y=RDA2, label=rownames(df3)),
                             color="blue",fill="transparent",
                             alpha=0.7,size=3) +
            geom_label_repel(data=df2,
                             aes(x=RDA1, y=RDA2, label=rownames(df2)),
                             color="red", fill="transparent",
                             alpha=0.7,size=3.5) +
            labs(x=var.rda1.label,y=var.rda2.label)
            #xlim(c(-2,2)) + ylim(c(-2,2)) +
            #theme(text=element_text(size=14))
rda.plot




