setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(VennDiagram)
library(reshape2)
library(patchwork)
library(RColorBrewer)
library(deeptime)

# ecosystem service labels
es.labels = c("Plant species richness","Herpetofauna species richness",
              "Carbon storage","Floodwater storage capacity")
es.colors = c("darkgreen","purple4","orangered","royalblue4")
es.lines = c("solid","dashed","twodash","dotted")
n.es = length(es.labels)

## read in wetland ecosystem service estimates
eco.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df) = c("county","region",es.labels)

# scale ecosystem service data
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(scale.eco.df[,3:6], center=T, scale=T)

## read in unprotected wetland percent estimates
unpro.df = read.csv("Dual-Risk-Repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
unpro.df = unpro.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                       "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]

# wetland flood-frequency cutoffs
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)
colnames(unpro.df) = c("county",water.reg.labels)

# scale unrpotected wetland percents
scale.unpro.df = unpro.df
scale.unpro.df[,water.reg.labels] = scale(scale.unpro.df[,water.reg.labels], center=T, scale=T)

## read in climate data
climate.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv")

# climate extreme.labels or thresholds
climate.vars = unique(climate.df$variable)
extreme.labels = c("Frost days","Days with max. temp. over\n100 deg F (38 deg C)",
                   "Consecutive dry days","Consecutive wet days")
n.ex = length(climate.vars)

# isolate SSP3-7.0 for 2041-2070
ssp370_2041_2070.df = subset(climate.df, ssp == "SSP3-7.0" & time == "2041-2070")

# unmelt climate df
climate.wide.df = pivot_wider(ssp370_2041_2070.df, 
                              names_from = variable, 
                              values_from = value)

# invert sign of frost day
climate.wide.df[,"Frost days"] = -climate.wide.df[,"Frost days"]

## join climate and ecosystem service dataframe
eco.clim.df = subset(inner_join(eco.df, climate.wide.df, by=c("county","region")), 
                     select=-c(ssp,time,ssp_time))
unpro.eco.clim.df = inner_join(eco.clim.df, unpro.df, by=c("county"))

# create indicator dataframe for counties with values >= percentile value
q.k = 50
ind.df = unpro.eco.clim.df
col.percentiles = apply(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)], 
                        2, quantile, 
                        probs = c(q.k/100))
n.col = ncol(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)])
for (i in 1:nrow(unpro.eco.clim.df)) {
  for (j in 1:n.col) {
    if (unpro.eco.clim.df[i,j+2] >= col.percentiles[j]) {
      ind.df[i,j+2] = 1
    } else {
      ind.df[i,j+2] = 0
    }
  }
}

# shorten column names
colnames(ind.df) = c("county","region",
                     "plantN","herpN","carbon","flood",
                     "fd","mxt100","cdd","cwd",
                     "mean_pf","mean_ie","mean_spf","mean_sf")

# write to file
write.csv(ind.df[,c("county","region","plantN","herpN","carbon","flood")],
          paste("Dual-Risk-Repo/County_Summaries/Ecosystem_Services_",q.k,"th_Percentile.csv",sep=""), 
          row.names=F)
write.csv(ind.df[,c("county","region","fd","mxt100","cdd","cwd")],
          paste("Dual-Risk-Repo/County_Summaries/Climate_Extremes_",q.k,"th_Percentile.csv",sep=""), 
          row.names=F)
write.csv(ind.df[,c("county","region","mean_pf","mean_ie","mean_spf","mean_sf")],
          paste("Dual-Risk-Repo/County_Summaries/Unprotected_Percents_",q.k,"th_Percentile.csv",sep=""), 
          row.names=F)

################################################################################
# high service x high risk

# plotting parameters
text.size = 12
line.width = 1

# number of percentciles
percentiles = seq(0, 100, by=1)
n.p = length(percentiles)

# all ssps
ssps = unique(climate.df$ssp)
n.s = length(ssps)

# all time intervals
times = unique(climate.df$time)
n.t = length(times)

## make dataframe counting intersecting counties for each ssp x time interval
ex.es.un.count.df = data.frame(matrix(nrow=0, ncol=8))
colnames(ex.es.un.count.df) = c("ssp","time","ssp_time","percentile",
                                "extreme","service","cutoff","count")
# loop through each SSP
for (i in 1:n.s) {
  ssp.i = ssps[i]
  
  # loop through each time interval
  for (j in 1:n.t) {
    time.j = times[j]
    
    # isolate climate data for a given SSP x time interval combination
    climate.ssp.time.ij = subset(climate.df, ssp == ssp.i & time == time.j)
    
    # pivot climate dataframe from long to wide format
    climate.spp.time.wide.ij = pivot_wider(climate.ssp.time.ij, 
                                           names_from = variable, 
                                           values_from = value)
    
    # invert sign for frost days
    climate.spp.time.wide.ij[,"Frost days"] = -climate.spp.time.wide.ij[,"Frost days"]
    
    # join ecosystem service and climate extreme data
    eco.climate.ij = subset(inner_join(eco.df, 
                                       climate.spp.time.wide.ij, 
                                       by=c("county","region")), 
                            select=-c(ssp, time, ssp_time))
    
    # join unprotected wetland area data
    eco.clim.unpro.ij = left_join(eco.climate.ij, unpro.df, by="county")
    
    # loop through each extreme
    for (k in 1:n.ex) {
      ex.k = extreme.labels[k]
      
      # loop through each service
      for (l in 1:n.es) {
        es.l = es.labels[l]
        
        # loop through each wetland flood-frequency cutoff
        for (m in 1:n.w) {
          fc.m = water.reg.labels[m]
          
          # make new dataframe for each ssp, time, extreme, service, and cutoff combination
          count.df.ijklm = data.frame(matrix(nrow=n.p, ncol=8))
          colnames(count.df.ijklm) = c("ssp","time","ssp_time","extreme",
                                       "service","cutoff","percentile","count")
          count.df.ijklm$ssp = ssp.i
          count.df.ijklm$time = time.j
          count.df.ijklm$ssp_time = paste(ssp.i, time.j, sep=" x ")
          count.df.ijklm$extreme = ex.k
          count.df.ijklm$service = es.l
          count.df.ijklm$cutoff = fc.m
          count.df.ijklm$percentile = percentiles
          
          # loop through each percentile
          for (n in 1:n.p) {
            ex.percentile.kn = quantile(eco.clim.unpro.ij[,ex.k], probs = percentiles[n]/100)
            es.percentile.ln = quantile(eco.clim.unpro.ij[,es.l], probs = percentiles[n]/100)
            un.percentile.mn = quantile(eco.clim.unpro.ij[,fc.m], probs = percentiles[n]/100)
            ex.ind.kn = 1*(eco.clim.unpro.ij[,ex.k] >= ex.percentile.kn)
            es.ind.ln = 1*(eco.clim.unpro.ij[,es.l] >= es.percentile.ln)
            un.ind.mn = 1*(eco.clim.unpro.ij[,fc.m] >= un.percentile.mn)
            count.df.ijklm[n,"count"] = sum(ex.ind.kn == 1 & es.ind.ln == 1 & un.ind.mn == 1)
          }
          ex.es.un.count.df = rbind(ex.es.un.count.df, count.df.ijklm)
        }
      }
    }
  }
}

# filter by water regime
ex.es.un.count.df.wr = subset(ex.es.un.count.df, cutoff == water.reg.labels[3])

# remove non-meaningful flood storage columns
ex.es.un.count.df.wr.filter = subset(ex.es.un.count.df.wr, !((service == "Floodwater storage capacity") & (extreme %in% extreme.labels[1:3])))

# plot absolute number of counties by ssp, time interval, and service
p.ex.es.un.percentiles = ggplot(ex.es.un.count.df.wr.filter,
                                aes(x=percentile,
                                    y=log10(count),
                                    color=factor(service, levels=es.labels),
                                    linetype=factor(service, levels=es.labels))) +
                                geom_line(size=line.width) + 
                                scale_color_manual(values=es.colors) +
                                scale_linetype_manual(values=es.lines) +
                                labs(x="Risk & ecosystem service percentile",
                                     y="log10(Number of intersecting counties)",
                                     color="Ecosystem service",
                                     linetype="Ecosystem service") +
                                theme(text=element_text(size = text.size)) +
                                facet_grid(ssp_time ~ factor(extreme, levels=extreme.labels)) +
                                scale_x_continuous(limits=c(0,100)) +
                                scale_y_continuous(limits=c(0,2.1)) +
                                guides(color = guide_legend(
                                       keywidth = unit(0.5,"cm"), 
                                       keyheight = unit(0.5,"cm")))
p.ex.es.un.percentiles
ggsave(paste("Manuscript/Main_Figures/Figure3_High_Risk_High_Service_Percentiles_",
             gsub(" ","_",water.reg.labels[3]),".jpeg", sep=""),
       plot=p.ex.es.un.percentiles, width=28, height=20, units="cm", dpi=800)    

comp.df = pivot_wider(subset(ex.es.un.count.df.wr.filter, extreme == extreme.labels[4]),
                    names_from = service, 
                    values_from = count)
print(subset(comp.df,(`Carbon storage` > `Herpetofauna species richness`) & (ssp == ssps[2] & time == times[2]))[,c("ssp_time","percentile")], n=100)

ggplot(ex.es.un.count.df.wr,
       aes(x=percentile,
           y=log10(count),
           color=factor(extreme, levels=extreme.labels),
           linetype=factor(extreme, levels=extreme.labels))) +
          geom_line(size=line.width) + 
          scale_color_manual(values=es.colors) +
          scale_linetype_manual(values=es.lines) +
          labs(x="Risk & ecosystem service percentile",
               y="log10(Number of intersecting counties)",
               color="Ecosystem service",
               linetype="Ecosystem service") +
          theme(text=element_text(size = text.size)) +
          facet_grid(ssp_time ~ factor(service, levels=es.labels)) +
          scale_x_continuous(limits=c(0,100)) +
          scale_y_continuous(limits=c(0,2.1)) +
          guides(color = guide_legend(
            keywidth = unit(0.5,"cm"), 
            keyheight = unit(0.5,"cm")))
# calculate and plot pairwise differences
#ex.un.es.diff.df = data.frame(matrix(nrow=0, ncol=5))
#colnames(ex.un.es.diff.df) = c("ssp_time","percentile","es1","es2","diff")
#for (i in 1:n.s) {
#  ssp.i = ssps[i]
#  for (j in 1:n.t) {
#    time.j = times[j]
#    df.ij = subset(ex.es.un.count.df, ssp == ssp.i & time == time.j)
#    for (k in 1:(n.es-1)) {
#      es.k = es.labels[k]
#      for (l in (k+1):n.es) {
#        es.l = es.labels[l]
#        df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
#        colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
#        df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
#        df.ijkl$percentile = percentiles
#        df.ijkl$es1 = es.labels[k]
#        df.ijkl$es2 = es.labels[l]
#        df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
#        ex.un.es.diff.df = rbind(ex.un.es.diff.df, df.ijkl)
#      }
#    }
#  }
#}

#p.ex.un.es.diff = ggplot(ex.un.es.diff.df,
#                         aes(x=percentile,
#                             y=diff,
#                             color=factor(es1, levels=es.labels[1:3]),
#                             linetype=factor(es1, levels=es.labels[1:3]))) +
#                    geom_line(linewidth=line.width) + 
#                    scale_color_manual(values=es.colors[1:3]) +
#                    scale_linetype_manual(values=es.lines[1:3]) +
#                    geom_hline(yintercept=0,color="black") +
#                    facet_grid(factor(es2, levels=es.labels[2:4]) ~ ssp_time) +
#                    guides(color = guide_legend(keywidth = unit(1,"cm"), 
#                                                keyheight = unit(1,"cm"))) +
#                    scale_x_continuous(limits=c(50,100)) +
#                    scale_y_continuous(limits=c(-20,20)) +
#                    labs(color="Ecosystem service",
#                         linetype="Ecosystem service",
#                         y="Difference in log10(Number of intersecting counties)",
#                         x="Conversion risk, climate risk, & ecosystem service percentile")
#p.ex.un.es.diff

#ggsave("Manuscript/Supp_Figures/FigureD3_High_Unprotected_Climate_Extreme_High_Service_Percentile_Differences.jpeg", 
#       plot=ex.es.un.plots[[water.reg.labels[3]]], width=30, height=16, units="cm", dpi=600)


# make venn diagram for count of ES x unprotected area x climate extreme county intersections at a given percentile
eco.clim.df = subset(inner_join(eco.df, climate.wide.df, by=c("county","region")), 
                     select=-c(ssp,time,ssp_time))
unpro.eco.clim.df = inner_join(eco.clim.df, unpro.df, by=c("county"))
q.k = 50
ind.df = unpro.eco.clim.df
col.percentiles = apply(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)], 2, quantile, probs = c(q.k/100))
n.col = ncol(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)])
for (i in 1:nrow(unpro.eco.clim.df)) {
  for (j in 1:n.col) {
    if (unpro.eco.clim.df[i,j+2] < col.percentiles[j]) {
      ind.df[i,j+2] = 0
    } else {
      ind.df[i,j+2] = 1
    }
  }
}
colnames(ind.df) = c("county","region",
                     "plantN","herpN","carbon","flood",
                     "fd","mxt100","cdd","cwd",
                     "mean_pf","mean_ie","mean_spf","mean_sf")

un.ex.ind.df = subset(ind.df, (mean_spf == 1) & (fd == 1 | mxt100 == 1 | cdd == 1 | cwd == 1))
venn.un.ex = draw.quad.venn(
  area1 = sum(un.ex.ind.df$plantN == 1), 
  area2 = sum(un.ex.ind.df$herpN == 1),  
  area3 = sum(un.ex.ind.df$carbon == 1),  
  area4 = sum(un.ex.ind.df$flood == 1), 
  n12 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1), 
  n13 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$carbon == 1), 
  n14 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$flood == 1), 
  n23 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1),  
  n24 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$flood == 1),  
  n34 = sum(un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1), 
  n123 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1),  
  n124 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$flood == 1),   
  n134 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1),
  n234 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1),
  n1234 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1), 
  category = c("Plant species\nrichness","Herpetofauna species\nrichness",
               "Carbon storage","Floodwater storage\ncapacity"),
  fill = es.colors,
  cat.col = es.colors,
  cat.cex = 1.1, margin = 0.05, cex = 1.3, ind = TRUE)
venn.un.ex

all.four.ind = un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1
sort(un.ex.ind.df[all.four.ind,"county"])

#ggsave("Manuscript/Supp_Figures/FigureD4_High_Risk_High_Service_Venn.jpeg", 
#       plot=venn.un.ex, width=20, height=16, units="cm", dpi=600)

################################################################################
# low service x high risk

text.size = 12
line.width = 1

# make dataframe counting intersecting counties for each ssp x time interval
## make dataframe counting intersecting counties for each ssp x time interval
ex.es.un.count.df = data.frame(matrix(nrow=0, ncol=8))
colnames(ex.es.un.count.df) = c("ssp","time","ssp_time","percentile",
                                "extreme","service","cutoff","count")
# loop through each SSP
for (i in 1:n.s) {
  ssp.i = ssps[i]
  
  # loop through each time interval
  for (j in 1:n.t) {
    time.j = times[j]
    
    # isolate climate data for a given SSP x time interval combination
    climate.ssp.time.ij = subset(climate.df, ssp == ssp.i & time == time.j)
    
    # pivot climate dataframe from long to wide format
    climate.spp.time.wide.ij = pivot_wider(climate.ssp.time.ij, 
                                           names_from = variable, 
                                           values_from = value)
    
    # invert sign for frost days
    climate.spp.time.wide.ij[,"Frost days"] = -climate.spp.time.wide.ij[,"Frost days"]
    
    # join ecosystem service and climate extreme data
    eco.climate.ij = subset(inner_join(eco.df, 
                                       climate.spp.time.wide.ij, 
                                       by=c("county","region")), 
                            select=-c(ssp, time, ssp_time))
    
    # join unprotected wetland area data
    eco.clim.unpro.ij = left_join(eco.climate.ij, unpro.df, by="county")
    
    # loop through each extreme
    for (k in 1:n.ex) {
      ex.k = extreme.labels[k]
      
      # loop through each service
      for (l in 1:n.es) {
        es.l = es.labels[l]
        
        # loop through each wetland flood-frequency cutoff
        for (m in 1:n.w) {
          fc.m = water.reg.labels[m]
          
          # make new dataframe for each ssp, time, extreme, service, and cutoff combination
          count.df.ijklm = data.frame(matrix(nrow=n.p, ncol=8))
          colnames(count.df.ijklm) = c("ssp","time","ssp_time","extreme",
                                       "service","cutoff","percentile","count")
          count.df.ijklm$ssp = ssp.i
          count.df.ijklm$time = time.j
          count.df.ijklm$ssp_time = paste(ssp.i, time.j, sep=" x ")
          count.df.ijklm$extreme = ex.k
          count.df.ijklm$service = es.l
          count.df.ijklm$cutoff = fc.m
          count.df.ijklm$percentile = percentiles
          
          # loop through each percentile
          for (n in 1:n.p) {
            ex.percentile.kn = quantile(eco.clim.unpro.ij[,ex.k], probs = percentiles[n]/100)
            es.percentile.ln = quantile(eco.clim.unpro.ij[,es.l], probs = (100-percentiles[n])/100)
            un.percentile.mn = quantile(eco.clim.unpro.ij[,fc.m], probs = percentiles[n]/100)
            ex.ind.kn = 1*(eco.clim.unpro.ij[,ex.k] >= ex.percentile.kn)
            es.ind.ln = 1*(eco.clim.unpro.ij[,es.l] < es.percentile.ln)
            un.ind.mn = 1*(eco.clim.unpro.ij[,fc.m] >= un.percentile.mn)
            count.df.ijklm[n,"count"] = sum(ex.ind.kn == 1 & es.ind.ln == 1 & un.ind.mn == 1)
          }
          ex.es.un.count.df = rbind(ex.es.un.count.df, count.df.ijklm)
        }
      }
    }
  }
}
#print(subset(ex.es.un.count.df,(`Carbon storage` >= `Herpetofauna species richness`) & (percentile > 50))[,c("ssp_time","percentile")])

# filter by water regime
ex.es.un.count.df.wr = subset(ex.es.un.count.df, cutoff == water.reg.labels[3])

# remove non-meaningful flood storage columns
ex.es.un.count.df.wr.filter = subset(ex.es.un.count.df.wr, !((service == "Floodwater storage capacity") & (extreme %in% extreme.labels[1:3])))

# plot absolute number of counties by ssp, time interval, and service
p.ex.es.un.percentiles = ggplot(ex.es.un.count.df.wr.filter,
                                aes(x=percentile,
                                    y=log10(count),
                                    color=factor(service, levels=es.labels),
                                    linetype=factor(service, levels=es.labels))) +
                                geom_line(size=line.width) + 
                                scale_color_manual(values=es.colors) +
                                scale_linetype_manual(values=es.lines) +
                                labs(x="Risk & ecosystem service percentile",
                                     y="log10(Number of intersecting counties)",
                                     color="Ecosystem service",
                                     linetype="Ecosystem service") +
                                theme(text=element_text(size = text.size)) +
                                facet_grid(ssp_time ~ factor(extreme, levels=extreme.labels)) +
                                scale_x_continuous(limits=c(0,100)) +
                                scale_y_continuous(limits=c(0,2.1)) +
                                guides(color = guide_legend(
                                  keywidth = unit(0.5,"cm"), 
                                  keyheight = unit(0.5,"cm")))
p.ex.es.un.percentiles
ggsave(paste("Manuscript/Main_Figures/Figure4_High_Risk_Low_Service_Percentiles_",
             gsub(" ","_",water.reg.labels[3]),".jpeg", sep=""),
       plot=p.ex.es.un.percentiles, width=28, height=20, units="cm", dpi=800)    


# calculate and plot pairwise differences
#ex.un.es.diff.df = data.frame(matrix(nrow=0, ncol=5))
#colnames(ex.un.es.diff.df) = c("ssp_time","percentile","es1","es2","diff")
#for (i in 1:n.s) {
#  ssp.i = ssps[i]
#  for (j in 1:n.t) {
#    time.j = times[j]
#    df.ij = subset(ex.es.un.count.df, ssp == ssp.i & time == time.j)
#    for (k in 1:(n.es-1)) {
#      es.k = es.labels[k]
#      for (l in (k+1):n.es) {
#        es.l = es.labels[l]
#        df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
#        colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
#        df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
#        df.ijkl$percentile = percentiles
#        df.ijkl$es1 = es.labels[k]
#        df.ijkl$es2 = es.labels[l]
#        df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
#        ex.un.es.diff.df = rbind(ex.un.es.diff.df, df.ijkl)
#      }
#    }
#  }
#}
  
#p.ex.un.es.diff = ggplot(ex.un.es.diff.df,
#                         aes(x=percentile,
#                             y=diff,
#                             color=factor(es1, levels=es.labels[1:3]),
#                             linetype=factor(es1, levels=es.labels[1:3]))) +
#                          geom_line(linewidth=line.width) + 
#                          scale_color_manual(values=es.colors[1:3]) +
#                          scale_linetype_manual(values=es.lines[1:3]) +
#                          geom_hline(yintercept=0,color="black") +
#                          facet_grid(factor(es2, levels=es.labels[2:4]) ~ ssp_time) +
#                          guides(color = guide_legend(keywidth = unit(1,"cm"), 
#                                                      keyheight = unit(1,"cm")))  +
#                          scale_x_continuous(limits=c(50,100)) +
#                          scale_y_continuous(limits=c(-20,20)) +
#                          labs(color="Ecosystem service",
#                               linetype="Ecosystem service",
#                               y="Difference in log10(Number of intersecting counties)",
#                               x="Conversion risk, climate risk, & ecosystem service percentile")
#ex.es.un.plots[[water.reg.labels[w]]] = p.ex.un.es.diff

#ggsave("Manuscript/Supp_Figures/FigureD7_High_Unprotected_Climate_Extreme_Low_Service_Percentile_Differences.jpeg", 
#       plot=ex.es.un.plots[[water.reg.labels[3]]], width=30, height=16, units="cm", dpi=600)

# make venn diagram for count of ES x unprotected area x climate extreme county intersections at a given percentile
eco.clim.df = subset(inner_join(eco.df, climate.wide.df, by=c("county","region")), 
                     select=-c(ssp,time,ssp_time))
unpro.eco.clim.df = inner_join(eco.clim.df, unpro.df, by=c("county"))
q.k = 50
ind.df = unpro.eco.clim.df
lower.col.percentiles = apply(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)], 2, quantile, probs = q.k/100)
upper.col.percentiles = apply(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)], 2, quantile, probs = (100-q.k)/100)
n.col = ncol(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)])
for (i in 1:nrow(unpro.eco.clim.df)) {
  for (j in 1:n.col) {
    if (j+2 <= 6) {
      if (unpro.eco.clim.df[i,j+2] <= lower.col.percentiles[j]) {
        ind.df[i,j+2] = 1
      } else {
        ind.df[i,j+2] = 0
      }
    } else {
      if (unpro.eco.clim.df[i,j+2] >= upper.col.percentiles[j]) {
        ind.df[i,j+2] = 1
      } else {
        ind.df[i,j+2] = 0
      }
    }
  }
}
colnames(ind.df) = c("county","region",
                     "plantN","herpN","carbon","flood",
                     "fd","mxt100","cdd","cwd",
                     "mean_pf","mean_ie","mean_spf","mean_sf")

un.ex.ind.df = subset(ind.df, (mean_spf == 1) & (fd == 1 | mxt100 == 1 | cdd == 1 | cwd == 1))
venn.un.ex = draw.quad.venn(
  area1 = sum(un.ex.ind.df$plantN == 1), 
  area2 = sum(un.ex.ind.df$herpN == 1),  
  area3 = sum(un.ex.ind.df$carbon == 1),  
  area4 = sum(un.ex.ind.df$flood == 1), 
  n12 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1), 
  n13 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$carbon == 1), 
  n14 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$flood == 1), 
  n23 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1),  
  n24 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$flood == 1),  
  n34 = sum(un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1), 
  n123 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1),  
  n124 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$flood == 1),   
  n134 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1),
  n234 = sum(un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1),
  n1234 = sum(un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1), 
  category = c("Plant species\nrichness","Herpetofauna species\nrichness",
               "Carbon storage","Floodwater storage\ncapacity"),
  fill = es.colors,
  cat.col = es.colors,
  cat.cex = 1.1, margin = 0.05, cex = 1.3, ind = TRUE)
venn.un.ex

all.four.ind = un.ex.ind.df$plantN == 1 & un.ex.ind.df$herpN == 1 & un.ex.ind.df$carbon == 1 & un.ex.ind.df$flood == 1
sort(un.ex.ind.df[all.four.ind,"county"])


#ggsave("Manuscript/Supp_Figures/FigureD8_High_Risk_Low_Service_Venn.jpeg", 
#       plot=venn.un.ex, width=20, height=16, units="cm", dpi=600)
