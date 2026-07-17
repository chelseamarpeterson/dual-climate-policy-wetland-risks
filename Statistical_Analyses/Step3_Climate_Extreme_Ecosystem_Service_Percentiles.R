setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(VennDiagram)
library(reshape2)
library(patchwork)
library(RColorBrewer)
library(deeptime)

# read in wetland ecosystem service estimates
eco.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df)[1:2] = c("county","region")
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(scale.eco.df[,3:6], center=T, scale=T)
es.vars = colnames(scale.eco.df)[3:6]
n.es = length(es.vars)
es.labels1 = c("Plant species richness","Herpetofauna species richness",
               "Carbon storage","Floodwater storage capacity")
es.labels2 = c("Plant species richness","Herpetofauna species richness",
               "Carbon storage","-(Floodwater storage capacity)")

# read in Unprotected wetland percent estimates
unpro.df = read.csv("Dual-Risk-Repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
unpro.df = unpro.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff",
                       "mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)
colnames(unpro.df) = c("county",water.reg.labels)
scale.unpro.df = unpro.df
scale.unpro.df[,water.reg.labels] = scale(scale.unpro.df[,water.reg.labels], center=T, scale=T)

# read in climate data
climate.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv")
climate.vars = unique(climate.df$variable)
clim.var.order = c("Frost days","Days with max. temp. over\n100 deg F (38 deg C)",
                   "Consecutive wet days","Consecutive dry days")
n.v = length(climate.vars)

# isolate SSP3-7.0 for 2041-2070
ssp370_2041_2070.df = subset(climate.df, ssp == "SSP3-7.0" & time == "2041-2070")

# unmelt climate df
climate.wide.df = pivot_wider(ssp370_2041_2070.df, 
                              names_from = variable, 
                              values_from = value)

# invert sign of frost day
climate.wide.df[,"Frost days"] = -climate.wide.df[,"Frost days"]

# join climate and ecosystem service dataframe
eco.clim.df = subset(inner_join(eco.df, climate.wide.df, by=c("county","region")), 
                     select=-c(ssp,time,ssp_time))
unpro.eco.clim.df = inner_join(eco.clim.df, unpro.df, by=c("county"))

# create indicator dataframe for counties with values >= percentile value
q.k = 25
ind.df = unpro.eco.clim.df
col.percentiles = apply(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)], 2, quantile, probs = c(q.k/100))
n.col = ncol(unpro.eco.clim.df[3:ncol(unpro.eco.clim.df)])
for (i in 1:nrow(unpro.eco.clim.df)) {
  for (j in 1:n.col) {
    if (unpro.eco.clim.df[i,j+2] <= col.percentiles[j]) {
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

# number of ES x unpro intersecting counties as function of percentiles
text.size = 24
line.width = 2

percentiles = seq(0, 100, by=1)
n.p = length(percentiles)
un.es.count.df = data.frame(matrix(nrow=0, ncol=2+n.es))
colnames(un.es.count.df) = c("cutoff","percentile",es.vars)
for (i in 1:n.w) {
  count.df.i = data.frame(matrix(nrow=n.p, ncol=2+n.es))
  colnames(count.df.i) = c("cutoff","percentile",es.vars)
  count.df.i$cutoff = water.reg.labels[i]
  count.df.i$percentile = percentiles
  for (j in 1:n.es) {
    for (k in 1:n.p) {
      unpro.percentile.k = quantile(unpro.df[,water.reg.labels[i]], probs=percentiles[k]/100)
      es.percentile.k = quantile(eco.df[,es.vars[j]], probs = percentiles[k]/100)
      unpro.ind.k = 1*(unpro.df[,water.reg.labels[i]] >= unpro.percentile.k)
      es.ind.k = 1*(eco.df[,es.vars[j]] >= es.percentile.k)
      count.df.i[k,es.vars[j]] = sum(unpro.ind.k == 1 & es.ind.k == 1)
    }
  }
  un.es.count.df = rbind(un.es.count.df, count.df.i)
}

sum(nrow(subset(un.es.count.df, (Carbon.storage > Herpetofauna.species.richness) & (cutoff == water.reg.labels[4] & percentile >= 0))))
sum(nrow(subset(un.es.count.df, (Carbon.storage == Herpetofauna.species.richness) & (cutoff == water.reg.labels[4] & percentile >= 0))))
sum(nrow(subset(un.es.count.df, (Carbon.storage < Herpetofauna.species.richness) & (cutoff == water.reg.labels[4] & percentile >= 0))))


# plot number of counties by wetland cutoff
colnames(un.es.count.df)[3:6] = es.labels1
un.es.count.melt = melt(un.es.count.df, id.vars = c("cutoff","percentile"))
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
es.colors = c("darkgreen","purple4","orangered","royalblue4")
es.lines = c("solid","dashed","twodash","dotted")
p.un.es.percentiles = ggplot(un.es.count.melt,
                      aes(x=percentile,
                          y=value,
                          color=variable,
                          linetype=variable)) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors) +
                      scale_linetype_manual(values=es.lines) +
                      labs(x="Conversion risk\n& ecosystem service percentile",
                           y="Number of intersecting counties",
                           color="Ecosystem service",
                           linetype="Ecosystem service") +
                      theme(text=element_text(size = text.size),
                            legend.position = "none") + 
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(0,50)) +
                      guides(color = guide_legend(keywidth = unit(1, "cm"), 
                                                  keyheight = unit(1, "cm"))) +
                      facet_wrap(.~factor(cutoff, levels=rev(water.reg.labels)), ncol=1)
p.un.es.percentiles

# calculate and plot pairwise differences
un.es.diff.df = data.frame(matrix(nrow=0,ncol=5))
colnames(un.es.diff.df) = c("cutoff","percentile","es1","es2","diff")
for (i in 1:n.w) {
  wr.i = water.reg.labels[i]
  df.i = subset(un.es.count.df, cutoff == wr.i)
  for (j in 1:(n.es-1)) {
    es.j = es.labels1[j]
    for (k in (j+1):n.es) {
      es.k = es.labels1[k]
      df.ijk = data.frame(matrix(nrow=n.p,ncol=5))
      colnames(df.ijk) = c("cutoff","percentile","es1","es2","diff")
      df.ijk$cutoff = wr.i
      df.ijk$percentile = percentiles
      df.ijk$es1 = es.labels1[j]
      df.ijk$es2 = es.labels1[k]
      df.ijk$diff = df.i[,es.j] - df.i[,es.k]
      un.es.diff.df = rbind(un.es.diff.df, df.ijk)
    }
  }
}

p.un.es.diff = ggplot(un.es.diff.df,
                      aes(x=percentile,
                          y=diff,
                          color=factor(es1, levels=es.labels1[1:3]),
                          linetype=factor(es1, levels=es.labels1[1:3]))) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors[1:3]) +
                      scale_linetype_manual(values=es.lines[1:3]) +
                      geom_hline(yintercept=0,color="black") +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(-20,20)) +
                      facet_grid(factor(es2, levels=es.labels1[2:4]) ~ factor(cutoff, levels=water.reg.labels)) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm"))) +
                      labs(color="Ecosystem service",
                           linetype="Ecosystem service",
                           y="Difference in number of intersecting counties",
                           x="Conversion risk & ecosystem service percentile")
p.un.es.diff

#ggsave("Manuscript/Supp_Figures/FigureD1_High_Unprotected_High_Service_Percentile_Differences.jpeg", 
#       plot=p.un.es.diff, width=30, height=16, units="cm", dpi=600)

## number of ES x climate extremes intersecting counties as function of percentiles

# all ssps
ssps = unique(climate.df$ssp)
n.s = length(ssps)

# all time intervals
times = unique(climate.df$time)
n.t = length(times)

# make dataframe counting intersecting counties for each ssp x time interval
es.extremes = c("Frost days","Days with max. temp. over\n100 deg F (38 deg C)",
                "Consecutive wet days","Consecutive dry days")
ex.es.count.df = data.frame(matrix(nrow=0, ncol=4+n.es))
colnames(ex.es.count.df) = c("ssp","time","ssp_time","percentile",es.labels1)
for (k in 1:n.s) {
  for (l in 1:n.t) {
    climate.ssp.time.df = subset(climate.df, ssp == ssps[k] & time == times[l])
    climate.spp.time.wide = pivot_wider(climate.ssp.time.df, 
                                        names_from = variable, 
                                        values_from = value)
    eco.climate.df = subset(inner_join(eco.df, climate.spp.time.wide, 
                                       by=c("county","region")), 
                            select=-c(ssp, time, ssp_time))
    eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
    ex.es.count.df.kl = data.frame(matrix(nrow=n.p, ncol=4+n.es))
    colnames(ex.es.count.df.kl) = c("ssp","time","ssp_time","percentile",es.labels1)
    ex.es.count.df.kl$ssp = ssps[k]
    ex.es.count.df.kl$time = times[l]
    ex.es.count.df.kl$ssp_time = paste(ssps[k], times[l], sep=" x ")
    ex.es.count.df.kl$percentile = percentiles
    for (i in 1:n.es) {
      for (j in 1:n.p) {
        ex.percentile.j = quantile(eco.climate.df[,es.extremes[i]],
                                   probs = percentiles[j]/100)
        es.percentile.j = quantile(eco.climate.df[,es.vars[i]],
                                   probs = percentiles[j]/100)
        ex.ind.j = 1*(eco.climate.df[,es.extremes[i]] >= ex.percentile.j)
        es.ind.j = 1*(eco.climate.df[,es.vars[i]] >= es.percentile.j)
        ex.es.count.df.kl[j,es.labels1[i]] = sum(ex.ind.j == 1 & es.ind.j == 1)
      }
    }
    ex.es.count.df = rbind(ex.es.count.df, ex.es.count.df.kl)
  }
}

ex.es.count.melt = melt(ex.es.count.df, 
                        id.vars = c("ssp","time","ssp_time","percentile"))
p.ex.es.percentiles = ggplot(ex.es.count.melt,
                      aes(x=percentile,
                          y=value,
                          color=variable,
                          linetype=variable)) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values = es.colors) +
                      scale_linetype_manual(values = es.lines) +
                      labs(x="Climate risk\n& ecosystem service percentile",
                           y="",
                           color="Ecosystem service",
                           linetype="Ecosystem service") +
                      theme(text=element_text(size = text.size),
                            legend.position = "none",
                            axis.text.y = element_blank()) +
                      facet_wrap(.~ssp_time, ncol=1) +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(0,50)) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm")))
p.ex.es.percentiles

# calculate and plot pairwise differences
ex.es.diff.df = data.frame(matrix(nrow=0, ncol=5))
colnames(ex.es.diff.df) = c("ssp_time","percentile","es1","es2","diff")
for (i in 1:n.s) {
  ssp.i = ssps[i]
  for (j in 1:n.t) {
    time.j = times[j]
    df.ij = subset(ex.es.count.df, ssp == ssp.i & time == time.j)
    for (k in 1:(n.es-1)) {
      es.k = es.labels1[k]
      for (l in (k+1):n.es) {
        es.l = es.labels1[l]
        df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
        colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
        df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
        df.ijkl$percentile = percentiles
        df.ijkl$es1 = es.labels1[k]
        df.ijkl$es2 = es.labels1[l]
        df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
        ex.es.diff.df = rbind(ex.es.diff.df, df.ijkl)
      }
    }
  }
}

p.ex.es.diff = ggplot(ex.es.diff.df,
                      aes(x=percentile,
                          y=diff,
                          color=factor(es1, levels=es.labels1[1:3]),
                          linetype=factor(es1, levels=es.labels1[1:3]))) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors[1:3]) +
                      scale_linetype_manual(values=es.lines[1:3]) +
                      geom_hline(yintercept=0,color="black") +
                      facet_grid(factor(es2, levels=es.labels1[2:4]) ~ ssp_time) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm"))) +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(-20,20)) +
                      labs(color="Ecosystem service",
                           linetype="Ecosystem service",
                           y="Difference in number of intersecting counties",
                           x="Climate risk & ecosystem service percentile")
p.ex.es.diff

#ggsave("Manuscript/Supp_Figures/FigureD2_High_Climate_Extreme_High_Service_Percentile_Differences.jpeg", 
#       plot=p.ex.es.diff, width=30, height=16, units="cm", dpi=600)

## number of ES x climate extremes x unpro area intersecting counties as function of percentiles

ex.es.un.plots = list()
for (w in 1:n.w) {
  # make dataframe counting intersecting counties for each ssp x time interval
  ex.es.un.count.df = data.frame(matrix(nrow=0, ncol=4+n.es))
  colnames(ex.es.un.count.df) = c("ssp","time","ssp_time","percentile",es.labels1)
  for (k in 1:n.s) {
    for (l in 1:n.t) {
      climate.ssp.time.df = subset(climate.df, ssp == ssps[k] & time == times[l])
      climate.spp.time.wide = pivot_wider(climate.ssp.time.df, names_from = variable, 
                                          values_from = value)
      eco.climate.df = subset(inner_join(eco.df, climate.spp.time.wide, by=c("county","region")), 
                              select=-c(ssp, time, ssp_time))
      eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
      eco.clim.unpro.df = left_join(eco.climate.df, unpro.df, by="county")
      ex.es.un.count.df.kl = data.frame(matrix(nrow=n.p, ncol=4+n.es))
      colnames(ex.es.un.count.df.kl) = c("ssp","time","ssp_time","percentile",es.labels1)
      ex.es.un.count.df.kl$ssp = ssps[k]
      ex.es.un.count.df.kl$time = times[l]
      ex.es.un.count.df.kl$ssp_time = paste(ssps[k], times[l], sep=" x ")
      ex.es.un.count.df.kl$percentile = percentiles
      for (i in 1:n.es) {
        for (j in 1:n.p) {
          ex.percentile.j = quantile(eco.clim.unpro.df[,es.extremes[i]], probs = percentiles[j]/100)
          es.percentile.j = quantile(eco.clim.unpro.df[,es.vars[i]], probs = percentiles[j]/100)
          un.percentile.j = quantile(eco.clim.unpro.df[,water.reg.labels[w]], probs = percentiles[j]/100)
          ex.ind.j = 1*(eco.clim.unpro.df[,es.extremes[i]] >= ex.percentile.j)
          es.ind.j = 1*(eco.clim.unpro.df[,es.vars[i]] >= es.percentile.j)
          un.ind.j = 1*(eco.clim.unpro.df[,water.reg.labels[w]] >= un.percentile.j)
          ex.es.un.count.df.kl[j,es.labels1[i]] = sum(ex.ind.j == 1 & es.ind.j == 1 & un.ind.j == 1)
        }
      }
      ex.es.un.count.df = rbind(ex.es.un.count.df, ex.es.un.count.df.kl)
    }
  }
  #print(subset(ex.es.un.count.df,(`Carbon storage` >= `Herpetofauna species richness`) & (percentile > 50))[,c("ssp_time","percentile")])

  # plot absolute number of counties by ssp, time interval, and service
  ex.es.un.count.melt = melt(ex.es.un.count.df, 
                             id.vars = c("ssp","time","ssp_time","percentile"))
  p.ex.es.un.percentiles = ggplot(ex.es.un.count.melt,
                           aes(x=percentile,
                               y=value,
                               color=variable,
                               linetype=variable)) +
                           #geom_vline(xintercept=75, color="gray10") +
                           geom_line(size=line.width) + 
                           scale_color_manual(values=es.colors) +
                           scale_linetype_manual(values=es.lines) +
                           labs(x="Conversion risk, climate risk,\n& ecosystem service percentile",
                                y="",
                                color="Ecosystem service",
                                linetype="Ecosystem service") +
                           theme(text=element_text(size = text.size),
                                 axis.text.y = element_blank()) +
                           facet_wrap(.~ssp_time, ncol=1) +
                           scale_x_continuous(limits=c(50,100)) +
                           scale_y_continuous(limits=c(0,50)) +
                           guides(color = guide_legend(
                                  keywidth = unit(1,"cm"), 
                                  keyheight = unit(1,"cm")))
  p.ex.es.un.percentiles
  if (w == 3) {
    p.quantiles.poster = p.un.es.percentiles + p.ex.es.percentiles + p.ex.es.un.percentiles
    p.quantiles.poster
    ggsave("Presentations/Figure_All_Quantiles_High_Risk_High_Service.jpeg",
           plot=p.quantiles.poster, width=54, height=48, units="cm", dpi=600)
    #ggsave("Manuscript/Main_Figures/Figure2_High_Risk_High_Service_Percentiles.jpeg",
    #       plot=p.quantiles.poster, width=28, height=20, units="cm", dpi=600)
  }
  
  # calculate and plot pairwise differences
  ex.un.es.diff.df = data.frame(matrix(nrow=0, ncol=5))
  colnames(ex.un.es.diff.df) = c("ssp_time","percentile","es1","es2","diff")
  for (i in 1:n.s) {
    ssp.i = ssps[i]
    for (j in 1:n.t) {
      time.j = times[j]
      df.ij = subset(ex.es.un.count.df, ssp == ssp.i & time == time.j)
      for (k in 1:(n.es-1)) {
        es.k = es.labels1[k]
        for (l in (k+1):n.es) {
          es.l = es.labels1[l]
          df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
          colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
          df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
          df.ijkl$percentile = percentiles
          df.ijkl$es1 = es.labels1[k]
          df.ijkl$es2 = es.labels1[l]
          df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
          ex.un.es.diff.df = rbind(ex.un.es.diff.df, df.ijkl)
        }
      }
    }
  }

  p.ex.un.es.diff = ggplot(ex.un.es.diff.df,
                           aes(x=percentile,
                               y=diff,
                               color=factor(es1, levels=es.labels1[1:3]),
                               linetype=factor(es1, levels=es.labels1[1:3]))) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors[1:3]) +
                      scale_linetype_manual(values=es.lines[1:3]) +
                      geom_hline(yintercept=0,color="black") +
                      facet_grid(factor(es2, levels=es.labels1[2:4]) ~ ssp_time) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm"))) +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(-20,20)) +
                      labs(color="Ecosystem service",
                           linetype="Ecosystem service",
                           y="Difference in number of intersecting counties",
                           x="Conversion risk, climate risk, & ecosystem service percentile")
  ex.es.un.plots[[water.reg.labels[w]]] = p.ex.un.es.diff
}

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

# number of ES x unpro intersecting counties as function of percentiles
text.size = 12
line.width = 1

un.es.count.df2 = data.frame(matrix(nrow=0, ncol=2+n.es))
colnames(un.es.count.df2) = c("cutoff","percentile",es.vars)
for (i in 1:n.w) {
  count.df.i = data.frame(matrix(nrow=n.p, ncol=2+n.es))
  colnames(count.df.i) = c("cutoff","percentile",es.vars)
  count.df.i$cutoff = water.reg.labels[i]
  count.df.i$percentile = percentiles
  for (j in 1:n.es) {
    for (k in 1:n.p) {
      unpro.percentile.k = quantile(unpro.df[,water.reg.labels[i]], probs=percentiles[k]/100)
      es.percentile.k = quantile(eco.df[,es.vars[j]], probs = (100-percentiles[k])/100)
      unpro.ind.k = 1*(unpro.df[,water.reg.labels[i]] >= unpro.percentile.k)
      es.ind.k = 1*(eco.df[,es.vars[j]] <= es.percentile.k)
      count.df.i[k,es.vars[j]] = sum(unpro.ind.k == 1 & es.ind.k == 1)
    }
  }
  un.es.count.df2 = rbind(un.es.count.df2, count.df.i)
}

#sum(nrow(subset(un.es.count.df, (Carbon.storage >= Herpetofauna.species.richness) & cutoff == water.reg.labels[1])))
#sum(nrow(subset(un.es.count.df, (Herpetofauna.species.richness >= Floodwater.storage.capacity) & cutoff == water.reg.labels[4])))

# plot number of counties by wetland cutoff
colnames(un.es.count.df2)[3:6] = es.labels1
un.es.count.melt2 = melt(un.es.count.df2, id.vars = c("cutoff","percentile"))
blues = brewer.pal(n = 11, name = "BrBG")[seq(8,11)]
es.colors = c("darkgreen","purple4","orangered","royalblue4")
es.lines = c("solid","dashed","twodash","dotted")
p.un.es.percentiles2 = ggplot(un.es.count.melt2,
                             aes(x=percentile,
                                 y=value,
                                 color=variable,
                                 linetype=variable)) +
                            geom_line(linewidth=line.width) + 
                            scale_color_manual(values=es.colors) +
                            scale_linetype_manual(values=es.lines) +
                            labs(x="Conversion risk\n& ecosystem service percentile",
                                 y="Number of intersecting counties",
                                 color="Ecosystem service",
                                 linetype="Ecosystem service") +
                            scale_x_continuous(limits=c(50,100)) +
                            scale_y_continuous(limits=c(0,50)) +
                            theme(text=element_text(size = text.size),
                                  legend.position = "none") + 
                            guides(color = guide_legend(keywidth = unit(1, "cm"), 
                                                        keyheight = unit(1, "cm"))) +
                            facet_wrap(.~factor(cutoff, levels=rev(water.reg.labels)), ncol=1)
p.un.es.percentiles2

# calculate and plot pairwise differences
un.es.diff.df2 = data.frame(matrix(nrow=0,ncol=5))
colnames(un.es.diff.df2) = c("cutoff","percentile","es1","es2","diff")
for (i in 1:n.w) {
  wr.i = water.reg.labels[i]
  df.i = subset(un.es.count.df2, cutoff == wr.i)
  for (j in 1:(n.es-1)) {
    es.j = es.labels1[j]
    for (k in (j+1):n.es) {
      es.k = es.labels1[k]
      df.ijk = data.frame(matrix(nrow=n.p,ncol=5))
      colnames(df.ijk) = c("cutoff","percentile","es1","es2","diff")
      df.ijk$cutoff = wr.i
      df.ijk$percentile = percentiles
      df.ijk$es1 = es.labels1[j]
      df.ijk$es2 = es.labels1[k]
      df.ijk$diff = df.i[,es.j] - df.i[,es.k]
      un.es.diff.df2 = rbind(un.es.diff.df2, df.ijk)
    }
  }
}

p.un.es.diff2 = ggplot(un.es.diff.df2,
                      aes(x=percentile,
                          y=diff,
                          color=factor(es1, levels=es.labels1[1:3]),
                          linetype=factor(es1, levels=es.labels1[1:3]))) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors[1:3]) +
                      scale_linetype_manual(values=es.lines[1:3]) +
                      geom_hline(yintercept=0,color="black") +
                      facet_grid(factor(es2, levels=es.labels1[2:4]) ~ factor(cutoff, levels=water.reg.labels)) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm"))) +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(-20,20)) +
                      labs(color="Ecosystem service",
                           linetype="Ecosystem service",
                           y="Difference in number of intersecting counties",
                           x="Conversion risk & ecosystem service percentile")
p.un.es.diff2

ggsave("Manuscript/Supp_Figures/FigureD5_High_Unprotected_Low_Service_Percentile_Differences.jpeg", 
       plot=p.un.es.diff2, width=30, height=16, units="cm", dpi=600)

## number of ES x climate extremes intersecting counties as function of percentiles

# make dataframe counting intersecting counties for each ssp x time interval
ex.es.count.df2 = data.frame(matrix(nrow=0, ncol=4+n.es))
colnames(ex.es.count.df2) = c("ssp","time","ssp_time","percentile",es.labels1)
for (k in 1:n.s) {
  for (l in 1:n.t) {
    climate.ssp.time.df = subset(climate.df, ssp == ssps[k] & time == times[l])
    climate.spp.time.wide = pivot_wider(climate.ssp.time.df, 
                                        names_from = variable, 
                                        values_from = value)
    eco.climate.df = subset(inner_join(eco.df, climate.spp.time.wide, 
                                       by=c("county","region")), 
                            select=-c(ssp, time, ssp_time))
    eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
    ex.es.count.df.kl = data.frame(matrix(nrow=n.p, ncol=4+n.es))
    colnames(ex.es.count.df.kl) = c("ssp","time","ssp_time","percentile",es.labels1)
    ex.es.count.df.kl$ssp = ssps[k]
    ex.es.count.df.kl$time = times[l]
    ex.es.count.df.kl$ssp_time = paste(ssps[k], times[l], sep=" x ")
    ex.es.count.df.kl$percentile = percentiles
    for (i in 1:n.es) {
      for (j in 1:n.p) {
        ex.percentile.j = quantile(eco.climate.df[,es.extremes[i]],
                                   probs = percentiles[j]/100)
        es.percentile.j = quantile(eco.climate.df[,es.vars[i]],
                                   probs = (100-percentiles[j])/100)
        ex.ind.j = 1*(eco.climate.df[,es.extremes[i]] >= ex.percentile.j)
        es.ind.j = 1*(eco.climate.df[,es.vars[i]] <= es.percentile.j)
        ex.es.count.df.kl[j,es.labels1[i]] = sum(ex.ind.j == 1 & es.ind.j == 1)
      }
    }
    ex.es.count.df2 = rbind(ex.es.count.df2, ex.es.count.df.kl)
  }
}

ex.es.count.melt2 = melt(ex.es.count.df2, 
                        id.vars = c("ssp","time","ssp_time","percentile"))
p.ex.es.percentiles2 = ggplot(ex.es.count.melt2,
                              aes(x=percentile,
                                  y=value,
                                  color=variable,
                                  linetype=variable)) +
                              geom_line(linewidth=line.width) + 
                              scale_color_manual(values = es.colors) +
                              scale_linetype_manual(values = es.lines) +
                              labs(x="Climate risk\n& ecosystem service percentile",
                                   y="",
                                   color="Ecosystem service",
                                   linetype="Ecosystem service") +
                              theme(text=element_text(size = text.size),
                                    legend.position = "none",
                                    axis.text.y = element_blank()) +
                              facet_wrap(.~ssp_time, ncol=1) +
                              scale_x_continuous(limits=c(50,100)) +
                              scale_y_continuous(limits=c(0,50)) +
                              guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                          keyheight = unit(1,"cm")))
p.ex.es.percentiles2

# calculate and plot pairwise differences
ex.es.diff.df2 = data.frame(matrix(nrow=0, ncol=5))
colnames(ex.es.diff.df2) = c("ssp_time","percentile","es1","es2","diff")
for (i in 1:n.s) {
  ssp.i = ssps[i]
  for (j in 1:n.t) {
    time.j = times[j]
    df.ij = subset(ex.es.count.df2, ssp == ssp.i & time == time.j)
    for (k in 1:(n.es-1)) {
      es.k = es.labels1[k]
      for (l in (k+1):n.es) {
        es.l = es.labels1[l]
        df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
        colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
        df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
        df.ijkl$percentile = percentiles
        df.ijkl$es1 = es.labels1[k]
        df.ijkl$es2 = es.labels1[l]
        df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
        ex.es.diff.df2 = rbind(ex.es.diff.df2, df.ijkl)
      }
    }
  }
}

p.ex.es.diff2 = ggplot(ex.es.diff.df2,
                      aes(x=percentile,
                          y=diff,
                          color=factor(es1, levels=es.labels1[1:3]),
                          linetype=factor(es1, levels=es.labels1[1:3]))) +
                      geom_line(linewidth=line.width) + 
                      scale_color_manual(values=es.colors[1:3]) +
                      scale_linetype_manual(values=es.lines[1:3]) +
                      geom_hline(yintercept=0,color="black") +
                      facet_grid(factor(es2, levels=es.labels1[2:4]) ~ ssp_time) +
                      guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                  keyheight = unit(1,"cm"))) +
                      scale_x_continuous(limits=c(50,100)) +
                      scale_y_continuous(limits=c(-20,20)) +
                      labs(color="Ecosystem service",
                           linetype="Ecosystem service",
                           y="Difference in number of intersecting counties",
                           x="Climate risk & ecosystem service percentile")
p.ex.es.diff2

ggsave("Manuscript/Supp_Figures/FigureD6_High_Climate_Extreme_Low_Service_Percentile_Differences.jpeg", 
       plot=p.ex.es.diff2, width=28, height=16, units="cm", dpi=600)

## number of ES x climate extremes x unpro area intersecting counties as function of percentiles

ex.es.un.plots = list()
for (w in 1:n.w) {
  # make dataframe counting intersecting counties for each ssp x time interval
  ex.es.un.count.df = data.frame(matrix(nrow=0, ncol=4+n.es))
  colnames(ex.es.un.count.df) = c("ssp","time","ssp_time","percentile",es.labels1)
  for (k in 1:n.s) {
    for (l in 1:n.t) {
      climate.ssp.time.df = subset(climate.df, ssp == ssps[k] & time == times[l])
      climate.spp.time.wide = pivot_wider(climate.ssp.time.df, names_from = variable, 
                                          values_from = value)
      eco.climate.df = subset(inner_join(eco.df, climate.spp.time.wide, by=c("county","region")), 
                              select=-c(ssp, time, ssp_time))
      eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
      eco.clim.unpro.df = left_join(eco.climate.df, unpro.df, by="county")
      ex.es.un.count.df.kl = data.frame(matrix(nrow=n.p, ncol=4+n.es))
      colnames(ex.es.un.count.df.kl) = c("ssp","time","ssp_time","percentile",es.labels1)
      ex.es.un.count.df.kl$ssp = ssps[k]
      ex.es.un.count.df.kl$time = times[l]
      ex.es.un.count.df.kl$ssp_time = paste(ssps[k], times[l], sep=" x ")
      ex.es.un.count.df.kl$percentile = percentiles
      for (i in 1:n.es) {
        for (j in 1:n.p) {
          ex.percentile.j = quantile(eco.clim.unpro.df[,es.extremes[i]], probs = percentiles[j]/100)
          es.percentile.j = quantile(eco.clim.unpro.df[,es.vars[i]], probs = (100-percentiles[j])/100)
          un.percentile.j = quantile(eco.clim.unpro.df[,water.reg.labels[w]], probs = percentiles[j]/100)
          ex.ind.j = 1*(eco.clim.unpro.df[,es.extremes[i]] >= ex.percentile.j)
          es.ind.j = 1*(eco.clim.unpro.df[,es.vars[i]] <= es.percentile.j)
          un.ind.j = 1*(eco.clim.unpro.df[,water.reg.labels[w]] >= un.percentile.j)
          ex.es.un.count.df.kl[j,es.labels1[i]] = sum(ex.ind.j == 1 & es.ind.j == 1 & un.ind.j == 1)
        }
      }
      ex.es.un.count.df = rbind(ex.es.un.count.df, ex.es.un.count.df.kl)
    }
  }
  #print(subset(ex.es.un.count.df,(`Carbon storage` >= `Herpetofauna species richness`) & (percentile > 50))[,c("ssp_time","percentile")])
  
  # plot absolute number of counties by ssp, time interval, and service
  ex.es.un.count.melt = melt(ex.es.un.count.df, 
                             id.vars = c("ssp","time","ssp_time","percentile"))
  p.ex.es.un.percentiles2 = ggplot(ex.es.un.count.melt,
                                   aes(x=percentile,
                                       y=value,
                                       color=variable,
                                       linetype=variable)) +
                                   geom_line(size=line.width) + 
                                   scale_color_manual(values=es.colors) +
                                   scale_linetype_manual(values=es.lines) +
                                   labs(x="Conversion risk, climate risk,\n& ecosystem service percentile",
                                        y="",
                                        color="Ecosystem service",
                                        linetype="Ecosystem service") +
                                   theme(text=element_text(size = text.size),
                                         axis.text.y = element_blank()) +
                                   facet_wrap(.~ssp_time, ncol=1)  +
                                   scale_x_continuous(limits=c(50,100)) +
                                   scale_y_continuous(limits=c(0,50)) +
                                   guides(color = guide_legend(
                                          keywidth = unit(1,"cm"), 
                                          keyheight = unit(1,"cm")))
  p.ex.es.un.percentiles2
  if (w == 3) {
    p.quantiles.poster2 = p.un.es.percentiles2 + p.ex.es.percentiles2 + p.ex.es.un.percentiles2
    p.quantiles.poster2
    #ggsave("Presentations/Figure_All_Quantiles_High_Risk_Low_Service.jpeg",
    #       plot=p.quantiles.poster2, width=54, height=48,units="cm", dpi=600)
    ggsave("Manuscript/Main_Figures/Figure3_High_Risk_Low_Service_Percentiles.jpeg",
           plot=p.quantiles.poster2, width=28, height=20, units="cm", dpi=600)
  }
  
  #ggsave("Manuscript/Main_Figures/Figure2_Ecosystem_Service_Unprotected_Percent_Climate_Extreme_Percentiles.jpeg",
  #       plot=p.quantiles.poster, width=30, height=20, units="cm", dpi=600)
  
  
  # calculate and plot pairwise differences
  ex.un.es.diff.df = data.frame(matrix(nrow=0, ncol=5))
  colnames(ex.un.es.diff.df) = c("ssp_time","percentile","es1","es2","diff")
  for (i in 1:n.s) {
    ssp.i = ssps[i]
    for (j in 1:n.t) {
      time.j = times[j]
      df.ij = subset(ex.es.un.count.df, ssp == ssp.i & time == time.j)
      for (k in 1:(n.es-1)) {
        es.k = es.labels1[k]
        for (l in (k+1):n.es) {
          es.l = es.labels1[l]
          df.ijkl = data.frame(matrix(nrow=n.p, ncol=5))
          colnames(df.ijkl) = c("ssp_time","percentile","es1","es2","diff")
          df.ijkl$ssp_time = paste(ssp.i, time.j, sep=" x ")
          df.ijkl$percentile = percentiles
          df.ijkl$es1 = es.labels1[k]
          df.ijkl$es2 = es.labels1[l]
          df.ijkl$diff = df.ij[,es.k] - df.ij[,es.l]
          ex.un.es.diff.df = rbind(ex.un.es.diff.df, df.ijkl)
        }
      }
    }
  }
  
  p.ex.un.es.diff = ggplot(ex.un.es.diff.df,
                           aes(x=percentile,
                               y=diff,
                               color=factor(es1, levels=es.labels1[1:3]),
                               linetype=factor(es1, levels=es.labels1[1:3]))) +
                            geom_line(linewidth=line.width) + 
                            scale_color_manual(values=es.colors[1:3]) +
                            scale_linetype_manual(values=es.lines[1:3]) +
                            geom_hline(yintercept=0,color="black") +
                            facet_grid(factor(es2, levels=es.labels1[2:4]) ~ ssp_time) +
                            guides(color = guide_legend(keywidth = unit(1,"cm"), 
                                                        keyheight = unit(1,"cm")))  +
                            scale_x_continuous(limits=c(50,100)) +
                            scale_y_continuous(limits=c(-20,20)) +
                            labs(color="Ecosystem service",
                                 linetype="Ecosystem service",
                                 y="Difference in number of intersecting counties",
                                 x="Conversion risk, climate risk, & ecosystem service percentile")
  ex.es.un.plots[[water.reg.labels[w]]] = p.ex.un.es.diff
}

ggsave("Manuscript/Supp_Figures/FigureD7_High_Unprotected_Climate_Extreme_Low_Service_Percentile_Differences.jpeg", 
       plot=ex.es.un.plots[[water.reg.labels[3]]], width=30, height=16, units="cm", dpi=600)


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
