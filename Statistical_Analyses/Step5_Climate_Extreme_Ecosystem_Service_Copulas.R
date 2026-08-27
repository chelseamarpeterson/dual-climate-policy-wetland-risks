setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(reshape2)
library(ggfortify)
library(patchwork)
library(deming)
library(metR)
library(VineCopula)
library(RColorBrewer)
library(grid)
library(metR)
library(gridExtra)
library(cowplot)
library(ggtext)

# read in wetland ecosystem service estimates
eco.df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df)[1:2] = c("county","region")
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(scale.eco.df[,3:6], center=T, scale=T)
es.vars = colnames(scale.eco.df)[3:6]
n.es = length(es.vars)
es.labels = c("Plant species richness","Herpetofauna species richness",
              "Carbon storage","Floodwater storage capacity")
es.colors = c("darkgreen","purple4","orangered","royalblue4")
es.shapes = c("circle","square","diamond","triangle")

# read in unprotected wetland percent and area estimates
unpro.percent.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
unpro.area.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
unpro.percent.df = unpro.percent.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff","mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
unpro.area.df = unpro.area.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff","mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)
colnames(unpro.percent.df) = c("county",water.reg.labels)
colnames(unpro.area.df) = c("county",water.reg.labels)
scale.unpro.percent.df = unpro.percent.df
scale.unpro.area.df = unpro.area.df
scale.unpro.percent.df[,water.reg.labels] = scale(scale.unpro.percent.df[,water.reg.labels], center=T, scale=T)
scale.unpro.area.df[,water.reg.labels] = scale(scale.unpro.area.df[,water.reg.labels], center=T, scale=T)

# read in climate data
climate.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv")
climate.vars = unique(climate.df$variable)
climate.vars.order = c("-&Delta;(Frost days)",
                       "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
                       "&Delta;(Max. length dry spell)", 
                       "&Delta;(Max. length wet spell)")
n.ex = length(climate.vars)
climate.wide.df = pivot_wider(data = climate.df,
                              names_from = variable,
                              values_from = value)
scale.climate.df = climate.wide.df
scale.climate.df[,climate.vars] = scale(climate.wide.df[,climate.vars], center=T, scale=T)

# ssps
ssps = c("SSP2-4.5","SSP3-7.0")
n.s = length(ssps)

# time intervals
times = c("2041-2070","2071-2100")
n.t = length(times)
ssp.time.order = rev(c(paste(ssps[1], times, sep=" &times; "),
                       paste(ssps[2], times, sep=" &times; ")))

# join service and unprotected wetland area dataframes
scale.eco.unpro.percent.df = inner_join(scale.eco.df, scale.unpro.percent.df[,c("county",water.reg.labels)], by="county")
scale.eco.unpro.area.df = inner_join(scale.eco.df, scale.unpro.area.df[,c("county",water.reg.labels)], by="county")
#colnames(scale.eco.unpro.percent.df)[7] = "Unpro.percent.SPF"
#scale.eco.unpro.df = inner_join(scale.eco.unpro.percent.df, 
#                                scale.unpro.area.df[,c("county",water.reg.labels[3])], 
#                                by="county")
#colnames(scale.eco.unpro.df)[8] = "Unpro.area.SPF"
#scale.eco.unpro.clim.df = inner_join(scale.eco.unpro.df, 
#                                     subset(scale.climate.df, ssp == ssps[1] & time == times[1]), 
#                                     by=c("county","region"))
#colnames(scale.eco.unpro.clim.df)[12:15] = c("Frost.days","Max.temp.100F",
#                                             "Max.len.dry","Max.len.wet")
#write.csv(scale.eco.unpro.clim.df, "dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Areas_Climate_Extremes_SSP245_2041_2070_Scaled.csv")

# copulas that I plan to test
cop.names = c("Gaussian","t","Clayton")
cop.nums = seq(1,3)
all.cop.nums = list("1" = c(1),
                    "2" = c(2),
                    "3" = c(3,13,23,33))
n.c = length(cop.names)

################################################################################
# fit copulas for ecosystem services v. unprotected wetland percents

# make dataframes and lists
es.un.perc.fits = list()
es.un.perc.best.cop.number = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.perc.best.cop.family = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.perc.best.tail.dep.cross = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.perc.best.tail.dep.upper = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.perc.best.tau = data.frame(matrix(nrow=n.w, ncol=n.es))

# assign column names
colnames(es.un.perc.best.cop.number) = es.labels
colnames(es.un.perc.best.cop.family) = es.labels
colnames(es.un.perc.best.tail.dep.cross) = es.labels
colnames(es.un.perc.best.tail.dep.upper) = es.labels
colnames(es.un.perc.best.tau) = es.labels

# assign row names
row.names(es.un.perc.best.cop.number) = water.reg.labels
row.names(es.un.perc.best.cop.family) = water.reg.labels
row.names(es.un.perc.best.tail.dep.cross) = water.reg.labels
row.names(es.un.perc.best.tail.dep.upper) = water.reg.labels
row.names(es.un.perc.best.tau) = water.reg.labels

# identify copula that minimizes AIC
for (i in 1:n.es) {
  # make list for copula fits given a specific ES
  es.i = es.vars[i]
  es.un.perc.fits[[es.i]] = list()
  
  for(j in 1:n.w) {
    # get water regime label and make lists
    wr.j = water.reg.labels[j]
    es.un.perc.fits[[es.i]][[wr.j]] = list()
    
    # convert variables to ranks
    u = rank(scale.eco.unpro.percent.df[,wr.j]) / (length(scale.eco.unpro.percent.df[,wr.j]) + 1)
    v = rank(scale.eco.unpro.percent.df[,es.i]) / (length(scale.eco.unpro.percent.df[,es.i]) + 1) 
    v_inv = rank(-scale.eco.unpro.percent.df[,es.i]) / (length(scale.eco.unpro.percent.df[,es.i]) + 1) 
    
    # fit each copula model
    for (k in 1:n.c) { es.un.perc.fits[[es.i]][[wr.j]][[cop.names[k]]] = BiCopSelect(u, v, familyset = cop.nums[k]) }
    
    # compare copulas with AIC and BIC
    es.fits = es.un.perc.fits[[es.i]][[wr.j]]
    aic.df = sapply(list(es.fits[["Gaussian"]],es.fits[["t"]], es.fits[["Clayton"]]),
                    function(f) c(family = f$family, AIC = f$AIC))
    
    # save best copula number in matrix
    best.cop.number = as.integer(aic.df[1,which.min(aic.df[2,])])
    es.un.perc.best.cop.number[wr.j,es.labels[i]] = best.cop.number
    es.un.perc.best.tail.dep.cross[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u, v_inv, familyset = best.cop.number))$upper
    es.un.perc.best.tail.dep.upper[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u, v, familyset = best.cop.number))$upper
    es.un.perc.best.tau[wr.j,es.labels[i]] = BiCopSelect(u, v, familyset = best.cop.number)$tau
    for (k in 1:n.c) {
      potential.cop.nums = all.cop.nums[[k]]
      if (best.cop.number %in% potential.cop.nums) { best.cop.family = cop.names[k] }
    }
    es.un.perc.best.cop.family[wr.j,es.labels[i]] = best.cop.family
  }
}

################################################################################
# fit copulas for ecosystem services v. unprotected wetland areas

# make dataframes and lists
es.un.area.fits = list()
es.un.area.best.cop.number = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.area.best.cop.family = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.area.best.tail.dep.cross = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.area.best.tail.dep.upper = data.frame(matrix(nrow=n.w, ncol=n.es))
es.un.area.best.tau = data.frame(matrix(nrow=n.w, ncol=n.es))

# assign column names
colnames(es.un.area.best.cop.number) = es.labels
colnames(es.un.area.best.cop.family) = es.labels
colnames(es.un.area.best.tail.dep.cross) = es.labels
colnames(es.un.area.best.tail.dep.upper) = es.labels
colnames(es.un.area.best.tau) = es.labels

# assign row names
row.names(es.un.area.best.cop.number) = water.reg.labels
row.names(es.un.area.best.cop.family) = water.reg.labels
row.names(es.un.area.best.tail.dep.cross) = water.reg.labels
row.names(es.un.area.best.tail.dep.upper) = water.reg.labels
row.names(es.un.area.best.tau) = water.reg.labels

# identify copula that minimizes AIC
for (i in 1:n.es) {
  # make list for copula fits given a specific ES
  es.i = es.vars[i]
  es.un.area.fits[[es.i]] = list()
  
  for(j in 1:n.w) {
    # get water regime label and make lists
    wr.j = water.reg.labels[j]
    es.un.area.fits[[es.i]][[wr.j]] = list()
    
    # convert variables to ranks
    u = rank(scale.eco.unpro.area.df[,wr.j]) / (length(scale.eco.unpro.area.df[,wr.j]) + 1)
    v = rank(scale.eco.unpro.area.df[,es.i]) / (length(scale.eco.unpro.area.df[,es.i]) + 1) 
    v_inv = rank(-scale.eco.unpro.area.df[,es.i]) / (length(scale.eco.unpro.area.df[,es.i]) + 1) 
    
    # fit each copula model
    for (k in 1:n.c) { es.un.area.fits[[es.i]][[wr.j]][[cop.names[k]]] = BiCopSelect(u, v, familyset = cop.nums[k]) }
    
    # compare copulas with AIC and BIC
    es.fits = es.un.area.fits[[es.i]][[wr.j]]
    aic.df = sapply(list(es.fits[["Gaussian"]], es.fits[["t"]], es.fits[["Clayton"]]),
                    function(f) c(family = f$family, AIC = f$AIC))
    
    # save best copula number in matrix
    best.cop.number = as.integer(aic.df[1,which.min(aic.df[2,])])
    es.un.area.best.cop.number[wr.j,es.labels[i]] = best.cop.number
    es.un.area.best.tail.dep.cross[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u, v_inv, familyset = best.cop.number))$upper
    es.un.area.best.tail.dep.upper[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u, v, familyset = best.cop.number))$upper
    es.un.area.best.tau[wr.j,es.labels[i]] = BiCopSelect(u, v, familyset = best.cop.number)$tau
    for (k in 1:n.c) {
      potential.cop.nums = all.cop.nums[[k]]
      if (best.cop.number %in% potential.cop.nums) { best.cop.family = cop.names[k] }
    }
    es.un.area.best.cop.family[wr.j,es.labels[i]] = best.cop.family
  }
}

################################################################################
# copula comparison for ecosystem services v. climate extremes

# make dataframes and lists
ex.es.fits = list()
ex.es.best.cop.number = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.cop.family = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.cross = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.upper = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tau = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))

# update column names
colnames(ex.es.best.cop.number) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.cop.family) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.cross) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.upper) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tau) = c("ssp","time","extreme","service","value")

# fit copulas
n = 1
for (i in 1:n.s) {
  ssp.i = ssps[i]
  ex.es.fits[[ssp.i]] = list()
  for (j in 1:n.t) {
    time.j = times[j]
    ex.es.fits[[ssp.i]][[time.j]] = list()
    for (k in 1:n.es) {
      es.k = es.vars[k]
      ex.es.fits[[ssp.i]][[time.j]][[es.k]] = list()
      for (l in 1:n.ex) {
        ex.l = climate.vars.order[l]
        ex.es.fits[[ssp.i]][[time.j]][[es.k]][[ex.l]] = list()
        
        # join climate data for given ssp
        climate.df.ij = subset(subset(scale.climate.df, ssp == ssp.i & time == time.j),
                               select=-c(region, ssp_time))
        eco.climate.df = inner_join(scale.eco.df, climate.df.ij, by = "county")
        
        # prepare data (U and V must be nonexceedance probabilities between 0 and 1)
        u = rank(eco.climate.df[,ex.l]) / (length(eco.climate.df[,ex.l]) + 1)
        v = rank(eco.climate.df[,es.k]) / (length(eco.climate.df[,es.k]) + 1)
        v_inv = rank(-eco.climate.df[,es.k]) / (length(eco.climate.df[,es.k]) + 1)
        
        # fit each copula model
        for (m in 1:n.c) {
          ex.es.fits[[ssp.i]][[time.j]][[es.k]][[ex.l]][[cop.names[m]]] = BiCopSelect(u, v, familyset = cop.nums[m])
        }
        
        # compare AICs manually
        ex.fits = ex.es.fits[[ssp.i]][[time.j]][[es.k]][[ex.l]]
        aic.df = sapply(list(ex.fits[["Gaussian"]],ex.fits[["t"]], ex.fits[["Clayton"]]),
                        function(f) c(family = f$family, AIC = f$AIC))

        # save best copula number in matrix
        best.cop.number = as.integer(aic.df[1,which.min(aic.df[2,])])
        ex.es.best.cop.number[n,"ssp"] = ssp.i
        ex.es.best.cop.number[n,"time"] = time.j
        ex.es.best.cop.number[n,"service"] = es.labels[k]
        ex.es.best.cop.number[n,"extreme"] = ex.l
        ex.es.best.cop.number[n,"value"] = best.cop.number
        
        ex.es.best.tail.dep.upper[n,"ssp"] = ssp.i
        ex.es.best.tail.dep.upper[n,"time"] = time.j
        ex.es.best.tail.dep.upper[n,"service"] = es.labels[k]
        ex.es.best.tail.dep.upper[n,"extreme"] = ex.l
        ex.es.best.tail.dep.upper[n,"value"] = BiCopPar2TailDep(BiCopSelect(u, v, familyset = best.cop.number))$upper  
        
        ex.es.best.tail.dep.cross[n,"ssp"] = ssp.i
        ex.es.best.tail.dep.cross[n,"time"] = time.j
        ex.es.best.tail.dep.cross[n,"service"] = es.labels[k]
        ex.es.best.tail.dep.cross[n,"extreme"] = ex.l
        ex.es.best.tail.dep.cross[n,"value"] = BiCopPar2TailDep(BiCopSelect(u, v_inv, familyset = best.cop.number))$upper    
        
        ex.es.best.tau[n,"ssp"] = ssp.i
        ex.es.best.tau[n,"time"] = time.j
        ex.es.best.tau[n,"service"] = es.labels[k]
        ex.es.best.tau[n,"extreme"] = ex.l
        ex.es.best.tau[n,"value"] = BiCopSelect(u, v, familyset = best.cop.number)$tau
        
        for (m in 1:n.c) {
          potential.cop.nums = all.cop.nums[[m]]
          if (best.cop.number %in% potential.cop.nums) { best.cop.family = cop.names[m] }
        }
        ex.es.best.cop.family[n,"ssp"] = ssp.i
        ex.es.best.cop.family[n,"time"] = time.j
        ex.es.best.cop.family[n,"service"] = es.labels[k]
        ex.es.best.cop.family[n,"extreme"] = ex.l
        ex.es.best.cop.family[n,"value"] = best.cop.family
        n = n + 1
      }
    }
  }
}

################################################################################
# upper tail dependence plots

es.un.area.best.tail.dep.upper$metric = "Unprotected wetland area"
es.un.perc.best.tail.dep.upper$metric = "Unprotected wetland percentage"
es.un.area.best.tail.dep.upper$cutoff = water.reg.labels
es.un.perc.best.tail.dep.upper$cutoff = water.reg.labels
es.un.best.tail.dep.upper = rbind(es.un.area.best.tail.dep.upper,
                                  es.un.perc.best.tail.dep.upper)
es.un.tail.dep.upper.melt = melt(es.un.best.tail.dep.upper, id.var=c("cutoff","metric"))
p.un.es.upper = ggplot(es.un.tail.dep.upper.melt,
                       aes(x=value,
                           y=factor(cutoff, levels=water.reg.labels),
                           color=factor(variable, levels=es.labels),
                           shape=factor(variable, levels=es.labels),
                           size=factor(variable, levels=es.labels))) +
                       geom_point() + 
                       xlim(0,0.6) +
                       scale_size_manual(values=c(3,3,4,3)) +
                       scale_color_manual(values=es.colors) +
                       scale_shape_manual(values=es.shapes) +
                       theme(legend.position = "none",
                             text = element_text(size=16),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) +
                       facet_wrap(.~metric, ncol=1) +
                       labs(y="Wetland flood-frequency cutoff",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Tail dependence coefficient (high risk &times; high service)")
p.un.es.upper

ex.es.best.tail.dep.upper$ssp_time = paste(ex.es.best.tail.dep.upper$ssp,
                                           ex.es.best.tail.dep.upper$time,
                                           sep = " &times; ")
p.ex.es.upper = ggplot(ex.es.best.tail.dep.upper,
                       aes(x=value,
                           y=factor(ssp_time, levels=ssp.time.order),
                           color=factor(service, levels=es.labels),
                           shape=factor(service, levels=es.labels),
                           size=factor(service, levels=es.labels))) +
                       geom_point() +
                       facet_wrap(.~extreme) +
                      scale_size_manual(values=c(3,3,4,3)) +
                      scale_color_manual(values=es.colors) +
                      scale_shape_manual(values=es.shapes) +
                      theme(text = element_text(size=16),
                            strip.text = element_markdown(),
                            axis.text.y = element_markdown(),
                            axis.title.x = element_markdown()) + 
                      xlim(0,0.6) +
                      labs(y="Shared socioeconomic pathway\nby climatology period",
                           color="Ecosystem service",
                           shape="Ecosystem service",
                           size="Ecosystem service",
                           x="Tail dependence coefficient (high risk &times; high service)")
p.upper = p.un.es.upper + p.ex.es.upper + plot_layout(widths = c(1,2), axis_titles = "collect")
p.upper
ggsave("Manuscript/Main_Figures/Figure5_Bivariate_Copulas_Upper_Tail_Dependence.jpeg", 
       plot=p.upper, width=48, height=18, units="cm", dpi=800)

################################################################################
# cross tail dependence plots

es.un.area.best.tail.dep.cross$metric = "Unprotected wetland area"
es.un.perc.best.tail.dep.cross$metric = "Unprotected wetland percentage"
es.un.area.best.tail.dep.cross$cutoff = water.reg.labels
es.un.perc.best.tail.dep.cross$cutoff = water.reg.labels
es.un.best.tail.dep.cross = rbind(es.un.area.best.tail.dep.cross,
                                  es.un.perc.best.tail.dep.cross)
es.un.tail.dep.cross.melt = melt(es.un.best.tail.dep.cross, id.var=c("cutoff","metric"))
p.un.es.cross = ggplot(es.un.tail.dep.cross.melt,
                       aes(x=value,
                           y=factor(cutoff, levels=water.reg.labels),
                           color=factor(variable, levels=es.labels),
                           shape=factor(variable, levels=es.labels),
                           size=factor(variable, levels=es.labels))) +
                       geom_point() + 
                       xlim(0,0.6) +
                       scale_size_manual(values=c(3,3,4,3)) +
                       scale_color_manual(values=es.colors) +
                       scale_shape_manual(values=es.shapes) +
                       theme(legend.position = "none",
                             text = element_text(size=16),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) +
                       facet_wrap(.~metric, ncol=1) +
                       labs(y="Wetland flood-frequency cutoff",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Tail dependence coefficient (high risk &times; low service)")
p.un.es.cross

ex.es.best.tail.dep.cross$ssp_time = paste(ex.es.best.tail.dep.cross$ssp,
                                           ex.es.best.tail.dep.cross$time,
                                           sep = " &times; ")
p.ex.es.cross = ggplot(ex.es.best.tail.dep.cross,
                       aes(x=value,
                           y=factor(ssp_time, levels=ssp.time.order),
                           color=factor(service, levels=es.labels),
                           shape=factor(service, levels=es.labels),
                           size=factor(service, levels=es.labels))) +
                       geom_point() +
                       facet_wrap(.~extreme) +
                       scale_size_manual(values=c(3,3,4,3)) +
                       scale_color_manual(values=es.colors) +
                       scale_shape_manual(values=es.shapes) +
                       theme(text = element_text(size=16),
                             strip.text = element_markdown(),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) + 
                       xlim(0,0.6) +
                       labs(y="Shared socioeconomic pathway\nby climatology period",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Tail dependence coefficient (high risk &times; low service)")
p.cross = p.un.es.cross + p.ex.es.cross + plot_layout(widths = c(1,2), axis_titles = "collect")
p.cross
ggsave("Manuscript/Main_Figures/Figure6_Bivariate_Copulas_Cross_Tail_Dependence.jpeg", 
       plot=p.cross, width=48, height=18, units="cm", dpi=800)

################################################################################
# kendall tau plots

es.un.area.best.tau$metric = "Unprotected wetland area"
es.un.perc.best.tau$metric = "Unprotected wetland percentage"
es.un.area.best.tau$cutoff = water.reg.labels
es.un.perc.best.tau$cutoff = water.reg.labels
es.un.best.tau = rbind(es.un.area.best.tau,
                       es.un.perc.best.tau)
es.un.tau.melt = melt(es.un.best.tau, id.var=c("cutoff","metric"))
p.un.es.tau = ggplot(es.un.tau.melt) +
                     geom_vline(xintercept=0, color="gray25", linetype="dashed") +
                     geom_point(aes(x=value,
                                    y=factor(cutoff, levels=water.reg.labels),
                                    color=factor(variable, levels=es.labels),
                                    shape=factor(variable, levels=es.labels),
                                    size=factor(variable, levels=es.labels))) + 
                     scale_size_manual(values=c(3,3,4,3)) +
                     scale_color_manual(values=es.colors) +
                     scale_shape_manual(values=es.shapes) +
                     theme(legend.position = "none",
                           text = element_text(size=14),
                           axis.text.y = element_markdown(),
                           axis.title.x = element_markdown()) +
                     facet_wrap(.~metric, ncol=1) +
                     labs(y="Wetland flood-frequency cutoff",
                          color="Ecosystem service",
                          shape="Ecosystem service",
                          size="Ecosystem service",
                          x="Kendall's &tau;") + xlim(-0.55,0.55)
p.un.es.tau

ex.es.best.tau$ssp_time = paste(ex.es.best.tau$ssp,
                                ex.es.best.tau$time,
                                sep = " &times; ")
p.ex.es.tau = ggplot(ex.es.best.tau) +
                     geom_vline(xintercept=0, color="gray25", linetype="dashed") +
                     geom_point(aes(x=value,
                                    y=factor(ssp_time, levels=ssp.time.order),
                                    color=factor(service, levels=es.labels),
                                    shape=factor(service, levels=es.labels),
                                    size=factor(service, levels=es.labels))) +
                     facet_wrap(.~extreme) +
                     scale_size_manual(values=c(3,3,4,3)) +
                     scale_color_manual(values=es.colors) +
                     scale_shape_manual(values=es.shapes) +
                     theme(text = element_text(size=14),
                           strip.text = element_markdown(),
                           axis.text.y = element_markdown(),
                           axis.title.x = element_markdown()) + 
                     labs(y="Shared socioeconomic pathway\nby climatology period",
                          color="Ecosystem service",
                          shape="Ecosystem service",
                          size="Ecosystem service",
                          x="Kendall's &tau;") + xlim(-0.55,0.55)
p.tau = p.un.es.tau + p.ex.es.tau + plot_layout(widths = c(1,2), axis_titles = "collect")
p.tau
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD4_Bivariate_Copulas_Tau.jpeg", 
       plot=p.tau, width=43, height=16, units="cm", dpi=600)

################################################################################
# lack of protection bivariate copula plots

# unprotected percents
un_perc_best_bicop_model_list = list()
un_perc_melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(un_perc_melted_density_all) = c("x","y","value","service")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.es) {
  # get model with least AIC/BIC
  es.i = es.vars[i]
  un_perc_best_bicop_model_list[[es.i]] = es.un.perc.fits[[es.i]][[water.reg.labels[3]]][[es.un.perc.best.cop.family[i,3]]]
  
  # create grid sequence
  density_matrix = outer(grid_seq, grid_seq, 
                         function(x, y) {BiCopPDF(pnorm(x), pnorm(y), un_perc_best_bicop_model_list[[es.vars[i]]]) * dnorm(x) * dnorm(y)})
  dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
  
  # melt dataframe for plotting
  melted_density = melt(density_matrix, varnames = c("x", "y"))
  melted_density$service = es.labels[i]
  
  # upend to large matrix
  un_perc_melted_density_all = rbind(un_perc_melted_density_all, melted_density)
}

# apply rank transformation to full dataframe: U ~ U(0,1)
u.eco.unpro.percent.df = scale.eco.unpro.percent.df[,c(es.vars,water.reg.labels[3])]
u.cols = colnames(u.eco.unpro.percent.df)
for (i in 1:length(u.cols)) {
  u.eco.unpro.percent.df[,u.cols[i]] = rank(scale.eco.unpro.percent.df[,u.cols[i]]) / (length(scale.eco.unpro.percent.df[,u.cols[i]]) + 1)
}

# apply the inverse cumulative distribution fuction of the standard normal distribution
z.eco.unpro.percent.df = u.eco.unpro.percent.df
for (i in 1:length(u.cols)) {
  z.eco.unpro.percent.df[,u.cols[i]] = qnorm(z.eco.unpro.percent.df[,u.cols[i]])
}
colnames(z.eco.unpro.percent.df)[1:4] = es.labels
z.eco.unpro.percent.melt = melt(z.eco.unpro.percent.df, id.vars=c(water.reg.labels[3]))
colnames(z.eco.unpro.percent.melt) = c("x","service","y")

# make plots by service
p1.un.perc = ggplot() +
             metR::geom_contour_fill(data=subset(un_perc_melted_density_all, service == "Plant species richness"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Greens",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.percent.melt, service == "Plant species richness"),
                        aes(x=x,
                            y=y),
                        color="darkgreen", size=0.5) +
             labs(x = "Unprotected wetland percent (z)",
                  y = "Ecosystem service (z)",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p1.un.perc 
p2.un.perc = ggplot() +
             metR::geom_contour_fill(data=subset(un_perc_melted_density_all, service == "Herpetofauna species richness"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Purples",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.percent.melt, service == "Herpetofauna species richness"),
                        aes(x=x,
                            y=y),
                        color="darkorchid4", size=0.5) +
             labs(x = "Unprotected wetland percent (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p2.un.perc
p3.un.perc = ggplot() +
             metR::geom_contour_fill(data=subset(un_perc_melted_density_all, service == "Carbon storage"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Oranges",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.percent.melt, service == "Carbon storage"),
                        aes(x=x,
                            y=y),
                        color="darkorange3", size=0.5) +
             labs(x = "Unprotected wetland percent (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p3.un.perc
p4.un.perc = ggplot() +
             metR::geom_contour_fill(data=subset(un_perc_melted_density_all, service == "Floodwater storage capacity"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Blues",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.percent.melt, service == "Floodwater storage capacity"),
                        aes(x=x,
                            y=y),
                        color="steelblue4", size=0.5) +
             labs(x = "Unprotected wetland percent (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p4.un.perc
strip_axes = theme(axis.title.x = element_blank(),
                   text = element_text(size=12),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))
combo.un.perc = plot_grid(p1.un.perc + strip_axes,
                          p2.un.perc + strip_axes,
                          p3.un.perc + strip_axes,
                          p4.un.perc + strip_axes, nrow = 1)
combo.un.perc.lab = ggdraw(combo.un.perc) + 
                      draw_label("Unprotected wetland percent (z)", 
                                 y = 0.02, x = 0.5, 
                                 angle=0, vjust = 0.8, size=12) + 
                      theme(plot.margin = margin(5, 5, 10, 10))

# unprotected areas
un_area_best_bicop_model_list = list()
un_area_melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(un_area_melted_density_all) = c("x","y","value","service")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.es) {
  # get model with least AIC/BIC
  es.i = es.vars[i]
  un_area_best_bicop_model_list[[es.i]] = es.un.area.fits[[es.i]][[water.reg.labels[3]]][[es.un.area.best.cop.family[i,3]]]
  
  # create grid sequence
  density_matrix = outer(grid_seq, grid_seq, 
                         function(x, y) {BiCopPDF(pnorm(x), pnorm(y), un_area_best_bicop_model_list[[es.vars[i]]]) * dnorm(x) * dnorm(y)})
  dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
  
  # melt dataframe for plotting
  melted_density = melt(density_matrix, varnames = c("x", "y"))
  melted_density$service = es.labels[i]
  
  # upend to large matrix
  un_area_melted_density_all = rbind(un_area_melted_density_all, melted_density)
}

# apply rank transformation to full dataframe: U ~ U(0,1)
u.eco.unpro.area.df = scale.eco.unpro.area.df[,c(es.vars,water.reg.labels[3])]
u.cols = colnames(u.eco.unpro.area.df)
for (i in 1:length(u.cols)) {
  u.eco.unpro.area.df[,u.cols[i]] = rank(scale.eco.unpro.area.df[,u.cols[i]]) / (length(scale.eco.unpro.area.df[,u.cols[i]]) + 1)
}

# apply the inverse cumulative distribution fuction of the standard normal distribution
z.eco.unpro.area.df = u.eco.unpro.area.df
for (i in 1:length(u.cols)) {
  z.eco.unpro.area.df[,u.cols[i]] = qnorm(u.eco.unpro.area.df[,u.cols[i]])
}
colnames(z.eco.unpro.area.df)[1:4] = es.labels
z.eco.unpro.area.melt = melt(z.eco.unpro.area.df, id.vars=c(water.reg.labels[3]))
colnames(z.eco.unpro.area.melt) = c("x","service","y")

# make plots by service
p1.un.area = ggplot() +
             metR::geom_contour_fill(data=subset(un_area_melted_density_all, service == "Plant species richness"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Greens",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.area.melt, service == "Plant species richness"),
                        aes(x=x,
                            y=y),
                        color="darkgreen", size=0.5) +
             labs(x = "Unprotected wetland area (z)",
                  y = "Ecosystem service (z)",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p1.un.area
p2.un.area = ggplot() +
             metR::geom_contour_fill(data=subset(un_area_melted_density_all, service == "Herpetofauna species richness"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Purples",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.area.melt, service == "Herpetofauna species richness"),
                        aes(x=x,
                            y=y),
                        color="darkorchid4", size=0.5) +
             labs(x = "Unprotected wetland area (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p2.un.area
p3.un.area = ggplot() +
             metR::geom_contour_fill(data=subset(un_area_melted_density_all, service == "Carbon storage"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Oranges",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.area.melt, service == "Carbon storage"),
                        aes(x=x,
                            y=y),
                        color="darkorange3", size=0.5) +
             labs(x = "Unprotected wetland area (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p3.un.area
p4.un.area = ggplot() +
             metR::geom_contour_fill(data=subset(un_area_melted_density_all, service == "Floodwater storage capacity"), 
                                     aes(x = x, y = y, z = value),
                                     breaks=seq(0,0.3,length.out=10)) +
             scale_fill_distiller(palette = "Blues",
                                  direction = 2,
                                  limits=c(0,0.31)) +
             coord_equal() +
             geom_point(data=subset(z.eco.unpro.area.melt, service == "Floodwater storage capacity"),
                        aes(x=x,
                            y=y),
                        color="steelblue4", size=0.5) +
             labs(x = "Unprotected wetland percent (z)",
                  y = "",
                  color = "Density", fill = "Density") +
             facet_wrap(.~service)
p4.un.area
strip_axes = theme(axis.title.x = element_blank(),
                   text = element_text(size=12),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))
combo.un.perc = plot_grid(p1.un.perc + strip_axes,
                          p2.un.perc + strip_axes,
                          p3.un.perc + strip_axes,
                          p4.un.perc + strip_axes, nrow = 1)
combo.un.perc.lab = ggdraw(combo.un.perc) + 
  draw_label("Unprotected wetland percent (z)", 
             y = 0.02, x = 0.5, 
             angle=0, vjust = 0.8, size=12) + 
  theme(plot.margin = margin(5, 5, 10, 10))

#ggsave("Manuscript/Supp_Figures/AppendixD/FigureD1_Bivariate_Copulas_Services_Versus_Unprotected_Percents_Areas.jpeg", 
#       combo.un.perc.lab, width = 16, height = 3.5, dpi = 600)

################################################################################
# climate extreme bivariate copula plots

# chosen ssp and time combination for plotting
ssp1 = ssps[1]
time1 = times[1]
ex_best_bicop_model_list = list()
ex_melted_density_all = data.frame(matrix(nrow=0, ncol=5))
colnames(ex_melted_density_all) = c("x","y","value","service","extreme")
for (i in 1:n.es) {
  es.i = es.labels[i]
  ex_best_bicop_model_list[[es.i]] = list()
  
  for (j in 1:n.ex) {
    ex.j = climate.vars.order[j]
    
    # get model with least AIC/BIC
    ex.es.best.cop.family.ij = subset(ex.es.best.cop.family, (ssp == ssp1 & time == time1) & (service == es.i & extreme == ex.j))$value
    ex_best_bicop_model_list[[es.i]][[ex.j]] = ex.es.fits[[ssp]][[time]][[es.vars[i]]][[ex.j]][[ex.es.best.cop.family.ij]]
    
    # create grid sequence
    density_matrix = outer(grid_seq, grid_seq, 
                           function(x, y) {BiCopPDF(pnorm(x), pnorm(y), ex_best_bicop_model_list[[es.i]][[ex.j]]) * dnorm(x) * dnorm(y)})
    dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
    
    # melt dataframe for plotting
    ex_melted_density = melt(density_matrix, varnames = c("x", "y"))
    ex_melted_density$service = es.i
    ex_melted_density$extreme = ex.j
    
    # upend to large matrix
    ex_melted_density_all = rbind(ex_melted_density_all, ex_melted_density)
  }
}

# decrease in frost days
p1.fd = ggplot(subset(ex_melted_density_all, 
               service == "Plant species richness" & extreme == climate.vars.order[1]), 
               aes(x = x, y = y, z = value)) +
               metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
               scale_fill_distiller(palette = "Greens",
                                    direction = 2,
                                    limits=c(0,0.25)) +
               coord_equal() +
               labs(x = climate.vars.order[1],
                    y = "Ecosystem service (z)",
                    color = "Density", fill = "Density") +
               facet_wrap(.~service)
p2.fd = ggplot(subset(ex_melted_density_all, 
               service == "Herpetofauna species richness" & extreme == climate.vars.order[1]),
               aes(x = x, y = y, z = value)) +
               metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
               scale_fill_distiller(palette = "Purples",
                                    direction = 2,
                                    limits=c(0,0.25)) +
               coord_equal() +
               labs(x = climate.vars.order[1],
                    y = "", color = "Density", fill = "Density") +
               facet_wrap(.~service)
p3.fd = ggplot(subset(ex_melted_density_all, 
               service == "Carbon storage" & extreme == climate.vars.order[1]),
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Oranges",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = climate.vars.order[1],
                 y = "", color = "Density", fill = "Density") +
            facet_wrap(.~service)
p4.fd = ggplot(subset(ex_melted_density_all, 
               service == "Floodwater storage capacity" & extreme == climate.vars.order[1]), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Blues",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = climate.vars.order[1],
                 y = "", color = "Density", fill = "Density") +
            facet_wrap(.~service)
strip_axes = theme(axis.title.x = element_blank(),
                   axis.text.x = element_markdown(),
                   text = element_text(size=12),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))

combo.fd = plot_grid(p1.fd + strip_axes, p2.fd + strip_axes, p3.fd + strip_axes, p4.fd + strip_axes, nrow = 1)
combo.fd.lab = ggdraw(combo.fd) +
               draw_label(expression(-Delta * "(Frost days) (z)"), 
                          y = 0.02, x = 0.5, 
                          angle=0, vjust = 0.8, size=12) + 
               theme(plot.margin = margin(5, 5, 10, 10))
combo.fd.lab

# increase in maximum temperature over 100 degrees F
p1.mxt100 = ggplot(subset(ex_melted_density_all, 
                      service == "Plant species richness" & extreme == climate.vars.order[2]), 
                   aes(x = x, y = y, z = value)) +
                   metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                   scale_fill_distiller(palette = "Greens",
                                        direction = 2,
                                        limits=c(0,0.25)) +
                   coord_equal() +
                   labs(x = climate.vars.order[2],
                        y = "Ecosystem service (z)",
                        color = "Density", fill = "Density") +
                   facet_wrap(.~service)
p2.mxt100 = ggplot(subset(ex_melted_density_all, 
                      service == "Herpetofauna species richness" & extreme == climate.vars.order[2]),
                   aes(x = x, y = y, z = value)) +
                   metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                   scale_fill_distiller(palette = "Purples",
                                        direction = 2,
                                        limits=c(0,0.25)) +
                   coord_equal() +
                   labs(x = climate.vars.order[2],
                        y = "", color = "Density", fill = "Density") +
                   facet_wrap(.~service)
p3.mxt100 = ggplot(subset(ex_melted_density_all, 
                      service == "Carbon storage" & extreme == climate.vars.order[2]),
                   aes(x = x, y = y, z = value)) +
                   metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                   scale_fill_distiller(palette = "Oranges",
                                        direction = 2,
                                        limits=c(0,0.25)) +
                   coord_equal() +
                   labs(x = climate.vars.order[2],
                        y = "", color = "Density", fill = "Density") +
                   facet_wrap(.~service)
p4.mxt100 = ggplot(subset(ex_melted_density_all, 
                      service == "Floodwater storage capacity" & extreme == climate.vars.order[2]), 
                   aes(x = x, y = y, z = value)) +
                   metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                   scale_fill_distiller(palette = "Blues",
                                        direction = 2,
                                        limits=c(0,0.25)) +
                   coord_equal() +
                   labs(x = climate.vars.order[2],
                        y = "", color = "Density", fill = "Density") +
                   facet_wrap(.~service)
combo.mxt100 = plot_grid(p1.mxt100 + strip_axes, p2.mxt100 + strip_axes, p3.mxt100 + strip_axes, p4.mxt100 + strip_axes, nrow = 1)
combo.mxt100.lab = ggdraw(combo.mxt100) +
                      draw_label(expression(Delta * "(Days with max. temp. over 100°F [38°C]) (z)"), 
                                 y = 0.02, x = 0.5, 
                                 angle=0, vjust = 0.8, size=12) + 
                      theme(plot.margin = margin(5, 5, 10, 10))
combo.mxt100.lab
                
# increase in max. dry spell length
p1.mdsl = ggplot(subset(ex_melted_density_all, 
                 service == "Plant species richness" & extreme == climate.vars.order[3]), 
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Greens",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[3],
                      y = "Ecosystem service (z)",
                      color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p2.mdsl = ggplot(subset(ex_melted_density_all, 
                          service == "Herpetofauna species richness" & extreme == climate.vars.order[3]),
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Purples",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[3],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p3.mdsl = ggplot(subset(ex_melted_density_all, 
                          service == "Carbon storage" & extreme == climate.vars.order[3]),
                   aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Oranges",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[3],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p4.mdsl = ggplot(subset(ex_melted_density_all, 
                 service == "Floodwater storage capacity" & extreme == climate.vars.order[3]), 
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Blues",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[3],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
combo.mdsl = plot_grid(p1.mdsl + strip_axes, p2.mdsl + strip_axes, p3.mdsl + strip_axes, p4.mdsl + strip_axes, nrow = 1)
combo.mdsl.lab = ggdraw(combo.mdsl) +
                 draw_label(expression(Delta * "(Max. length dry spell) (z)"), 
                            y = 0.02, x = 0.5, 
                            angle=0, vjust = 0.8, size=12) + 
                 theme(plot.margin = margin(5, 5, 10, 10))
combo.mdsl.lab

# increase in max. wet spell length
p1.mwsl = ggplot(subset(ex_melted_density_all, 
                        service == "Plant species richness" & extreme == climate.vars.order[4]), 
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Greens",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[4],
                      y = "Ecosystem service (z)",
                      color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p2.mwsl = ggplot(subset(ex_melted_density_all, 
                        service == "Herpetofauna species richness" & extreme == climate.vars.order[4]),
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Purples",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[4],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p3.mwsl = ggplot(subset(ex_melted_density_all, 
                        service == "Carbon storage" & extreme == climate.vars.order[4]),
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Oranges",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[4],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
p4.mwsl = ggplot(subset(ex_melted_density_all, 
                        service == "Floodwater storage capacity" & extreme == climate.vars.order[4]), 
                 aes(x = x, y = y, z = value)) +
                 metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
                 scale_fill_distiller(palette = "Blues",
                                      direction = 2,
                                      limits=c(0,0.25)) +
                 coord_equal() +
                 labs(x = climate.vars.order[4],
                      y = "", color = "Density", fill = "Density") +
                 facet_wrap(.~service)
combo.mwsl = plot_grid(p1.mwsl + strip_axes, p2.mwsl + strip_axes, p3.mwsl + strip_axes, p4.mwsl + strip_axes, nrow = 1)
combo.mwsl.lab = ggdraw(combo.mwsl) +
                        draw_label(expression(Delta * "(Max. length wet spell) (z)"), 
                                   y = 0.02, x = 0.5, 
                                   angle=0, vjust = 0.8, size=12) + 
                        theme(plot.margin = margin(5, 5, 10, 10))
combo.mwsl.lab

combo.all = plot_grid(combo.fd.lab, combo.mxt100.lab, combo.mdsl.lab, combo.mwsl.lab, nrow=4) 
#ggsave("Manuscript/Supp_Figures/AppendixD/FigureD2_Bivariate_Copulas_Services_Versus_Climate_Extremes.jpeg", 
#       combo.all, width = 16, height = 14, dpi = 600)


################################################################################
# fit copulas between unprotecgted wetland areas/percents and climate extremes

unpro.types = c("area","percentage")
n.ut = length(unpro.types)

# make dataframes and lists
ex.es.fits = list()
ex.es.best.cop.number = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.cop.family = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.cross = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.upper = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tau = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))

# update column names
colnames(ex.es.best.cop.number) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.cop.family) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.cross) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.upper) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tau) = c("ssp","time","extreme","service","value")

# unprotected areas
for (i in 1:n.s) {
  ssp.i = ssps[i]
  ex.un.fits[[ssp.i]] = list()
  for (j in 1:n.t) {
    time.j = times[j]
    ex.un.fits[[ssp.i]][[time.j]] = list()
    for (k in 1:n.ex) {
      ex.k = climate.vars.order[k]
      ex.un.fits[[ssp.i]][[time.j]][[ex.k]] = list()
      for (l in 1:n.ut) {
        type.l = unpro.types[l]
        ex.un.fits[[ssp.i]][[time.j]][[es.k]][[type.l]] = list()
        climate.df.ij = subset(subset(scale.climate.df, ssp == ssp.i & time == time.j),
                               select=-c(region, ssp_time))
        if (l == 1) {
          unpro.climate.df = inner_join(scale.unpro.area.df, climate.df.ij, by = "county")
        } else {
          unpro.climate.df = inner_join(scale.unpro.percent.df, climate.df.ij, by = "county")
        }
        
        # prepare data (U and V must be nonexceedance probabilities between 0 and 1)
        u = rank(unpro.climate.df[,ex.k]) / (length(unpro.climate.df[,ex.k]) + 1)
        v = rank(unpro.climate.df[,water.reg.labels[3]]) / (length(unpro.climate.df[,water.reg.labels[3]]) + 1)
        
        # fit each copula model
        for (m in 1:n.c) {
          ex.un.fits[[ssp.i]][[time.j]][[es.k]][[type.l]][[cop.names[m]]] = BiCopSelect(u, v, familyset = cop.nums[m])
        }
        
        # compare AICs manually
        ex.fits = ex.es.fits[[ssp.i]][[time.j]][[es.k]][[type.l]]
        aic.df = sapply(list(ex.fits[["Gaussian"]],ex.fits[["t"]], ex.fits[["Clayton"]]),
                        function(f) c(family = f$family, AIC = f$AIC))
        
        
      }
    }
  }
}
  
