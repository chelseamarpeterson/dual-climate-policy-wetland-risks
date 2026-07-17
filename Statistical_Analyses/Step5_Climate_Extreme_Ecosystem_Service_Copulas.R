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

# read in wetland ecosystem service estimates
eco.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df)[1:2] = c("county","region")
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(scale.eco.df[,3:6], center=T, scale=T)
es.vars = colnames(scale.eco.df)[3:6]
n.es = length(es.vars)
es.labels = c("Plant species richness","Herpetofauna species richness",
              "Carbon storage","Floodwater storage capacity")

# read in unprotected wetland area estimates
unpro.df = read.csv("Dual-Risk-Repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
unpro.df = unpro.df[,c("NAME","mean_PF_brinkerhoff","mean_IE_brinkerhoff","mean_SPF_brinkerhoff","mean_SF_brinkerhoff")]
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded","Seasonally Flooded")
n.w = length(water.reg.labels)
colnames(unpro.df) = c("county",water.reg.labels)
scale.unpro.df = unpro.df
scale.unpro.df[,water.reg.labels] = scale(scale.unpro.df[,water.reg.labels], center=T, scale=T)

# read in climate data
climate.df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv")
climate.vars = unique(climate.df$variable)
climate.vars.order = c("Frost days","Days with max. temp. over\n100 deg F (38 deg C)",
                       "Consecutive dry days","Consecutive wet days")
n.v = length(climate.vars)
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

# join service and unprotected wetland area dataframes
scale.eco.unpro.df = inner_join(scale.eco.df, scale.unpro.df, by="county")

## copula comparison for ecosystem services v. unprotected wetland area
es.un.fits = list()
es.un.best.cop = c()
cop.names = c("t","Clayton","Gumbel","Frank")
cop.nums = seq(2,5)
n.c = length(cop.names)
for (i in 1:n.es) {
  # make list for copula fits given a specific ES
  es.un.fits[[es.vars[i]]] = list()
  
  # convert variables to ranks
  u = rank(scale.eco.unpro.df[,water.reg.labels[3]]) / (length(scale.eco.unpro.df[,water.reg.labels[3]]) + 1)
  v = rank(scale.eco.unpro.df[,es.vars[i]]) / (length(scale.eco.unpro.df[,es.vars[i]]) + 1) 
  
  # fit each copula model
  for (j in 1:n.c) { 
    es.un.fits[[es.vars[i]]][[cop.names[j]]] = BiCopSelect(u, v, 
                                                           familyset = cop.nums[j]) 
    }
  
  # compare copulas with AIC and BIC
  es.fits = es.un.fits[[es.vars[i]]]
  aic.df = sapply(list(es.fits[["t"]], es.fits[["Clayton"]], es.fits[["Gumbel"]], es.fits[["Frank"]]),
                   function(f) c(family = f$family, AIC = f$AIC))
  es.un.best.cop = c(es.un.best.cop, as.integer(aic.df[1,which.min(aic.df[2,])]))
}

# create contour plot over normal margin
#contour(best_bicop_model_plants, margins = "norm", col = terrain.colors(15))
best_bicop_model_list = list()
best_model_numbers = cop.names[es.un.best.cop-1] # t, Frank, Frank, t
melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(melted_density_all) = c("x","y","value","service")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.es) {
  # get model with least AIC/BIC
  best_bicop_model_list[[es.vars[i]]] = es.un.fits[[es.vars[i]]][[best_model_numbers[i]]]
  
  # create grid sequence
  density_matrix = outer(grid_seq, grid_seq, 
                         function(x, y) {BiCopPDF(pnorm(x), pnorm(y), best_bicop_model_list[[es.vars[i]]]) * dnorm(x) * dnorm(y)})
  dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
  
  # melt dataframe for plotting
  melted_density = melt(density_matrix, varnames = c("x", "y"))
  melted_density$service = es.labels[i]
  
  # upend to large matrix
  melted_density_all = rbind(melted_density_all, melted_density)
}

p1 = ggplot(subset(melted_density_all, service == "Plant species richness"), 
           aes(x = x, y = y, z = value)) +
           metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
           scale_fill_distiller(palette = "Greens",
                                direction = 2,
                                limits=c(0,0.25)) +
           coord_equal() +
           labs(x = "Unprotected wetland Area (Standard normal margin)",
                y = "Ecosytem service (Standard normal margin)",
                color = "Density", fill = "Density") +
           facet_wrap(.~service) 
p2 = ggplot(subset(melted_density_all, service == "Herpetofauna species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Purples",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland area (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p3 = ggplot(subset(melted_density_all, service == "Carbon storage"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Oranges",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland area (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p4 = ggplot(subset(melted_density_all, service == "Floodwater storage capacity"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Blues",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland area (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
strip_axes = theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   text = element_text(size=12),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))
p.cop.es.un = grid.arrange(p1+strip_axes, p2+strip_axes, p3+strip_axes, p4+strip_axes, ncol=4, 
                           bottom="Unprotected wetland area (Standard normal margin)",
                           left="Ecosytem service (Standard normal margin)")
p.cop.es.un

# copula comparison for ecosystem services v. climate extremes
ex.es.fits = list()
ex.es.best.cop = list()
for (i in 1:n.s) {
  ssp.i = ssps[i]
  ex.es.fits[[ssp.i]] = list()
  ex.es.best.cop[[ssp.i]] = list()
  for (j in 1:n.t) {
    time.j = times[j]
    ex.es.fits[[ssp.i]][[time.j]] = list()
    ex.es.best.cop[[ssp.i]][[time.j]] = c()
    for (k in 1:n.es) {
      es.k = es.vars[k]
      ex.k = climate.vars.order[k]
      ex.es.fits[[ssp.i]][[time.j]][[es.k]] = list()
      
      # join climate data for given ssp
      climate.df.ij = subset(subset(scale.climate.df, ssp == ssp.i & time == time.j),
                             select=-c(region, ssp_time))
      eco.climate.df = inner_join(scale.eco.df, climate.df.ij, by = "county")
      eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
      
      # prepare data (U and V must be nonexceedance probabilities between 0 and 1)
      u = rank(eco.climate.df[,ex.k]) / (length(eco.climate.df[,ex.k]) + 1)
      v = rank(eco.climate.df[,es.k]) / (length(eco.climate.df[,es.k]) + 1)
      
      # fit each copula model
      for (l in 1:n.c) {
        ex.es.fits[[ssp.i]][[time.j]][[es.k]][[cop.names[l]]] = BiCopSelect(u, v, familyset = cop.nums[l])
      }
      
      # let VineCopula select automatically across all families
      #fit_auto <- BiCopSelect(u, v, familyset = NA)
      #print(fit_auto)
      
      # compare AICs manually
      ex.fits = ex.es.fits[[ssp.i]][[time.j]][[es.k]]
      aic.df = sapply(list(ex.fits[["t"]], ex.fits[["Clayton"]], ex.fits[["Gumbel"]], ex.fits[["Frank"]]),
                      function(f) c(family = f$family, AIC = f$AIC))
      ex.es.best.cop[[ssp.i]][[time.j]] = c(ex.es.best.cop[[ssp.i]][[time.j]], 
                                            as.integer(aic.df[1,which.min(aic.df[2,])]))
    }
  }
}

ssp = ssps[2]
time = times[1]
ex_best_bicop_model_list = list()
ex_best_model_numbers = c(2, 3, 3, 5) #ex.es.best.cop[[ssp]][[time]] # t, rotated Clayton (180 degrees), Clayton (90 degrees), Frank
ex_melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(ex_melted_density_all) = c("x","y","value","service")
for (i in 1:n.es) {
  es.i = es.vars[i]
  
  # get model with least AIC/BIC
  ex_best_bicop_model_list[[es.i]] = ex.es.fits[[ssp]][[time]][[es.i]][[cop.names[ex_best_model_numbers[i]-1]]]
  print(ex_best_bicop_model_list[[es.i]])
  
  # create grid sequence
  density_matrix = outer(grid_seq, grid_seq, 
                         function(x, y) {BiCopPDF(pnorm(x), pnorm(y), ex_best_bicop_model_list[[es.i]]) * dnorm(x) * dnorm(y)})
  dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
  
  # melt dataframe for plotting
  ex_melted_density = melt(density_matrix, varnames = c("x", "y"))
  ex_melted_density$service = es.labels[i]
  
  # upend to large matrix
  ex_melted_density_all = rbind(ex_melted_density_all, ex_melted_density)
}


p1 = ggplot(subset(ex_melted_density_all, 
                   service == "Plant species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Greens",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p1
p2 = ggplot(subset(ex_melted_density_all, 
                   service == "Herpetofauna species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Purples",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p2
p3 = ggplot(subset(ex_melted_density_all, 
                   service == "Carbon storage"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Oranges",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p3
p4 = ggplot(subset(ex_melted_density_all, 
                   service == "Floodwater storage capacity"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Blues",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosytem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p4

p.cop.es.ex = grid.arrange(p1+strip_axes, p2+strip_axes, p3+strip_axes, p4+strip_axes, ncol=4, 
                           bottom="Climate extreme (Standard normal margin)",
                           left="Ecosytem service (Standard normal margin)")
p.cop.es.ex

################################################################################
# fit three-way copula
set.seed(2718)

# combine unprotected area, ecosystem service, and climate dataframe
wr = "Semipermanently Flooded"
q = 0.75
n_sim = 1e6
# apply empirical CDF to put all variables on [0,1] scale
for (j in 1:n.s) {
  for (k in 1:n.t) {
    conditional.climate.risk.df = scale.eco.df
    for (i in 1:n.es) {
      es.i = es.vars[i]
      ex.i = climate.vars.order[i]
      
      # make dataframe for specific ecosystem service, ssp, and time interval
      climate.df.st = subset(scale.climate.df,
                             ssp == ssps[j] & time == times[k])
      eco.un.clim.df = left_join(scale.eco.df[,c("county",es.i)], 
                                 scale.unpro.df[,c("county",wr)], 
                                 by="county")
      eco.un.clim.df = left_join(eco.un.clim.df,
                                 climate.df.st[,c("county",ex.i)],
                                 by="county")
      if (ex.i == "Frost days") { eco.un.clim.df[,ex.i] = -eco.un.clim.df[,ex.i] }
      
      # make empirical CDFs
      u.es = rank(eco.un.clim.df[,es.i]) / (length(eco.un.clim.df[,es.i]) + 1)
      u.un = rank(eco.un.clim.df[,wr]) / (length(eco.un.clim.df[,wr]) + 1)
      u.ex = rank(eco.un.clim.df[,ex.i]) / (length(eco.un.clim.df[,ex.i]) + 1)
      u_df = cbind(u.es, u.un, u.ex)
      colnames(u_df) = c("ecosystem_service","unprotected_area","climate_extreme")
      
      # fit vine copula
      fit_vine = RVineStructureSelect(data = u_df,
                                      familyset     = NA, 
                                      type          = 0,      # 0 = RVine
                                      selectioncrit = "AIC",  
                                      indeptest     = TRUE,   
                                      level         = 0.1, 
                                      trunclevel    = NA,
                                      rotations = TRUE)
      
      # simulate from fitted vine
      sim_data = RVineSum(n_sim, fit_vine)
      
      # calculate joint exceedance probability for all variables
      joint_exceedance = mean(sim_data[,1] > q,
                             sim_data[,2] > q,
                             sim_data[,3] > q)
      
      # add to dataframe
      conditional.climate.risk.df[,es.i] = joint_exceedance
      
      # write csv
      write.csv(conditional.climate.risk.df,
                paste(paste("Dual-Risk-Repo/County_Summaries/Joint_Exceedance_Risk_75th_Percentile",
                            ssps[j], times[k], sep="_"),
                      ".csv", sep=""),
                row.names=F)
  }
}
  
  
