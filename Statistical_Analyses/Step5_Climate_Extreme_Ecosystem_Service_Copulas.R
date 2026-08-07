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
tail.dep = list()
cop.names = c("t","Clayton","Gumbel","Frank")
cop.nums = seq(2,5)
n.c = length(cop.names)
for (i in 1:n.es) {
  # make list for copula fits given a specific ES
  es.un.fits[[es.vars[i]]] = list()
  tail.dep[[es.vars[i]]] = list()
  
  # convert variables to ranks
  u = rank(scale.eco.unpro.df[,water.reg.labels[3]]) / (length(scale.eco.unpro.df[,water.reg.labels[3]]) + 1)
  v = rank(scale.eco.unpro.df[,es.vars[i]]) / (length(scale.eco.unpro.df[,es.vars[i]]) + 1) 
  
  # fit each copula model
  for (j in 1:n.c) { 
    es.un.fits[[es.vars[i]]][[cop.names[j]]] = BiCopSelect(u, v, familyset = cop.nums[j])
    tail.dep[[es.vars[i]]][[cop.names[j]]] = BiCopPar2TailDep(es.un.fits[[es.vars[i]]][[cop.names[j]]])    
  }
  
  # compare copulas with AIC and BIC
  es.fits = es.un.fits[[es.vars[i]]]
  aic.df = sapply(list(es.fits[["t"]], es.fits[["Clayton"]], es.fits[["Gumbel"]], es.fits[["Frank"]]),
                   function(f) c(family = f$family, AIC = f$AIC))
  best.family = as.integer(aic.df[1,which.min(aic.df[2,])])
  print(best.family)
  es.un.best.cop = c(es.un.best.cop, best.family)
  
  # evaluate tail dependence of best copula
  #if (best.family == 2) { 
  #  tail.dep.i = BiCopPar2TailDep(es.fits[["t"]])
  #} else if (best.family %in% c(3, 23, 33)) {
  #  tail.dep.i = BiCopPar2TailDep(es.fits[["Clayton"]])
  #  print(tail.dep.i)
  #} else if (best.family %in% c(4, 24, 34)) {
  #  tail.dep.i = BiCopPar2TailDep(es.fits[["Gumbel"]])    
  #} else if (best.family == 5) {
  #  tail.dep.i = 
  #}
  #tail.dep[[es.vars[i]]] = tail.dep.i
}

# create contour plot over normal margin
#contour(best_bicop_model_plants, margins = "norm", col = terrain.colors(15))
best_bicop_model_list = list()
best_models = cop.names[c(3,2,2,2)] # 24 = rotated Gumbel copula (90 degrees), 33 = rotated Clayton copula (270 degrees)
melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(melted_density_all) = c("x","y","value","service")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.es) {
  # get model with least AIC/BIC
  best_bicop_model_list[[es.vars[i]]] = es.un.fits[[es.vars[i]]][[best_models[i]]]
  
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

p1.un = ggplot(subset(melted_density_all, service == "Plant species richness"), 
           aes(x = x, y = y, z = value)) +
           metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
           scale_fill_distiller(palette = "Greens",
                                direction = 2,
                                limits=c(0,0.25)) +
           coord_equal() +
           labs(x = "Unprotected wetland percent (Standard normal margin)",
                y = "Ecosystem service (Standard normal margin)",
                color = "Density", fill = "Density") +
           facet_wrap(.~service) 
p2.un = ggplot(subset(melted_density_all, service == "Herpetofauna species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Purples",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland percent (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p3.un = ggplot(subset(melted_density_all, service == "Carbon storage"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Oranges",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland percent (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p4.un = ggplot(subset(melted_density_all, service == "Floodwater storage capacity"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Blues",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Unprotected wetland percent (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
strip_axes = theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   text = element_text(size=14),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))


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
ex.es.best.cop[[ssp]][[time]]
ex_best_model_numbers = c(2, 2, 2, 5) # rotated Clayton (180 degrees), rotated Clayton (180 degrees), Clayton (90 degrees), Frank
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


p1.ex = ggplot(subset(ex_melted_density_all, 
                   service == "Plant species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Greens",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p2.ex = ggplot(subset(ex_melted_density_all, 
                   service == "Herpetofauna species richness"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Purples",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p3.ex = ggplot(subset(ex_melted_density_all, 
                   service == "Carbon storage"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Oranges",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)
p4.ex = ggplot(subset(ex_melted_density_all, 
                   service == "Floodwater storage capacity"), 
            aes(x = x, y = y, z = value)) +
            metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
            scale_fill_distiller(palette = "Blues",
                                 direction = 2,
                                 limits=c(0,0.25)) +
            coord_equal() +
            labs(x = "Climate extreme (Standard normal margin)",
                 y = "Ecosystem service (Standard normal margin)",
                 color = "Density", fill = "Density") +
            facet_wrap(.~service)

library(cowplot)
combo.top = plot_grid(p1.un + strip_axes, p2.un + strip_axes,
                      p3.un + strip_axes, p4.un + strip_axes,nrow = 1)
combo.low = plot_grid(p1.ex + strip_axes, p2.ex + strip_axes,
                      p3.ex + strip_axes, p4.ex + strip_axes, nrow = 1)
combo.top.lab = ggdraw(combo.top) + 
                draw_label("Unprotected wetland percent (Standard normal margin)", 
                           y = 0.01, x = 0.5, angle=0, vjust = 0.8, size=12) + 
                draw_label("Ecosystem service\n(Standard normal margin)", 
                           y = 0.5, x = 0.003, angle=90, vjust = 0, size=12) + 
                theme(plot.margin = margin(5, 5, 10, 20))
combo.low.lab = ggdraw(combo.low) +
                draw_label("Climate extreme (Standard normal margin)", 
                           y = 0.01, x = 0.5, angle=0, vjust = 0.8, size=12) + 
                draw_label("Ecosystem service\n(Standard normal margin)", 
                           y = 0.5, x = 0.003, angle=90, vjust = 0, size=12) +
                theme(plot.margin = margin(5, 5, 10, 20))
combo.all = plot_grid(combo.top.lab, 
                      combo.low.lab, nrow=2) 
combo.all
ggsave("Manuscript/Main_Figures/Figure6_Bivariate_Copulas.jpeg", 
       combo.all, width = 16, height = 7.5, dpi = 600)

################################################################################
# fit three-way copula and simulate joint exceedance probabilities

set.seed(2718)

# estimate joint exceedance probability for climate and unprotected area:
# P (climate_extreme > q & unprotected_area > q | ecosystem service value)
wr = "Semipermanently Flooded"
q = 0.75
n_sim = 1e4
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
      
      cond_joint_exceedance = sapply(u_df[, "ecosystem_service"], function(u1_obs) {
        
        # simulate U2 and U3 marginally
        p2_sim = runif(n_sim)
        p3_sim = runif(n_sim)
        
        # Tree 1: condition U2 on U1
        # Hinv1 inverts h1(u2|u1) = P(U2 <= u2 | U1 = u1); thus, must return u2 | u1
        u2_cond = BiCopHinv1(
          u1  = rep(u1_obs, n_sim),
          u2  = p2_sim,
          obj = BiCop(family = fit_vine$family[2, 1],   # Tree 1, pair 1
                      par    = fit_vine$par[2, 1],
                      par2   = fit_vine$par2[2, 1]))
        
        # Tree 1: compute pseudo observation for Tree 2: h1(u2 | u1) = P(U2 <= u2 | U1 = u1)
        h_u2_given_u1 = BiCopHfunc1(
          u1 = rep(u1_obs, n_sim),
          u2 = u2_cond,
          obj = BiCop(family = fit_vine$family[2, 1],   # Tree 1, pair 1
                      par    = fit_vine$par[2, 1],
                      par2   = fit_vine$par2[2, 1]))
        
        # Tree 1: condition U3 on U2
        h_u3_given_u2 = BiCopHfunc1(
          u1 = u2_cond,
          u2 = p3_sim,
          obj = BiCop(
            family = fit_vine$family[3, 2], # Tree 1, pair 2
            par    = fit_vine$par[3, 2],
            par2   = fit_vine$par2[3, 2]))
        
        # Tree 2: condition U3 on U2 | U1
        # invert the Tree 2 h-function to get U3 draws conditional on both U1 and U2
        # h1(u3|u2) = P(U3 <= u3 | U2 = u1), so Hinv1 returns u3 | u2
        u3_cond = BiCopHinv1(
          u1  = h_u2_given_u1,
          u2  = h_u3_given_u2,
          obj = BiCop(
            family = fit_vine$family[3, 1],
            par    = fit_vine$par[3, 1],
            par2   = fit_vine$par2[3, 1]))
        
        # compute joint exceedance
        mean(u2_cond > q & u3_cond > q)
      })  
        
      # add to dataframe
      conditional.climate.risk.df[,es.i] = cond_joint_exceedance
      
      # write csv
      write.csv(conditional.climate.risk.df,
                paste(paste("Dual-Risk-Repo/County_Summaries/Conditional_Joint_Exceedance_Risk_75th_Percentile_Simulation_Method",
                            ssps[j], times[k], sep="_"),
                      ".csv", sep=""),
                row.names=F)
    }
  }
}

################################################################################
# fit three-way copula and calculate exact joint exceedance probabilities
library(cubature)
set.seed(2718)

# estimate joint exceedance probability for climate and unprotected area:
# P (climate_extreme > q & unprotected_area > q | ecosystem service value)
wr = "Semipermanently Flooded"
q = 0.75
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
      
      cond_joint_exceedance <- sapply(u_df[, "ecosystem_service"], function(u1_obs) {
        integrand <- function(uv) {
          u2 = uv[1]
          u3 = uv[2]
          
          # conditional density f(u2, u3 | u1)
          f12 = BiCopPDF(rep(u1_obs,1), u2,
                         obj = BiCop(family = fit_vine$family[2, 1],
                                     par    = fit_vine$par[2, 1],
                                     par2   = fit_vine$par2[2, 1]))
          
          f23 = BiCopPDF(u2, u3,
                         obj = BiCop(family = fit_vine$family[3, 2],  
                                     par    = fit_vine$par[3, 2],
                                     par2   = fit_vine$par2[3, 2]))
          
          h_u2 = BiCopHfunc1(rep(u1_obs,1), u2,
                             obj = BiCop(family = fit_vine$family[2, 1],
                                         par    = fit_vine$par[2, 1],
                                         par2   = fit_vine$par2[2, 1]))
          
          h_u3 = BiCopHfunc1(u2, u3,
                             obj = BiCop(family = fit_vine$family[3, 2],
                                         par    = fit_vine$par[3, 2],
                                         par2   = fit_vine$par2[3, 2]))
          
          f13_2 = BiCopPDF(h_u2, h_u3,
                           obj = BiCop(family = fit_vine$family[3, 1],
                                       par    = fit_vine$par[3, 1],
                                       par2   = fit_vine$par2[3, 1]))
          
          f12 * f23 * f13_2
        }
        
        # Integrate over exceedance region
        result <- adaptIntegrate(integrand,
                                 lowerLimit = c(q, q),
                                 upperLimit = c(1, 1),
                                 tol        = 1e-4)
        result$integral
      })
      
      # add to dataframe
      conditional.climate.risk.df[,es.i] = cond_joint_exceedance
      
      # write csv
      write.csv(conditional.climate.risk.df,
                paste(paste("Dual-Risk-Repo/County_Summaries/Conditional_Joint_Exceedance_Risk_75th_Percentile_Integration_Method",
                            ssps[j], times[k], sep="_"),
                      ".csv", sep=""),
                row.names=F)
    }
  }
}
