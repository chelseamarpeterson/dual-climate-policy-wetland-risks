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

# read in wetland ecosystem service estimates
eco.df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df)[1:2] = c("county","region")
scale.eco.df = eco.df
scale.eco.df[,3:6] = scale(scale.eco.df[,3:6], center=T, scale=T)
es.vars = colnames(scale.eco.df)[3:6]
n.es = length(es.vars)
es.labels = c("Plant species richness","Herpetofauna species richness",
              "Carbon storage","Floodwater storage capacity")

# read in unprotected wetland area estimates
unpro.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
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
scale.eco.unpro.df = inner_join(scale.eco.df, scale.unpro.df, by="county")

# copulas that I plan to test
cop.names = c("Gaussian","t","Clayton")
cop.nums = seq(1,3)
all.cop.nums = list("1" = c(1),
                    "2" = c(2),
                    "3" = c(3,13,23,33))
n.c = length(cop.names)

## copula comparison for ecosystem services v. unprotected wetland area
es.un.fits = list()
es.un.best.cop.number = data.frame(matrix(nrow=n.w,ncol=n.es))
es.un.best.cop.family = data.frame(matrix(nrow=n.w,ncol=n.es))
es.un.best.tail.dep.lower = data.frame(matrix(nrow=n.w,ncol=n.es))
es.un.best.tail.dep.upper = data.frame(matrix(nrow=n.w,ncol=n.es))
colnames(es.un.best.cop.number) = es.labels
colnames(es.un.best.cop.family) = es.labels
colnames(es.un.best.tail.dep.lower) = es.labels
colnames(es.un.best.tail.dep.upper) = es.labels
row.names(es.un.best.cop.number) = water.reg.labels
row.names(es.un.best.cop.family) = water.reg.labels
row.names(es.un.best.tail.dep.lower) = water.reg.labels
row.names(es.un.best.tail.dep.upper) = water.reg.labels
for (i in 1:n.es) {
  # make list for copula fits given a specific ES
  es.i = es.vars[i]
  es.un.fits[[es.i]] = list()
  
  for(j in 1:n.w) {
    # get water regime label and make lists
    wr.j = water.reg.labels[j]
    es.un.fits[[es.i]][[wr.j]] = list()
    
    # convert variables to ranks
    u = rank(scale.eco.unpro.df[,wr.j]) / (length(scale.eco.unpro.df[,wr.j]) + 1)
    u_inv = rank(-scale.eco.unpro.df[,wr.j]) / (length(scale.eco.unpro.df[,wr.j]) + 1)
    v = rank(scale.eco.unpro.df[,es.i]) / (length(scale.eco.unpro.df[,es.i]) + 1) 
    
    # fit each copula model
    for (k in 1:n.c) { es.un.fits[[es.i]][[wr.j]][[cop.names[k]]] = BiCopSelect(u, v, familyset = cop.nums[k]) }
    
    # compare copulas with AIC and BIC
    es.fits = es.un.fits[[es.i]][[wr.j]]
    aic.df = sapply(list(es.fits[["Gaussian"]],es.fits[["t"]], es.fits[["Clayton"]]),
                    function(f) c(family = f$family, AIC = f$AIC))
    
    # save best copula number in matrix
    best.cop.number = as.integer(aic.df[1,which.min(aic.df[2,])])
    es.un.best.cop.number[wr.j,es.labels[i]] = best.cop.number
    es.un.best.tail.dep.lower[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u_inv, v, familyset = best.cop.number))$lower
    es.un.best.tail.dep.upper[wr.j,es.labels[i]] = BiCopPar2TailDep(BiCopSelect(u_inv, v, familyset = best.cop.number))$upper    
    for (k in 1:n.c) {
      potential.cop.nums = all.cop.nums[[k]]
      if (best.cop.number %in% potential.cop.nums) { best.cop.family = cop.names[k] }
    }
    es.un.best.cop.family[wr.j,es.labels[i]] = best.cop.family
  }
}
es.colors = c("darkgreen","purple4","orangered","royalblue4")
es.shapes = c("circle","square","diamond","triangle")

# copula comparison for ecosystem services v. climate extremes
ex.es.fits = list()
ex.es.best.cop.number = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.cop.family = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.lower = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
ex.es.best.tail.dep.upper = data.frame(matrix(nrow=n.s*n.t*n.ex*n.es, ncol=5))
colnames(ex.es.best.cop.number) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.cop.family) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.lower) = c("ssp","time","extreme","service","value")
colnames(ex.es.best.tail.dep.upper) = c("ssp","time","extreme","service","value")
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
        
        ex.es.best.tail.dep.lower[n,"ssp"] = ssp.i
        ex.es.best.tail.dep.lower[n,"time"] = time.j
        ex.es.best.tail.dep.lower[n,"service"] = es.labels[k]
        ex.es.best.tail.dep.lower[n,"extreme"] = ex.l
        ex.es.best.tail.dep.lower[n,"value"] = BiCopPar2TailDep(BiCopSelect(u, v, familyset = best.cop.number))$lower    
        
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

es.un.best.tail.dep.upper$cutoff = water.reg.labels
es.un.tail.dep.upper.melt = melt(es.un.best.tail.dep.upper, id.var="cutoff")
es.un.tail.dep.upper.melt$label = "Unprotected wetland percent"
ex.es.best.tail.dep.upper$ssp_time = paste(ex.es.best.tail.dep.upper$ssp,
                                           ex.es.best.tail.dep.upper$time,
                                           sep = " &times; ")
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
                             text = element_text(size=14),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) +
                       facet_wrap(.~label) +
                       labs(y="Wetland flood-frequency cutoff",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Upper tail dependence coefficient (&lambda;<sub><em>U</em></sub>)")
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
                      theme(text = element_text(size=14),
                            strip.text = element_markdown(),
                            axis.text.y = element_markdown(),
                            axis.title.x = element_markdown()) + 
                      xlim(0,0.6) +
                      labs(y="Shared socioeconomic pathway\nby climatology period",
                           color="Ecosystem service",
                           shape="Ecosystem service",
                           size="Ecosystem service",
                           x="Upper tail dependence coefficient (&lambda;<sub><em>U</em></sub>)")
p.upper = p.un.es.upper + p.ex.es.upper + plot_layout(widths = c(1,2), axis_titles = "collect")
p.upper
ggsave("Manuscript/Main_Figures/Figure4_Bivariate_Copulas_Upper_Tail_Dependence.jpeg", 
       plot=p.upper, width=44, height=16, units="cm", dpi=600)

################################################################################
# lower tail dependence plots

ex.es.best.tail.dep.lower$ssp_time = paste(ex.es.best.tail.dep.lower$ssp,
                                           ex.es.best.tail.dep.lower$time,
                                           sep = " &times; ")
es.un.best.tail.dep.lower$cutoff = water.reg.labels
es.un.tail.dep.lower.melt = melt(es.un.best.tail.dep.lower, id.var="cutoff")
es.un.tail.dep.lower.melt$label = "Unprotected wetland percent"
p.un.es.lower = ggplot(es.un.tail.dep.lower.melt,
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
                             text = element_text(size=14),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) +
                       facet_wrap(.~label) +
                       labs(y="Wetland flood-frequency cutoff",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Lower tail dependence coefficient (&lambda;<sub><em>L</em></sub>)")
p.ex.es.lower = ggplot(ex.es.best.tail.dep.lower,
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
                       theme(text = element_text(size=14),
                             strip.text = element_markdown(),
                             axis.text.y = element_markdown(),
                             axis.title.x = element_markdown()) + 
                       xlim(0,0.6) +
                       labs(y="Shared socioeconomic pathway\nby climatology period",
                            color="Ecosystem service",
                            shape="Ecosystem service",
                            size="Ecosystem service",
                            x="Lower tail dependence coefficient (&lambda;<sub><em>L</em></sub>)")
p.lower = p.un.es.lower + p.ex.es.lower + plot_layout(widths = c(1,2), axis_titles = "collect")
p.lower
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD3_Bivariate_Copulas_Lower_Tail_Dependence.jpeg", 
       plot=p.lower, width=44, height=16, units="cm", dpi=600)

################################################################################
# lack of protection bivariate copula plots

best_bicop_model_list = list()
melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(melted_density_all) = c("x","y","value","service")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.es) {
  # get model with least AIC/BIC
  es.i = es.vars[i]
  best_bicop_model_list[[es.i]] = es.un.fits[[es.i]][[water.reg.labels[3]]][[es.un.best.cop.family[i,3]]]
  
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
               labs(x = "Unprotected wetland percent (z)",
                    y = "Ecosystem service (z)",
                    color = "Density", fill = "Density") +
               facet_wrap(.~service)
p2.un = ggplot(subset(melted_density_all, service == "Herpetofauna species richness"), 
               aes(x = x, y = y, z = value)) +
               metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
               scale_fill_distiller(palette = "Purples",
                                    direction = 2,
                                    limits=c(0,0.25)) +
               coord_equal() +
               labs(x = "Unprotected wetland percent (z)",
                    y = "",
                    color = "Density", fill = "Density") +
               facet_wrap(.~service)
p2.un
p3.un = ggplot(subset(melted_density_all, service == "Carbon storage"), 
               aes(x = x, y = y, z = value)) +
               metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
               scale_fill_distiller(palette = "Oranges",
                                    direction = 2,
                                    limits=c(0,0.25)) +
               coord_equal() +
               labs(x = "Unprotected wetland percent (z)",
                    y = "",
                    color = "Density", fill = "Density") +
               facet_wrap(.~service)
p3.un
p4.un = ggplot(subset(melted_density_all, service == "Floodwater storage capacity"), 
               aes(x = x, y = y, z = value)) +
               metR::geom_contour_fill(breaks=seq(0,0.25,length.out=10)) +
               scale_fill_distiller(palette = "Blues",
                                    direction = 2,
                                    limits=c(0,0.25)) +
               coord_equal() +
               labs(x = "Unprotected wetland percent (z)",
                    y = "",
                    color = "Density", fill = "Density") +
               facet_wrap(.~service)
p4.un
strip_axes = theme(axis.title.x = element_blank(),
                   text = element_text(size=12),
                   legend.title = element_text(size = 11),
                   legend.text = element_text(size = 8))
combo.un = plot_grid(p1.un + strip_axes,
                     p2.un + strip_axes,
                     p3.un + strip_axes,
                     p4.un + strip_axes, nrow = 1)
combo.un.lab = ggdraw(combo.un) + 
                      draw_label("Unprotected wetland percent (z)", 
                                 y = 0.02, x = 0.5, 
                                 angle=0, vjust = 0.8, size=12) + 
                      theme(plot.margin = margin(5, 5, 10, 10))
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD1_Bivariate_Copulas_Services_Versus_Unprotected_Percents.jpeg", 
       combo.un.lab, width = 16, height = 3.5, dpi = 600)

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
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD2_Bivariate_Copulas_Services_Versus_Climate_Extremes.jpeg", 
       combo.all, width = 16, height = 14, dpi = 600)






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
