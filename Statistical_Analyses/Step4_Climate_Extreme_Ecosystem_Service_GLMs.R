setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(reshape2)
library(patchwork)
library(RColorBrewer)
library(brms)
library(bayestestR)

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
climate.df = read.csv("dual-risk-repo/County_Summaries/All_County_Climate_Extremes.csv")
climate.vars = unique(climate.df$variable)
climate.vars.order = c("Frost days","Days with max. temp. over\n100 deg F (38 deg C)",
                       "Consecutive dry days","Consecutive wet days")
n.ex = length(climate.vars)
climate.wide.df = pivot_wider(data = climate.df,
                              names_from = variable,
                              values_from = value)
scale.climate.df = climate.wide.df
scale.climate.df[,climate.vars] = scale(climate.wide.df[,climate.vars], center=T, scale=T)

# ssps
ssps = c("SSP2-4.5","SSP3-7.0")
n.sp = length(ssps)

# time intervals
times = c("2041-2070","2071-2100")
n.t = length(times)

# county subsets
scopes = c("Entire state") #,"Counties without ordinances")
n.sc = length(scopes)

# join service and unprotected wetland area dataframes
scale.eco.unpro.df = inner_join(scale.eco.df, scale.unpro.df, by="county")

# counties with wetland protection
protected.counties = c("Cook","Lake","McHenry","DuPage","DeKalb","Will","Kane","Grundy")

################################################################################
# linear models with BRMS
set.seed(2718)

# ecosystem services v. unprotected wetland area

un.es.lm.df = data.frame(matrix(nrow=n.es*n.w*n.sc, ncol=9))
colnames(un.es.lm.df) = c("service","cutoff","scope",
                          "int_mean","int_lower","int_upper",
                          "slope_mean","slope_lower","slope_upper")

#un.es.lm.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Effect_Sizes.csv")
un.es.lm.list = list()
n = 1
for (i in 1:n.es) {
  es.i = es.vars[i]
  un.es.lm.list[[es.i]] = list()
  for (j in 1:n.w) {
    wr.j = water.reg.labels[j]
    un.es.lm.list[[es.i]][[wr.j]] = list()
    for (k in 1:n.sc) {
      s.k = scopes[k]
      if (s.k == "Entire state") {
        scale.eco.df.ij = scale.eco.unpro.df[,c("county","region",es.i,wr.j)]
      } else {
        scale.eco.df.ij = subset(scale.eco.unpro.df[,c("county","region",es.i,wr.j)],
                                 !(county %in% protected.counties))
      }
      colnames(scale.eco.df.ij)[3:4] = c("service","area")
      lm.ijk = brm(service ~ area, 
                   data = scale.eco.df.ij,
                   family = gaussian())
      un.es.lm.list[[es.i]][[wr.j]][[s.k]] = lm.ijk
      #n = which((un.es.lm.df$service == es.labels[i] & un.es.lm.df$cutoff == wr.j) & un.es.lm.df$scope == s.k)
      un.es.lm.df[n,"service"] = es.labels[i]
      un.es.lm.df[n,"cutoff"] = wr.j
      un.es.lm.df[n,"scope"] = s.k
      un.es.lm.df[n,"int_mean"] = fixef(lm.ijk)[1,1]
      un.es.lm.df[n,"slope_mean"] = fixef(lm.ijk)[2,1]
      un.es.lm.df[n,c("int_lower","int_upper")] = hdi(lm.ijk, ci = 0.90, effects = "fixed")[1,c("CI_low","CI_high")]
      un.es.lm.df[n,c("slope_lower","slope_upper")] = hdi(lm.ijk, ci = 0.90, effects = "fixed")[2,c("CI_low","CI_high")]
      n = n + 1
    }
  }
}

write.csv(un.es.lm.df, 
          "dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Percent_Effect_Sizes.csv",
          row.names=F)

# ecosystem services v. climate extremes
ex.es.lm.df = data.frame(matrix(nrow = n.es*n.ex*n.sp*n.t*n.sc, ncol = 10))
colnames(ex.es.lm.df) = c("service","extreme","ssp_time","scope",
                          "int_mean","int_lower","int_upper",
                          "slope_mean","slope_lower","slope_upper")
n = 1
ex.es.lm.list = list()
for (i in 1:n.es) {
  es.i = es.vars[i]
  ex.es.lm.list[[es.i]] = list()
  for (j in 1:n.ex) {
    ex.j = climate.vars.order[j]
    ex.es.lm.list[[es.i]][[ex.j]] = list()
    for (k in 1:n.sp) {
      ssp.k = ssps[k]
      ex.es.lm.list[[es.i]][[ex.j]][[ssp.k]] = list()
      for (l in 1:n.t) {
        time.l = times[l]
        ssp.time.kl = paste(ssp.k, time.l, sep=" x ")
        ex.es.lm.list[[es.i]][[ex.j]][[ssp.k]][[time.l]] = list()
        
        # join ecosystem services and climate data for given ssp and time combination
        scale.clim.df.kl = subset(subset(scale.climate.df, ssp == ssp.k & time == time.l),
                                  select=-c(region, ssp_time))
        scale.eco.clim.df.kl = inner_join(scale.eco.df, scale.clim.df.kl, by = "county")
        scale.eco.clim.df.kl[,"Frost days"] = -scale.eco.clim.df.kl[,"Frost days"]
        
        # subset based on scope
        for (m in 1:n.sc) {
          s.m = scopes[m]
          if (s.m == "Entire state") {
            scale.eco.clim.df.ijklm = scale.eco.clim.df.kl[,c("county","region",es.i,ex.j)]
          } else {
            scale.eco.clim.df.ijklm = subset(scale.eco.clim.df.kl[,c("county","region",es.i,ex.j)],
                                            !(county %in% protected.counties))
          }
          
          colnames(scale.eco.clim.df.ijklm)[3:4] = c("service","extreme")
          lm.ijklm = brm(service ~ extreme, 
                         data = scale.eco.clim.df.ijklm,
                         family = gaussian())
          ex.es.lm.list[[es.i]][[ex.j]][[ssp.k]][[time.l]][[s.m]] = lm.ijklm
          #n = which((ex.es.lm.df$service == es.labels[i] & ex.es.lm.df$ssp_time == ssp.time.jk) & (ex.es.lm.df$extreme == ex.i & ex.es.lm.df$scope == s.k))
          ex.es.lm.df[n,"service"] = es.labels[i]
          ex.es.lm.df[n,"extreme"] = ex.j
          ex.es.lm.df[n,"ssp_time"] = ssp.time.kl
          ex.es.lm.df[n,"scope"] = s.m
          ex.es.lm.df[n,"int_mean"] = fixef(lm.ijklm)[1,1]
          ex.es.lm.df[n,"slope_mean"] = fixef(lm.ijklm)[2,1]
          ex.es.lm.df[n,c("int_lower","int_upper")] = hdi(lm.ijklm, ci = 0.90, effects = "fixed")[1,c("CI_low","CI_high")]
          ex.es.lm.df[n,c("slope_lower","slope_upper")] = hdi(lm.ijklm, ci = 0.90, effects = "fixed")[2,c("CI_low","CI_high")]
          n = n + 1
        }
      }
    }
  }
}

write.csv(ex.es.lm.df, 
          "dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Effect_Sizes.csv",
          row.names=F)

#################################################################################
# plot effect sizes

# load dataframes
un.es.lm.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Percent_Effect_Sizes.csv")
ex.es.lm.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Effect_Sizes.csv")

# unprotected wetland percent plot
un.es.lm.df$label = "Unprotected wetland\npercent"
p.lm.un.es = ggplot(subset(un.es.lm.df, scope=="Entire state")) +
                    geom_vline(xintercept=0, color="black",linetype="dashed") +
                    geom_point(aes(x=slope_mean,
                                   y=factor(cutoff, levels=water.reg.labels),
                                   color=factor(service, levels = es.labels),
                                   shape=factor(service, levels = es.labels),
                                   size=factor(service, levels = es.labels)),
                               position=position_dodge(0.3)) +
                    geom_errorbar(aes(xmin=slope_lower,xmax=slope_upper,
                                      y=factor(cutoff, levels=water.reg.labels),
                                      color=factor(service, levels = es.labels)),
                                  position=position_dodge(0.3),
                                  width=0.3) +
                    labs(y="Wetland flood-freqency cutoff",
                         x="Effect size",
                         color="Ecosystem service",
                         shape="Ecosystem service",
                         size="Ecosystem service") +
                    scale_color_manual(values=es.colors) +
                    scale_shape_manual(values=es.shapes) +
                    scale_size_manual(values=c(2,2,2.5,2)) +
                    theme(text = element_text(size=14),
                          legend.position = "none") +
                    facet_wrap(.~label, scales="free_x",ncol=2)
                    #scale_x_continuous(limits=c(-1,2.5)) #+
p.lm.un.es

ssp.time.order = rev(c(paste(ssps[1], times, sep=" x "),
                       paste(ssps[2], times, sep=" x ")))
ex.es.lm.df.filter = subset(ex.es.lm.df,!((service=="Floodwater storage capacity") & (extreme %in% climate.vars.order[1:3])))
p.lm.ex.es = ggplot(subset(ex.es.lm.df.filter, scope=="Entire state")) +
                    geom_vline(xintercept=0, color="black",linetype="dashed") + 
                    geom_point(aes(x=slope_mean,
                                   y=factor(ssp_time, levels=ssp.time.order),
                                   color=factor(service, levels = es.labels),
                                   shape=factor(service, levels = es.labels),
                                   size=factor(service, levels = es.labels)),
                                   position=position_dodge(0.5)) +
                    geom_errorbar(aes(xmin=slope_lower, xmax=slope_upper,
                                      y=factor(ssp_time, levels=ssp.time.order),
                                      color=factor(service, levels = es.labels)),
                                  position=position_dodge(0.5),
                                  width=0.3) +
                    labs(y="Shared socioeconomic pathway\nby climatology period",
                         x="Effect size",
                         color="Ecosystem service",
                         shape="Ecosystem service",
                         size="Ecosystem service") +
                    scale_color_manual(values=es.colors) +
                    scale_shape_manual(values=es.shapes) +
                    scale_size_manual(values=c(2,2,2.5,2)) +
                    theme(text = element_text(size=14)) +
                    facet_wrap(.~factor(extreme, levels=climate.vars.order), 
                               ncol=2)
p.lm.ex.es          
p.glm = p.lm.un.es + p.lm.ex.es + plot_layout(widths = c(1, 2))
p.glm
ggsave("Manuscript/Main_Figures/Figure4_GLM_Effect_Sizes.jpeg", 
       plot=p.glm, width=36, height=14, units="cm", dpi=600)


################################################################################
# plot lines for univariate models
un.area.seq = seq(min(scale.eco.unpro.df[,water.reg.labels]),
                  max(scale.eco.unpro.df[,water.reg.labels]), by=0.1)
n.un = length(un.area.seq)
un.es.line.df = data.frame(matrix(nrow=0,ncol=7))
colnames(un.es.line.df) = c("service","cutoff","scope","area",
                            "mean","lower","upper")
#un.es.line.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Lines_SPF.csv")
for (i in 1:n.es) {
  es.i = es.vars[i]
  for (j in 1:n.w) {
    wr.j = water.reg.labels[j]
    for (k in 1:n.sc) {
      s.k = scopes[k]
      lm.ijk = un.es.lm.list[[es.i]][[wr.j]][[s.k]]
      
      # new dataframe with predictor variable (area)
      plot_data = data.frame(area = un.area.seq)
      
      # apply model to new data
      linpred_draws = fitted(lm.ijk, 
                             newdata = plot_data, 
                             scale = "linear", 
                             summary = FALSE) 
      
      # calculate mean and hdi
      fit_summary = data.frame(mean = apply(linpred_draws, 2, mean),
                               lower = apply(linpred_draws, 2, function(x) hdi(x, ci = 0.90)$CI_low),
                               upper = apply(linpred_draws, 2, function(x) hdi(x, ci = 0.90)$CI_high))
      
      # Combine original predictor with the estimated values
      plot_data = bind_cols(plot_data, as_tibble(fit_summary))
      plot_data = cbind(data.frame(service = rep(es.labels[i], n.un),
                                   cutoff = rep(wr.j, n.un),
                                   scope = rep(s.k, n.un)),
                        plot_data)
      
      # add to full dataframe
      #inds.ijk = which((un.es.line.df$service == es.labels[i] & un.es.line.df$cutoff == wr.j) & un.es.line.df$scope == s.k)
      #un.es.line.df[inds.ijk,] = plot_data
      un.es.line.df = rbind(un.es.line.df, plot_data)
    }
  }
}

write.csv(un.es.line.df, 
          "dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Percent_Lines_SPF.csv",
          row.names=F)

# plot lines
un.es.line.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Percent_Lines_SPF.csv")
colnames(scale.eco.unpro.df)[3:6] = es.labels
scale.eco.unpro.melt.spf = melt(scale.eco.unpro.df[,c("county","region",es.labels,water.reg.labels[3])], 
                                id.vars=c("county","region",water.reg.labels[3]))
colnames(scale.eco.unpro.melt.spf)[3:4] = c("area","service")
p.un.es.linear = ggplot() +
                  geom_point(data=scale.eco.unpro.melt.spf,
                             aes(x=area, 
                                 y=value,
                                 color=factor(service, levels=es.labels)),
                             size=1) +
                  geom_line(data=subset(un.es.line.df, cutoff == "Semipermanently Flooded"),
                            aes(x=area,
                                y=mean,
                                color=factor(service, levels=es.labels))) +
                  geom_ribbon(data=subset(un.es.line.df, cutoff == "Semipermanently Flooded"),
                              aes(x=area,
                                  ymax=upper,
                                  ymin=lower,
                                  color=factor(service, levels=es.labels),
                                  fill=factor(service, levels=es.labels)),
                              alpha=0.3, color=NA) +
                 facet_wrap(.~factor(service, levels=es.labels), ncol=2) +
                 scale_fill_manual(values=es.colors) +
                 scale_color_manual(values=es.colors) +
                 labs(x="Unprotected wetland percent (z)",
                      y="Ecosystem service (z)",fill="",color="") +
                 theme(text = element_text(size=14),
                       legend.position = "none") 

# plot lines
ex.es.line.df = data.frame(matrix(nrow=0,ncol=7))
colnames(ex.es.line.df) = c("service","ssp_time","scope","extreme",
                            "mean","lower","upper")
for (i in 1:n.es) {
  es.i = es.vars[i]
  ex.i = climate.vars.order[i]
  for (j in 1:n.sp) {
    ssp.j = ssps[j]
    for (k in 1:n.t) {
      time.k = times[k]
      scale.clim.df.jk = subset(scale.climate.df, ssp==ssp.j & time==time.k)[,ex.i]
      if (ex.i == "Frost days") {
        scale.clim.df.jk[,"Frost days"] = -scale.clim.df.jk[,"Frost days"]
      } 
      for (l in 1:n.sc) {
        s.l = scopes[l]
        lm.ijkl = ex.es.lm.list[[es.i]][[ssp.j]][[time.k]][[s.l]]
        ex.area.seq = seq(min(scale.clim.df.jk),
                          max(scale.clim.df.jk),
                          by=0.1)
        n.ex = length(ex.area.seq)
        
        # new dataframe with predictor variable (area)
        plot_data = data.frame(extreme = ex.area.seq)
        
        # apply model to new data
        linpred_draws = fitted(lm.ijkl, 
                               newdata = plot_data, 
                               scale = "linear", 
                               summary = FALSE) 
        
        # calculate mean and hdi
        fit_summary = data.frame(mean = apply(linpred_draws, 2, mean),
                                 lower = apply(linpred_draws, 2, function(x) hdi(x, ci = 0.90)$CI_low),
                                 upper = apply(linpred_draws, 2, function(x) hdi(x, ci = 0.90)$CI_high))
        
        # Combine original predictor with the estimated values
        plot_data = bind_cols(plot_data, as_tibble(fit_summary))
        plot_data = cbind(data.frame(service = rep(es.labels[i], n.ex),
                                     ssp_time = rep(paste(ssp.j, time.k, sep=" x "), n.ex),
                                     scope = rep(s.l, n.ex)),
                          plot_data)
        
        # add to full dataframe
        ex.es.line.df = rbind(ex.es.line.df, plot_data)
      }
    }
  }
}

write.csv(ex.es.line.df, 
          "dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Lines.csv",
          row.names=F)

ex.es.line.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Lines.csv")
ssp.j = ssps[2]
time.k = times[1]
scale.clim.df.jk = subset(subset(scale.climate.df, ssp == ssp.j & time == time.k),
                          select=-c(region, ssp_time))
scale.eco.clim.df.jk = inner_join(scale.eco.df, scale.clim.df.jk, by = "county")
scale.eco.clim.df.jk[,"Frost days"] = -scale.eco.clim.df.jk[,"Frost days"]
scale.eco.clim.df.jk$scope = "Counties without ordinances"
for (i in 1:nrow(scale.eco.clim.df.jk)) {
  if (scale.eco.clim.df.jk$county[i] %in% protected.counties) {
    scale.eco.clim.df.jk$scope[i] = "Entire state"
  }
}
colnames(scale.eco.clim.df.jk)[3:6] = es.labels

ex.es.plot.pnts = data.frame(matrix(nrow=0,ncol=7))
colnames(ex.es.plot.pnts) = c("county","region","scope",
                              "service","service_value",
                              "extreme","extreme_value")
cols.i = c("county","region","scope","service_value","extreme_value")
for (i in 1:n.es) {
  es.i = es.labels[i]
  ex.i = climate.vars.order[i]
  ex.es.df.i = data.frame(matrix(nrow=nrow(scale.eco.clim.df.jk),ncol=7))
  colnames(ex.es.df.i) = c("county","region","scope",
                           "service","service_value",
                           "extreme","extreme_value") 
  ex.es.df.i[,cols.i] = scale.eco.clim.df.jk[,c("county","region","scope",es.i,ex.i)]
  ex.es.df.i$service = es.i
  ex.es.df.i$extreme = ex.i
  ex.es.plot.pnts = rbind(ex.es.plot.pnts,ex.es.df.i)
}

p.ex.es.linear = ggplot() +
                 geom_point(data=ex.es.plot.pnts,
                            aes(x=extreme_value,
                                y=service_value,
                                color=factor(service, levels=es.labels)),
                            size=1) +
                 geom_line(data=subset(ex.es.line.df, ssp_time == "SSP3-7.0 x 2041-2070"),
                           aes(x=extreme,
                               y=mean,
                               color=factor(service, levels=es.labels))) +
                 geom_ribbon(data=subset(ex.es.line.df, ssp_time == "SSP3-7.0 x 2041-2070"),
                             aes(x=extreme,
                                 ymax=upper,
                                 ymin=lower,
                                 fill=factor(service, levels=es.labels)),
                             alpha=0.3, color=NA) +
                 scale_fill_manual(values=es.colors) +
                 scale_color_manual(values=es.colors) +
                 labs(x="Change in climate extreme (z)",
                      y="Ecosystem service (z)",fill="",color="") +
                 theme(text = element_text(size=14),
                       legend.position = "none") +
                 facet_wrap(.~factor(service, levels=es.labels), ncol=2,
                            scales="free_x")

p.linear = p.un.es.linear/p.ex.es.linear


################################################################################
# trivariate models?

es.ex.un.lm.df= data.frame(matrix(nrow = n.es*n.ex*n.sp*n.t*n.sc, ncol = 7))
colnames(es.ex.un.lm.df) = c("service","extreme","ssp_time","scope",
                          "int_mean","int_lower","int_upper")
n = 1
w = 3
es.ex.un.lm.list = list()
for (i in 1:n.es) {
  es.i = es.vars[i]
  es.ex.un.lm.list[[es.i]] = list()
  for (j in 1:n.ex) {
    ex.j = climate.vars.order[j]
    es.ex.un.lm.list[[es.i]][[ex.j]] = list()
    for (k in 1:n.sp) {
      ssp.k = ssps[k]
      es.ex.un.lm.list[[es.i]][[ex.j]][[ssp.k]] = list()
      for (l in 1:n.t) {
        time.l = times[l]
        ssp.time.kl = paste(ssp.k, time.l, sep=" x ")
        es.ex.un.lm.list[[es.i]][[ex.j]][[ssp.k]][[time.l]] = list()
        
        # join ecosystem services and climate data for given ssp and time combination
        scale.clim.df.kl = subset(subset(scale.climate.df, ssp == ssp.k & time == time.l),
                                  select=-c(region, ssp_time))
        scale.eco.clim.df.kl = inner_join(scale.eco.df, scale.clim.df.kl, by = "county")
        scale.eco.clim.df.kl[,"Frost days"] = -scale.eco.clim.df.kl[,"Frost days"]
        scale.eco.clim.unpro.df.kl = inner_join(scale.eco.clim.df.kl, scale.unpro.df)
        
        # subset based on scope
        for (m in 1:n.sc) {
          s.m = scopes[m]
          if (s.m == "Entire state") {
            scale.eco.clim.unpro.ijklm = scale.eco.clim.unpro.df.kl[,c("county","region",es.i,ex.j,water.reg.labels[w])]
          } else {
            scale.eco.clim.unpro.ijklm = subset(scale.eco.clim.unpro.df.kl[,c("county","region",es.i,ex.j,water.reg.labels[w])],
                                                !(county %in% protected.counties))
          }
          
          colnames(scale.eco.clim.unpro.ijklm)[3:5] = c("service","extreme","percent")
          lm.ijklm = brm(service ~ extreme * percent, 
                         data = scale.eco.clim.unpro.ijklm,
                         family = gaussian())
          es.ex.un.lm.list[[es.i]][[ex.j]][[ssp.k]][[time.l]][[s.m]] = lm.ijklm
          es.ex.un.lm.df[n,"service"] = es.labels[i]
          es.ex.un.lm.df[n,"extreme"] = ex.j
          es.ex.un.lm.df[n,"ssp_time"] = ssp.time.kl
          es.ex.un.lm.df[n,"scope"] = s.m
          es.ex.un.lm.df[n,"int_mean"] = fixef(lm.ijklm)[4,1]
          es.ex.un.lm.df[n,c("int_lower","int_upper")] = hdi(lm.ijklm, ci = 0.90, effects = "fixed")[4,c("CI_low","CI_high")]
          n = n + 1
        }
      }
    }
  }
}

write.csv(es.ex.un.lm.df, 
          "dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Unprotected_Percents_Effect_Sizes.csv")

es.ex.un.lm.df = read.csv("dual-risk-repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Unprotected_Percents_Effect_Sizes.csv")
es.ex.un.lm.df.filter = subset(es.ex.un.lm.df,!((service=="Floodwater storage capacity") & (extreme %in% climate.vars.order[1:3])))
p.int = ggplot(es.ex.un.lm.df.filter) +
               geom_vline(xintercept=0, color="black",linetype="dashed") + 
               geom_point(aes(x=int_mean,
                              y=factor(ssp_time, levels=ssp.time.order),
                              color=factor(service, levels = es.labels),
                              shape=factor(service, levels = es.labels),
                              size=factor(service, levels = es.labels)),
                          position=position_dodge(0.5)) +
               geom_errorbar(aes(xmin=int_lower, xmax=int_upper,
                                 y=factor(ssp_time, levels=ssp.time.order),
                                 color=factor(service, levels = es.labels)),
                             position=position_dodge(0.5),
                             width=0.3) +
               labs(y="Shared socioeconomic pathway\nby climatology period",
                    x="Interaction effect size",
                    color="Ecosystem service",
                    shape="Ecosystem service",
                    size="Ecosystem service") +
               scale_color_manual(values=es.colors) +
               scale_shape_manual(values=es.shapes) +
               scale_size_manual(values=c(2,2,2.5,2)) +
               theme(text = element_text(size=14)) +
               facet_wrap(.~factor(extreme, levels=climate.vars.order), 
                          ncol=2)
p.int
ggsave("Manuscript/Main_Figures/Figure5_Interaction_Effect_Sizes.jpeg", 
       plot=p.int, width=26, height=14, units="cm", dpi=600)
