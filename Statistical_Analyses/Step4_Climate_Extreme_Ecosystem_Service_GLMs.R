setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(reshape2)
library(patchwork)
library(RColorBrewer)
library(brms)

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
n.ssp = length(ssps)

# time intervals
times = c("2041-2070","2071-2100")
n.t = length(times)

# join service and unprotected wetland area dataframes
scale.eco.unpro.df = inner_join(scale.eco.df, scale.unpro.df, by="county")

################################################################################
# calculate spearman rho

# each service v. unprotected wetland area
rho.un.es.list = list()
for (i in 1:n.es) {
  es.i = es.vars[i]
  print(es.i)
  rho.un.es.list[[es.i]] = list()
  for (j in 1:n.w) {
    wr.j = rev(water.reg.labels)[j]
    rho.un.es.list[[es.i]][[wr.j]] = round(cor(scale.eco.unpro.df[,wr.j], 
                                               scale.eco.unpro.df[,es.i], method="spearman"),2)
    test = cor.test(scale.eco.unpro.df[,wr.j], scale.eco.unpro.df[,es.i], method = "spearman")
    print(test$p.value)
  }
}

# each service v. climate extreme
rho.ex.es.list = list()
for (i in 1:n.es) {
  es.i = es.vars[i]
  print(es.i)
  ex.i = climate.vars.order[i]
  rho.ex.es.list[[es.i]] = list()
  for (j in 1:n.ssp) {
    ssp.j = ssps[j]
    rho.ex.es.list[[es.i]][[ssp.j]] = list()
    for (k in 1:n.t) {
      time.k = times[k]
      
      # join climate data for given ssp
      climate.df.jk = subset(subset(scale.climate.df, ssp == ssp.j & time == time.k),
                             select=-c(region, ssp_time))
      eco.climate.df = inner_join(scale.eco.df, climate.df.jk, by = "county")
      eco.climate.df[,"Frost days"] = -eco.climate.df[,"Frost days"]
      
      # calculate rho
      rho.ex.es.list[[es.i]][[ssp.j]][[time.k]] = round(cor(eco.climate.df[,ex.i], 
                                                            eco.climate.df[,es.i], method="spearman"),2)
      test = cor.test(eco.climate.df[,ex.i], eco.climate.df[,es.i], method = "spearman")
      print(test$p.value)
    }
  }
}

################################################################################
# linear models with BRMS
set.seed(2718)

# ecosystem services v. unprotected wetland area
scopes = c("Entire state","Counties without ordinances")
n.s = length(scopes)
protected.counties = c("Cook","Lake","McHenry","DuPage","DeKalb","Will","Kane","Grundy")
un.es.lm.df = data.frame(matrix(nrow=n.es*n.w*n.s, ncol=9))
colnames(un.es.lm.df) = c("service","cutoff","scope",
                          "int_mean","int_lower","int_upper",
                          "slope_mean","slope_lower","slope_upper")
#un.es.lm.df = read.csv("Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Effect_Sizes.csv")
un.es.lm.list = list()
n = 1
for (i in 1:n.es) {
  es.i = es.vars[i]
  un.es.lm.list[[es.i]] = list()
  for (j in 1:n.w) {
    wr.j = water.reg.labels[j]
    un.es.lm.list[[es.i]][[wr.j]] = list()
    for (k in 1:n.s) {
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
      un.es.lm.df[n,c("int_mean","int_lower","int_upper")] = fixef(lm.ijk)[1,c(1,3,4)]
      un.es.lm.df[n,c("slope_mean","slope_lower","slope_upper")] = fixef(lm.ijk)[2,c(1,3,4)]
      n = n + 1
    }
  }
}

write.csv(un.es.lm.df, 
          "Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Effect_Sizes.csv",
          row.names=F)

# plot effect sizes
p.lm.un.es = ggplot(un.es.lm.df,
                    aes(x=slope_mean,
                        y=cutoff,
                        color=scope)) +
                    geom_point(position=position_dodge(0.9)) +
                    geom_errorbar(aes(xmin=slope_lower,xmax=slope_upper,
                                  y=cutoff,
                                  color=scope),
                                  position=position_dodge(0.9),
                                  width=0.3) +
                    labs(y="Wetland flood-freqency cutoff",
                         x="Effect size: Ecosystem service v. unprotected wetland area",
                         color="") +
                    scale_color_manual(values=c("Counties without ordinances"="goldenrod4", 
                                                "Entire state"="grey30")) +
                    geom_vline(xintercept=0, color="black",linetype="dashed") +
                    facet_wrap(.~factor(service, levels = es.labels), ncol=4) +
                    theme(text = element_text(size=14))
p.lm.un.es

# plot lines
un.area.seq = seq(min(scale.eco.unpro.df[,water.reg.labels]),
                  max(scale.eco.unpro.df[,water.reg.labels]),
                  by=0.1)
n.un = length(un.area.seq)
un.es.line.df = data.frame(matrix(nrow=0,ncol=8))
colnames(un.es.line.df) = c("service","cutoff","scope","area",
                            "Estimate","Est.error","Q2.5","Q97.5")
#un.es.line.df = read.csv("Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Lines_SPF.csv")
for (i in 1:n.es) {
  es.i = es.vars[i]
  for (j in 1:n.w) {
    wr.j = water.reg.labels[j]
    for (k in 1:n.s) {
      s.k = scopes[k]
      lm.ijk = un.es.lm.list[[es.i]][[wr.j]][[s.k]]
      
      # new dataframe with predictor variable (area)
      plot_data = data.frame(area = un.area.seq)
      
      # apply model to new data
      fit_summary = fitted(lm.ijk, newdata = plot_data, summary = TRUE) 
      
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
          "Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Unprotected_Area_Lines_SPF.csv",
          row.names=F)

scale.eco.unpro.df$scope = "Counties without ordinances"
for (i in 1:nrow(scale.eco.unpro.df)) {
  if (scale.eco.unpro.df$county[i] %in% protected.counties) {
    scale.eco.unpro.df$scope[i] = "Entire state"
  }
}
colnames(scale.eco.unpro.df)[3:6] = es.labels
scale.eco.unpro.melt.spf = melt(scale.eco.unpro.df[,c("county","region","scope",es.labels,water.reg.labels[3])], 
                                id.vars=c("county","region","scope",water.reg.labels[3]))
colnames(scale.eco.unpro.melt.spf)[4:5] = c("area","service")
p.un.es.linear = ggplot() +
                 geom_line(data=subset(un.es.line.df, cutoff == "Semipermanently Flooded"),
                           aes(x=area,
                               y=Estimate,
                               fill=scope,
                               color=scope)) +
                 geom_ribbon(data=subset(un.es.line.df, cutoff == "Semipermanently Flooded"),
                             aes(x=area,
                                 ymax=`Q2.5`,
                                 ymin=`Q97.5`,
                                 fill=scope),
                             alpha=0.3, color=NA) +
                 geom_point(data=scale.eco.unpro.melt.spf,
                            aes(x=area, 
                                y=value,
                                fill=scope,
                                color=scope)) +
                 scale_fill_manual(values=c("Counties without ordinances"="goldenrod4", 
                                            "Entire state"="grey30")) +
                 scale_color_manual(values=c("Counties without ordinances"="goldenrod4", 
                                            "Entire state"="grey30")) +
                 labs(x="Unprotected wetland area (z)",
                      y="Ecosystem service (z)",fill="",color="") +
                 theme(text = element_text(size=14)) +
                 facet_wrap(.~factor(service, levels=es.labels), ncol=4)
p.un.es.linear

# ecosystem services v. climate extremes
ex.es.lm.df = data.frame(matrix(nrow=n.es*n.ssp*n.t*n.s, ncol=9))
colnames(ex.es.lm.df) = c("service","ssp_time","scope",
                          "int_mean","int_lower","int_upper",
                          "slope_mean","slope_lower","slope_upper")
n = 1
ex.es.lm.list = list()
for (i in 1:n.es) {
  es.i = es.vars[i]
  ex.i = climate.vars.order[i]
  ex.es.lm.list[[es.i]] = list()
  for (j in 1:n.ssp) {
    ssp.j = ssps[j]
    ex.es.lm.list[[es.i]][[ssp.j]] = list()
    for (k in 1:n.t) {
      time.k = times[k]
      ssp.time.jk = paste(ssp.j, time.k, sep=" x ")
      ex.es.lm.list[[es.i]][[ssp.j]][[time.k]] = list()
      
      # join ecosystem services and climate data for given ssp and time combination
      scale.clim.df.jk = subset(subset(scale.climate.df, ssp == ssp.j & time == time.k),
                                select=-c(region, ssp_time))
      scale.eco.clim.df.jk = inner_join(scale.eco.df, scale.clim.df.jk, by = "county")
      scale.eco.clim.df.jk[,"Frost days"] = -scale.eco.clim.df.jk[,"Frost days"]
      
      # subset based on scope
      for (l in 1:n.s) {
        s.l = scopes[l]
        if (s.l == "Entire state") {
          scale.eco.clim.df.ijkl = scale.eco.clim.df.jk[,c("county","region",es.i,ex.i)]
        } else {
          scale.eco.clim.df.ijkl = subset(scale.eco.clim.df.jk[,c("county","region",es.i,ex.i)],
                                          !(county %in% protected.counties))
        }
        
        colnames(scale.eco.clim.df.ijkl)[3:4] = c("service","extreme")
        lm.ijkl = brm(service ~ extreme, 
                      data = scale.eco.clim.df.ijkl,
                      family = gaussian())
        ex.es.lm.list[[es.i]][[ssp.j]][[time.k]][[s.l]] = lm.ijkl
        #n = which((ex.es.lm.df$service == es.labels[i] & ex.es.lm.df$ssp_time == ssp.time.jk) & (ex.es.lm.df$extreme == ex.i & ex.es.lm.df$scope == s.k))
        ex.es.lm.df[n,"service"] = es.labels[i]
        ex.es.lm.df[n,"ssp_time"] = ssp.time.jk
        ex.es.lm.df[n,"extreme"] = ex.i
        ex.es.lm.df[n,"scope"] = scopes[l]
        ex.es.lm.df[n,c("int_mean","int_lower","int_upper")] = fixef(lm.ijkl)[1,c(1,3,4)]
        ex.es.lm.df[n,c("slope_mean","slope_lower","slope_upper")] = fixef(lm.ijkl)[2,c(1,3,4)]
        n = n + 1
      }
    }
  }
}

write.csv(ex.es.lm.df, 
          "Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Effect_Sizes.csv",
          row.names=F)

ssp.time.order = rev(c(paste(ssps[1], times, sep=" x "),
                       paste(ssps[2], times, sep=" x ")))
p.lm.ex.es = ggplot(ex.es.lm.df,
                    aes(x=slope_mean,
                        y=factor(ssp_time, levels=ssp.time.order),
                        color=scope)) +
                    geom_point(position=position_dodge(0.9)) +
                    geom_errorbar(aes(xmin=slope_lower,xmax=slope_upper,
                                      y=factor(ssp_time, levels=ssp.time.order),
                                      color=scope),
                                  position=position_dodge(0.9),
                                  width=0.3) +
                    labs(y="Shared socioeconomic pathway\nby climatology period",
                         x="Effect size: Ecosystem service v. change in climate extreme",
                         color="") +
                    scale_color_manual(values=c("Counties without ordinances"="goldenrod4", 
                                                "Entire state"="grey30")) +
                    geom_vline(xintercept=0, color="black",linetype="dashed") +
                    facet_wrap(.~factor(service, levels = es.labels), ncol=4) +
                    theme(text = element_text(size=14))
p.lm.ex.es

p.glm = p.lm.un.es/p.lm.ex.es
p.glm

ggsave("Manuscript/Main_Figures/Figure3_GLM_Effect_Sizes.jpeg", 
       plot=p.glm, width=38, height=16, units="cm", dpi=600)

# plot lines
ex.es.line.df = data.frame(matrix(nrow=0,ncol=8))
colnames(ex.es.line.df) = c("service","ssp_time","scope","extreme",
                            "Estimate","Est.error","Q2.5","Q97.5")
for (i in 1:n.es) {
  es.i = es.vars[i]
  ex.i = climate.vars.order[i]
  for (j in 1:n.ssp) {
    ssp.j = ssps[j]
    for (k in 1:n.t) {
      time.k = times[k]
      scale.clim.df.jk = subset(scale.climate.df, ssp==ssp.j & time==time.k)[,ex.i]
      if (ex.i == "Frost days") {
        scale.clim.df.jk[,"Frost days"] = -scale.clim.df.jk[,"Frost days"]
      } 
      for (l in 1:n.s) {
        s.l = scopes[l]
        lm.ijkl = ex.es.lm.list[[es.i]][[ssp.j]][[time.k]][[s.l]]
        ex.area.seq = seq(min(scale.clim.df.jk),
                          max(scale.clim.df.jk),
                          by=0.1)
        n.ex = length(ex.area.seq)
        
        # new dataframe with predictor variable (area)
        plot_data = data.frame(extreme = ex.area.seq)
        
        # apply model to new data
        fit_summary = fitted(lm.ijkl, newdata = plot_data, summary = TRUE) 
        
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
          "Dual-Risk-Repo/Statistical_Analyses/Linear_Model_Output/Ecosystem_Service_Versus_Climate_Extremes_Lines.csv",
          row.names=F)

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

#nrow(scale.eco.clim.df.jk)
p.ex.es.linear = ggplot() +
                 geom_line(data=subset(ex.es.line.df, ssp_time == "SSP3-7.0 x 2041-2070"),
                           aes(x=extreme,
                               y=Estimate,
                               fill=scope,
                               color=scope)) +
                 geom_ribbon(data=subset(ex.es.line.df, ssp_time == "SSP3-7.0 x 2041-2070"),
                             aes(x=extreme,
                                 ymax=`Q2.5`,
                                 ymin=`Q97.5`,
                                 fill=scope),
                             alpha=0.3, color=NA) +
                 geom_point(data=ex.es.plot.pnts,
                            aes(x=extreme_value,
                                y=service_value,
                                color=scope)) +
                 scale_fill_manual(values=c("Counties without ordinances"="goldenrod4", 
                                            "Entire state"="grey30")) +
                 scale_color_manual(values=c("Counties without ordinances"="goldenrod4", 
                                             "Entire state"="grey30")) +
                 labs(x="Change in climate extreme (z)",
                      y="Ecosystem service (z)",fill="",color="") +
                 theme(text = element_text(size=14)) +
                 facet_wrap(.~factor(service, levels=es.labels), ncol=4,
                            scales="free_x")
p.ex.es.linear

p.linear = p.un.es.linear/p.ex.es.linear
p.linear
ggsave("Manuscript/Main_Figures/Figure4_GLM_Linear_Plots.jpeg", 
       plot=p.linear, width=32, height=14, units="cm", dpi=600)
