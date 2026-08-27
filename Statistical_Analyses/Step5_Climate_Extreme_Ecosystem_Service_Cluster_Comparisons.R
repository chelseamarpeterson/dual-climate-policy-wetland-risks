setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(tidyverse)
library(reshape2)
library(brms)
library(bayestestR)
library(ggtext)

# read in cluster results
cluster.df = read.csv("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Areas_Climate_Extremes_SSP245_2041_2070_Clusters.csv")
colnames(cluster.df)[17] = "county"

# give clusters names
cluster.df$CLUSTER_NAME = 0
cluster.df$CLUSTER_NAME[cluster.df$CLUSTER_ID == 4] = "Northeast"
cluster.df$CLUSTER_NAME[cluster.df$CLUSTER_ID == 1] = "Northwest"
cluster.df$CLUSTER_NAME[cluster.df$CLUSTER_ID == 2] = "Southeast"
cluster.df$CLUSTER_NAME[cluster.df$CLUSTER_ID == 3] = "Southwest"

sum(cluster.df$CLUSTER_NAME == "Northeast")
sum(cluster.df$CLUSTER_NAME == "Northwest")
sum(cluster.df$CLUSTER_NAME == "Southeast")
sum(cluster.df$CLUSTER_NAME == "Southwest")

################################################################################
# run simple stats to compare ecosystem services, unprotected wetland areas and
# percents, and climate extremes across clusters

# read in wetland ecosystem service estimates
eco.df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco.df)[1:2] = c("county","region")
cluster.eco.join = inner_join(cluster.df[,c("county","CLUSTER_ID","CLUSTER_NAME")],
                              eco.df, by="county")
eco.vars = colnames(eco.df)[3:6]

# read in unprotected wetland area and percent estimates
unpro.percent.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percent_Totals.csv")
unpro.area.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Area_Totals.csv")
unpro.percent.df = unpro.percent.df[,c("NAME","mean_SPF_brinkerhoff")]
unpro.area.df = unpro.area.df[,c("NAME","mean_SPF_brinkerhoff")]
colnames(unpro.percent.df) = c("county","percent_spf")
colnames(unpro.area.df) = c("county","area_spf")
cluster.eco.unpro.join = inner_join(inner_join(cluster.eco.join, unpro.percent.df, by="county"),
                                    unpro.area.df, by="county")

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
climate.subset.df = subset(climate.wide.df, ssp=="SSP2-4.5" & time=="2041-2070") %>%
                           select(-c(ssp, time, ssp_time))
cluster.eco.un.clim.join = inner_join(cluster.eco.unpro.join, climate.subset.df,
                                      by=c("county","region"))

# normalize all variables and collect means
v.means = list()
vars = colnames(cluster.eco.un.clim.join)[5:14]
n.v = length(vars)
scale.df = cluster.eco.un.clim.join
for (i in 1:n.v) {
  v.i = vars[i]
  mean.i = mean(cluster.eco.un.clim.join[,v.i])
  scale.df[,v.i] = cluster.eco.un.clim.join[,v.i]/mean.i
  v.means[[v.i]] = mean.i
}

# fit simple linear models with intercepts for each variable by cluster
v.lms = list()
v.int.df = data.frame(matrix(nrow=0,ncol=8))
colnames(v.int.df) = c("var","cluster_raw","rhat","mean","CI_5","CI_95","CI_25","CI_75")
for (i in 1:n.v) {
  v.i = vars[i]
  df.i = scale.df[,c("CLUSTER_NAME",v.i)]
  colnames(df.i) = c("cluster","var")
  lm.i = brm(var ~ 0 + cluster, 
             data = df.i,
             family = gaussian(),
             chains=10, iter=10000)
  v.lms[[v.i]] = lm.i
  v.df.i = data.frame(matrix(nrow=4,ncol=8))
  colnames(v.df.i) = c("var","cluster_raw","rhat","mean","CI_5","CI_95","CI_25","CI_75")
  v.df.i$var = v.i
  v.df.i$cluster_raw = row.names(summary(lm.i)$fixed)
  v.df.i$rhat = summary(lm.i)$fixed$Rhat
  v.df.i$mean = summary(lm.i)$fixed$Estimate
  v.df.i[,c("CI_5","CI_95")] = hdi(lm.i, ci = 0.90, effects = "fixed")[,c("CI_low","CI_high")]
  v.df.i[,c("CI_25","CI_75")] = hdi(lm.i, ci = 0.50, effects = "fixed")[,c("CI_low","CI_high")]
  v.int.df = rbind(v.int.df, v.df.i)
}
clean_cluster_names = c("Northeast","Northwest","Southeast","Southwest")
raw_cluster_names = paste("cluster", clean_cluster_names, sep="")
v.int.df$cluster = 0
for (i in 1:4) {
  v.int.df$cluster[v.int.df$cluster_raw == raw_cluster_names[i]] = clean_cluster_names[i]
}

# rescale data
v.int.df.rescale = v.int.df
num.cols = c("mean","CI_5","CI_95","CI_25","CI_75")
for (i in 1:n.v) {
  v.i = vars[i]
  v.row.ind = which(v.int.df$var == v.i)
  if (v.i == "area_spf") {
    v.int.df.rescale[v.row.ind, num.cols] = v.int.df[v.row.ind, num.cols] * v.means[[v.i]] / 1000
  } else {
    v.int.df.rescale[v.row.ind, num.cols] = v.int.df[v.row.ind, num.cols] * v.means[[v.i]]
  }
}
write.csv(v.int.df, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Unscaled.csv", row.names=F)
write.csv(v.int.df.rescale, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Natural_Scale.csv", row.names=F)

# make ecosystem service plots
var_name_update = c("Plant species richness", 
                    "Herpetofauna species richness", 
                    "Carbon storage (1,000 Gg)", 
                    "Floodwater storage capacity (1,000 m<sup>3</sup>)",
                    "Unprotected wetland percentage (%)",
                    "Unprotected wetland area (1,000 ha)",
                     climate.vars.order)
v.int.df.rescale$var_new = 0
for (i in 1:n.v) {
  v.int.df.rescale$var_new[v.int.df.rescale$var == vars[i]] = var_name_update[i]
}
cluster.color = c("red4","salmon2","gray60","gray20")
cluster.order = c("Southwest","Southeast","Northwest","Northeast")
p.int = ggplot(v.int.df.rescale) +
               geom_vline(xintercept=0, color="gray") + 
               geom_point(aes(x=mean,
                              y=factor(cluster, levels=cluster.order),
                              color=factor(cluster, levels=cluster.order)),
                          size=3) +
               geom_errorbar(aes(xmin=CI_5,
                                 xmax=CI_95,
                                 y=factor(cluster, levels=cluster.order),
                                 color=factor(cluster, levels=cluster.order)),
                             width=0.3)  +
               geom_errorbar(aes(xmin=CI_25,
                                 xmax=CI_75,
                                 y=factor(cluster, levels=cluster.order),
                                 color=factor(cluster, levels=cluster.order)),
                             width=0, linewidth=1.8) +  
               facet_wrap(.~var_new, scales="free_x", ncol=2) +
               theme(strip.text = element_markdown(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     legend.position = "none",
                     text = element_text(size = 14),
                     plot.margin = margin(t = 6, r = 12, b = 6, l = 2, unit = "pt")) +
               labs(y="",x="Posterior interval",color="Cluster") +
               scale_color_manual(values=cluster.color) +
               scale_x_continuous(labels = scales::label_comma())
p.int
ggsave("dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Figure5_Cluster_Posterior_Comparisons.jpeg", 
       plot=p.int, width=17, height=27, units="cm", dpi=600)
