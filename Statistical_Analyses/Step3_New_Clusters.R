setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Dissertation/Ch5_Dual_Wetland_Risk")

library(tidyverse)
library(reshape2)
library(brms)
library(bayestestR)
library(ggtext)
library(factoextra)
library(patchwork)

################################################################################
# run PCAs for all SSP and time interval combinations

# ssps
ssps = c("SSP2-4.5","SSP3-7.0")
ssp_cols = c("ssp245","ssp370")
n_s = length(ssps)

# time intervals
times = c("2041-2070","2071-2100")
time_cols = c("2041_2070","2071_2100")
n_t = length(times)

# select columns to use for PCA
pca_cols = c("plantn","herpn","carbon","flood",
             "area_spf","percent_spf",
             "row_crop_area","row_crop_percent",
             "imp_cover_area","imp_cover_percent",
             "fd","mxt100F","mdsl","mwsl")
pca_col_labels = c("Plant N","Herp N","Carbon","Flood",
                   "Unpro Area","Unpro Percent",
                   "Crops Area","Crops Percent",
                   "Urban Area","Urban Percent",
                   "FD","MXT100F","MDSL","MWSL")
combo_pca_df = data.frame(scaled_df[,c("county")])
colnames(combo_pca_df) = "county"
for (i in 1:n_s) {
  for (j in 1:n_t) {
    # read in scaled variables
    scaled_df = read.csv(paste("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Converted_Areas_Climate_Extremes",
                               ssps[i], times[j], "Scaled.csv", sep="_"))
    
    # update column names
    scaled_df = scaled_df[,c("county","region",pca_cols)]
    colnames(scaled_df)[3:(2+length(pca_cols))] = pca_col_labels
    
    # run PCAs on services and risks separately
    pca_services = prcomp(scaled_df[,pca_col_labels[1:4]], center = T, scale. = T)
    pca_risks = prcomp(scaled_df[,pca_col_labels[5:14]], center = T, scale. = T)
    
    # combine first axes and join to file
    if (i == 1 & j == 1) {
      service_df = data.frame(cbind(scaled_df[,"county"], pca_services$x[,"PC1"]))
      colnames(service_df)[1:2] = c("county","services")
      service_risk_df = data.frame(cbind(service_df, pca_risks$x[,"PC1"]))
      colnames(service_risk_df)[3] = paste("risks", ssp_cols[i], time_cols[j], sep="_")
      combo_pca_df = left_join(combo_pca_df, 
                               service_risk_df,
                               by="county")
    } else {
      risk_df = data.frame(cbind(scaled_df[,"county"], pca_risks$x[,"PC1"]))
      colnames(risk_df)[1:2] = c("county",paste("risks", ssp_cols[i], time_cols[j], sep="_"))
      combo_pca_df = left_join(combo_pca_df,
                               risk_df,
                               by="county")
    }
  }
}
write.csv(combo_pca_df,
          "dual-risk-repo/Statistical_Analyses/All_PCAs/Services_Risks_First_PCs.csv",
          row.names=F)

################################################################################

# read in cluster results
cluster_df = read.csv("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Converted_Areas_Climate_Extremes_SSP245_2041_2070_Clusters.csv")
colnames(cluster_df)[1:2] = c("county","region")

# read in hot spot comparison results
hotspot_df = read.csv("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Converted_Areas_Climate_Extremes_SSP245_2041_2070_Hot_Spot_Comparison.csv")
colnames(hotspot_df)[1:2] = c("county","region")

# number of counties in each cluster
cluster_df$ones = 1
cluster_sum = cluster_df %>%
  group_by(CLUSTER_NAME) %>%
  summarize(total_counties = sum(ones))
cluster_sum

# scree plots
fviz_eig(pca_services, 
         addlabels = TRUE,  
         ylim = c(0, 70),   
         main = "Scree Plot")
fviz_eig(pca_risks, 
         addlabels = TRUE,  
         ylim = c(0, 70),   
         main = "Scree Plot")

# update comparisons type column
hotspot_df$Comparison_Type[hotspot_df$Comparison_Type == "High services × High risks"] = "Higher services × Higher risks"
hotspot_df$Comparison_Type[hotspot_df$Comparison_Type == "High services × Low risks"] = "Higher services × Lower risks"
hotspot_df$Comparison_Type[hotspot_df$Comparison_Type == "Low services × High risks"] = "Lower services × Higher risks"

# services PCA biplot
service_scores = as.data.frame(pca_services$x)
service_scores$CLUSTER_NAME = cluster_df$CLUSTER_NAME
service_scores$Comparison_Type = hotspot_df$Comparison_Type
p_services = fviz_pca_biplot(
                pca_services,
                label = "var",
                geom.ind = "none",
                repel = TRUE) +
                geom_point(
                  data = service_scores,
                  aes(PC1,
                      PC2,
                      color = CLUSTER_NAME,
                      shape = Comparison_Type),
                  size = 3) +
                scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                stat_ellipse(
                  data = service_scores,
                  aes(PC1,
                      PC2,
                      group = CLUSTER_NAME,
                      color = CLUSTER_NAME),
                  inherit.aes = FALSE,
                  linewidth = 0.5) +
                stat_ellipse(
                  data = service_scores,
                  mapping = aes(
                    x = PC1,
                    y = PC2,
                    group = CLUSTER_NAME,
                    fill = CLUSTER_NAME),
                  inherit.aes = FALSE,
                  geom = "polygon",
                  alpha = 0.15,
                  color = NA) +
                theme(text = element_text(size=18),
                      legend.position = "none") +
                labs(title = NULL, 
                     x="PC1 (49.4%)",
                     y="PC2 (20.0%)",
                     color="Cluster",
                     shape="Hot spot comparison") +
                annotate("text",
                         x = 7.5,
                         y = 9,
                         label = "a",
                         size = 16)
p_services

# risks PCA biplot
risk_scores = as.data.frame(pca_risks$x)
risk_scores$CLUSTER_NAME = cluster_df$CLUSTER_NAME
risk_scores$Comparison_Type = hotspot_df$Comparison_Type
p_risks = fviz_pca_biplot(pca_risks,
                label = "var",
                geom.ind = "none",
                repel = TRUE) +
                geom_point(
                  data = risk_scores,
                  aes(PC1,
                      PC2,
                      color = CLUSTER_NAME,
                      shape = Comparison_Type),
                  size = 3) +
                scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                stat_ellipse(
                  data = risk_scores,
                  aes(PC1,
                      PC2,
                      group = CLUSTER_NAME,
                      color = CLUSTER_NAME),
                  inherit.aes = FALSE,
                  linewidth = 0.5)  +
                stat_ellipse(
                  data = risk_scores,
                  mapping = aes(
                    x = PC1,
                    y = PC2,
                    group = CLUSTER_NAME,
                    fill = CLUSTER_NAME),
                  inherit.aes = FALSE,
                  geom = "polygon",
                  alpha = 0.15,
                  color = NA) +
                labs(title = NULL, 
                     x="PC1 (36.4%)",
                     y="PC2 (20.2%)",
                     color="Cluster",
                     shape="Hot spot comparison",
                     fill="Cluster") +
                theme(text = element_text(size=18)) +
                annotate("text",
                         x = 4.5,
                         y = 12.5,
                         label = "b",
                         size = 16)
p_risks
                        
p_axes = p_services + p_risks
p_axes
ggsave("Manuscript/Main_Figures/Figure4_Service_Risk_PCAs.jpeg", 
       p_axes, width = 20, height = 10, dpi = 800)

###############################################################################
# plot service PC v. risk PC

# combine PCs with clusters
combo_pca_df = data.frame(combo_pca_df)
for (i in 2:3) { combo_pca_df[,i] = as.numeric(combo_pca_df[,i]) }
cluster_pca_df = left_join(left_join(combo_pca_df, 
                           cluster_df[,c("county","CLUSTER_NAME")],
                           by="county"),
                           hotspot_df[,c("county","Comparison_Type")],
                           by="county")

# fix labels
cluster_pca_df$Comparison_Type[cluster_pca_df$Comparison_Type == "High services × High risks"] = "Higher services × Higher risks"
cluster_pca_df$Comparison_Type[cluster_pca_df$Comparison_Type == "High services × Low risks"] = "Higher services × Lower risks"
cluster_pca_df$Comparison_Type[cluster_pca_df$Comparison_Type == "Low services × High risks"] = "Lower services × Higher risks"

p_axes = ggplot(cluster_pca_df,
                aes(x=risks_pc1,
                    y=services_pc1,
                    color=CLUSTER_NAME,
                    shape=Comparison_Type)) +
                geom_point() +
                geom_text(aes(label = county,
                              color = CLUSTER_NAME), 
                          vjust = -0.5) +
                scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                labs(x="More urban conversion + less frost days ← Risk axis → More agricultural conversion + extreme heat, dry spells, & wet spells",
                     y="Wetland service axis → More plant richness, herpetofauna richness, carbon storage, & floodwater storage capacity",
                     color="Cluster",
                     shape="Hot spot comparison")
p_axes
ggsave("Manuscript/Main_Figures/Figure6_Service_Risk_PCAs.jpeg", 
       p_axes, width = 10.5, height = 9, dpi = 800)

################################################################################
# bayesian analysis

# read in wetland ecosystem service estimates
eco_df = read.csv("dual-risk-repo/County_Summaries/All_County_Ecosystem_Services.csv")
colnames(eco_df)[1:2] = c("county","region")
cluster_eco_join = inner_join(cluster_df[,c("county","CLUSTER_ID","CLUSTER_NAME")],
                              eco_df, by="county")
eco_vars = colnames(eco_df)[3:6]

# read in unprotected wetland area estimates
unpro_area_df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Areas.csv")
unpro_area_df = unpro_area_df[,c("NAME","mean_SPF_brinkerhoff")]
colnames(unpro_area_df) = c("county","area_spf")

# read in unprotected wetland percent estimates
unpro_percent_df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Unprotected_Wetland_Percents.csv")
unpro_percent_df = unpro_percent_df[,c("NAME","mean_SPF_brinkerhoff")]
colnames(unpro_percent_df) = c("county","percent_spf")

# join unprotected wetland areas and percents 
cluster_eco_unpro_join = inner_join(inner_join(cluster_eco_join, 
                                               unpro_percent_df, 
                                               by="county"),
                                    unpro_area_df, by="county")

# read in converted wetland area estimates
converted_df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Wetland_Area_Percents.csv")
converted_df = converted_df[,c("NAME","County_Group",
                               "row_crop_area","imp_cover_area",
                               "row_crop_percent","imp_cover_percent")]
colnames(converted_df)[1:2] = c("county","region")
cluster_eco_unpro_conv_join = inner_join(cluster_eco_unpro_join, 
                                         converted_df, 
                                         by=c("county","region"))

# read in climate data
climate_df = read.csv("Dual-Risk-Repo/County_Summaries/All_County_Climate_Extremes.csv")
climate_vars = unique(climate_df$variable)
climate_vars_order = c("-&Delta;(Frost days)",
                       "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
                       "&Delta;(Max. length dry spell)", 
                       "&Delta;(Max. length wet spell)")
n_ex = length(climate_vars)
climate_wide_df = pivot_wider(data = climate_df,
                              names_from = variable,
                              values_from = value) 
climate_subset_df = subset(climate_wide_df, ssp=="SSP2-4.5" & time=="2041-2070") %>%
  select(-c(ssp, time, ssp_time))
unscaled_df = inner_join(cluster_eco_unpro_conv_join, 
                         climate_subset_df,
                         by=c("county","region"))

# normalize all variables and collect means
v_means = list()
vars = colnames(unscaled_df)[5:length(unscaled_df)]
n_v = length(vars)
scaled_df = unscaled_df
for (i in 1:n_v) {
  v_i = vars[i]
  mean_i = mean(unscaled_df[,v_i])
  scaled_df[,v_i] = unscaled_df[,v_i]/mean_i
  v_means[[v_i]] = mean_i
}

# fit simple linear models with intercepts for each variable by cluster
v_lms = list()
v_int_df = data.frame(matrix(nrow=0,ncol=8))
colnames(v_int_df) = c("var","cluster_raw","rhat","mean",
                       "CI_5","CI_95","CI_25","CI_75")
for (i in 1:n_v) {
  v_i = vars[i]
  df_i = scaled_df[,c("CLUSTER_NAME",v_i)]
  colnames(df_i) = c("cluster","var")
  lm_i = brm(var ~ 0 + cluster, 
             data = df_i,
             family = gaussian(),
             chains=10, iter=10000)
  v_lms[[v_i]] = lm_i
  v_df_i = data.frame(matrix(nrow=4, ncol=8))
  colnames(v_df_i) = c("var","cluster_raw","rhat","mean","CI_5","CI_95","CI_25","CI_75")
  v_df_i$var = v_i
  v_df_i$cluster_raw = row.names(summary(lm_i)$fixed)
  v_df_i$rhat = summary(lm_i)$fixed$Rhat
  v_df_i$mean = summary(lm_i)$fixed$Estimate
  v_df_i[,c("CI_5","CI_95")] = hdi(lm_i, ci = 0.90, effects = "fixed")[,c("CI_low","CI_high")]
  v_df_i[,c("CI_25","CI_75")] = hdi(lm_i, ci = 0.50, effects = "fixed")[,c("CI_low","CI_high")]
  v_int_df = rbind(v_int_df, v_df_i)
}

clean_cluster_names = c("Northeast","Northwest","Southeast","Southwest")
raw_cluster_names = paste("cluster", clean_cluster_names, sep="")
v_int_df$cluster = 0
for (i in 1:4) {
  v_int_df$cluster[v_int_df$cluster_raw == raw_cluster_names[i]] = clean_cluster_names[i]
}

# rescale data
v_int_df_rescale = v_int_df
num_cols = c("mean","CI_5","CI_95","CI_25","CI_75")
for (i in 1:n_v) {
  v_i = vars[i]
  v_row_ind = which(v_int_df$var == v_i)
  v_int_df_rescale[v_row_ind, num_cols] = v_int_df[v_row_ind, num_cols] * v_means[[v_i]]
}
write.csv(v_int_df, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Unscaled.csv", row.names=F)
write.csv(v_int_df_rescale, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Natural_Scale.csv", row.names=F)

# make ecosystem service plots
v_int_df_rescale = read.csv("dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Natural_Scale.csv")
var_order = c( "Plant.species.richness",
               "&Delta;(Max. length wet spell)",
               "imp_cover_percent",
               "imp_cover_area",
               "Herpetofauna.species.richness",
               "&Delta;(Max. length dry spell)",
               "row_crop_percent",
               "row_crop_area",
               "Floodwater.storage.capacity",
               "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
               "percent_spf",                                           
               "area_spf",
               "Carbon.storage",
               "-&Delta;(Frost days)")
var_name_update = c("Plant species richness", 
                    "&Delta;(Max. length wet spell)",
                    "Wetland area converted to impervious (%)",
                    "Wetland area converted to impervious (ha)",
                    "Herpetofauna species richness", 
                    "&Delta;(Max. length dry spell)", 
                    "Wetland area converted to row crops (%)",
                    "Wetland area converted to row crops (ha)",
                    "Floodwater storage capacity (1,000 m<sup>3</sup>)",
                    "&Delta;(Days with max. temp. over\n100&deg;F [38&deg;C])",
                    "Unprotected wetland percentage (%)",
                    "Unprotected wetland area (ha)",
                    "Carbon storage (1,000 Gg)", 
                    "-&Delta;(Frost days)")
n_v = length(var_order)
v_int_df_rescale$var_new = 0
for (i in 1:n_v) {
  v_int_df_rescale$var_new[v_int_df_rescale$var == var_order[i]] = var_name_update[i]
}
cluster_color = c("red4","salmon2","gray60","gray20")
cluster_order = c("Southwest","Southeast","Northwest","Northeast")
p_int = ggplot(v_int_df_rescale) +
  geom_vline(xintercept=0, color="gray") + 
  geom_point(aes(x=mean,
                 y=factor(cluster, levels=cluster_order),
                 color=factor(cluster, levels=cluster_order),
                 shape=factor(cluster, levels=cluster_order),
                 size=factor(cluster, levels=cluster_order))) +
  geom_errorbar(aes(xmin=CI_5,
                    xmax=CI_95,
                    y=factor(cluster, levels=cluster_order),
                    color=factor(cluster, levels=cluster_order)),
                width=0.3)  +
  geom_errorbar(aes(xmin=CI_25,
                    xmax=CI_75,
                    y=factor(cluster, levels=cluster_order),
                    color=factor(cluster, levels=cluster_order)),
                width=0, linewidth=1.8) +  
  facet_wrap(.~factor(var_new, levels=rev(var_name_update)), 
             scales="free_x", 
             ncol=4,
             dir="v") +
  theme(strip.text = element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #legend.position = "none",
        text = element_text(size = 14),
        plot.margin = margin(t = 6, r = 12, b = 6, l = 2, unit = "pt")) +
  labs(y="", 
       x="Posterior interval", 
       color="Cluster",
       shape="Cluster",
       size="Cluster") +
  scale_color_manual(values=cluster_color) +
  scale_shape_manual(values=seq(18,15)) +
  scale_size_manual(values=c(4,3,3,3)) +
  scale_x_continuous(labels = scales::label_comma())
p_int
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD1_Cluster_Posterior_Comparisons.jpeg", 
       plot=p_int, width=34, height=30, units="cm", dpi=800)
