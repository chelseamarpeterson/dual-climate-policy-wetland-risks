setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_Dual_Wetland_Risk")

library(tidyverse)
library(reshape2)
library(brms)
library(bayestestR)
library(ggtext)
library(VineCopula)
library(factoextra)

# read in cluster results
cluster.df = read.csv("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Areas_Climate_Extremes_SSP245_2041_2070_Clusters.csv")
colnames(cluster.df)[1:2] = c("county","region")

# number of counties in each cluster
cluster.df$ones = 1
cluster.sum = cluster.df %>%
              group_by(CLUSTER_NAME) %>%
              summarize(total_counties = sum(ones))
cluster.sum

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

# read in converted wetland area estimates
ag.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Ag_Area_Totals.csv")[,c("NAME","SUM_Polygon_Area_Ha_Geodesic")]
urban.df = read.csv("dual-risk-repo/County_Summaries/Step2_County_Converted_Impervious_Area_Totals.csv")[,c("NAME","SUM_Polygon_Area_Ha_Geodesic")]
colnames(ag.df) = c("county","area_crop")
colnames(urban.df) = c("county","area_urban")
cluster.eco.unpro.conv.join = inner_join(inner_join(cluster.eco.unpro.join, ag.df, by="county"), urban.df, by="county")
  
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
unscaled.df = inner_join(cluster.eco.unpro.conv.join, climate.subset.df,
                        by=c("county","region"))

# sum unscaled data by cluster
unscaled.sum = unscaled.df %>%
               group_by(CLUSTER_NAME) %>%
               summarize(total_crops = sum(area_crop),
                         total_urban = sum(area_urban),
                         total_unpro = sum(area_spf))
unscaled.sum

# normalize all variables and collect means
v.means = list()
vars = colnames(unscaled.df)[5:16]
n.v = length(vars)
scaled.df = unscaled.df
for (i in 1:n.v) {
  v.i = vars[i]
  mean.i = mean(unscaled.df[,v.i])
  scaled.df[,v.i] = unscaled.df[,v.i]/mean.i
  v.means[[v.i]] = mean.i
}

# fit simple linear models with intercepts for each variable by cluster
v.lms = list()
v.int.df = data.frame(matrix(nrow=0,ncol=8))
colnames(v.int.df) = c("var","cluster_raw","rhat","mean","CI_5","CI_95","CI_25","CI_75")
for (i in 1:n.v) {
  v.i = vars[i]
  df.i = scaled.df[,c("CLUSTER_NAME",v.i)]
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
  v.int.df.rescale[v.row.ind, num.cols] = v.int.df[v.row.ind, num.cols] * v.means[[v.i]]
}
write.csv(v.int.df, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Unscaled.csv", row.names=F)
write.csv(v.int.df.rescale, "dual-risk-repo/Statistical_Analyses/Cluster_Comparison_Results/Variable_Posteriors_Natural_Scale.csv", row.names=F)

# make ecosystem service plots
var_name_update = c("Plant species richness", 
                    "Herpetofauna species richness", 
                    "Carbon storage (1,000 Gg)", 
                    "Floodwater storage capacity (1,000 m<sup>3</sup>)",
                    "Unprotected wetland percentage (%)",
                    "Unprotected wetland area (ha)",
                    "Wetland area converted to row crops (ha)",
                    "Wetland area converted to impervious cover (ha)",
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
       plot=p.int, width=19, height=30, units="cm", dpi=800)

################################################################################
# run PCA and plot clusters as colors

# select columns to use for PCA
pca_cols = c("Plant N","Herp N","Carbon","Flood",
             "Area","Percent","Crops","Urban",
             "FD","MXT100F","MDSL","MWSL")
colnames(cluster.df)[5:16] = pca_cols

# run PCA with standard R base function
pca_result = prcomp(cluster.df[,pca_cols], center = T, scale. = T)

# view standard deviations and proportion of variance explained
summary(pca_result)

# scree plot
fviz_eig(pca_result, 
         addlabels = TRUE,  
         ylim = c(0, 70),   
         main = "Scree Plot")

# invert PC1
pca_result$x[,"PC1"] = -pca_result$x[,"PC1"]
pca_result$rotation[,"PC1"] = - pca_result$rotation[,"PC1"]

# biplot
p.biplot = fviz_pca_biplot(pca_result, 
                label="var", 
                habillage=cluster.df$CLUSTER_NAME,
                axes = c(1, 2),
                addEllipses=TRUE,
                mean.point=FALSE,
                repel=TRUE,
                labelsize = 6,
                pointsize = 3) +
                scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_shape_manual(values = seq(15,19)) +
                labs(title = NULL, 
                     x="PC1 (29.8%) &rarr; Higher climate risk, agricultural conversion, & unprotected area",
                     y="PC2 (24.1%) &rarr; Higher ecosystem services & urban conversion",
                     color="Cluster",
                     shape="Cluster",
                     fill="Cluster") + 
                scale_y_continuous(limits=c(-4,8),
                                   breaks=seq(-4,8,by=2)) +
                theme(axis.title.x = element_markdown(),
                      axis.title.y = element_markdown(),
                      text = element_text(size=18),
                      legend.text = element_text(size=18),
                      legend.title = element_text(size=24))
p.biplot
ggsave("Manuscript/Main_Figures/Figure5_PCA.jpeg", 
       p.biplot, width = 14, height = 12, dpi = 800)

################################################################################

library(patchwork)

# run PCAs on services and risks separately
pca_services = prcomp(cluster.df[,pca_cols[1:4]], center = T, scale. = T)
pca_risks = prcomp(cluster.df[,pca_cols[5:12]], center = T, scale. = T)

# write PCs to file
#pca.services.df = cbind(cluster.df[,c("county","region")],
#                        pca_services$x)
#pca.risks.df = cbind(cluster.df[,c("county","region")],
#                     pca_risks$x)
#write.csv(pca.services.df, 
#          "dual-risk-repo/County_Summaries/Ecosystem_Service_PCs.csv",
#          row.names=F)
#write.csv(pca.risks.df, 
#          "dual-risk-repo/County_Summaries/Climate_Risk_PCs.csv",
#          row.names=F)

# scree plots
fviz_eig(pca_services, 
         addlabels = TRUE,  
         ylim = c(0, 70),   
         main = "Scree Plot")
fviz_eig(pca_risks, 
         addlabels = TRUE,  
         ylim = c(0, 70),   
         main = "Scree Plot")
# services PCA biplot
p.services = fviz_pca_biplot(pca_services, 
                           label="var", 
                           habillage=cluster.df$CLUSTER_NAME,
                           axes = c(1, 2),
                           addEllipses=TRUE,
                           mean.point=FALSE,
                           repel=TRUE,
                           labelsize = 6,
                           pointsize = 3) +
                  scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                  scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                  scale_shape_manual(values = seq(15,19)) +
                  labs(title = NULL, 
                       x="PC1 (49.4%)",
                       y="PC2 (20.0%)",
                       color="Cluster",
                       shape="Cluster",
                       fill="Cluster") + 
                  theme(axis.title.x = element_markdown(),
                        axis.title.y = element_markdown(),
                        text = element_text(size=18),
                        legend.text = element_text(size=18),
                        legend.title = element_text(size=24),
                        legend.position = "none")
p.services

# risks PCA biplot
p.risks = fviz_pca_biplot(pca_risks, 
                          label="var", 
                          habillage=cluster.df$CLUSTER_NAME,
                          axes = c(1, 2),
                          addEllipses=TRUE,
                          mean.point=FALSE,
                          repel=TRUE,
                          labelsize = 6,
                          pointsize = 3) +
                          scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                          scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                          scale_shape_manual(values = seq(15,19)) +
                          labs(title = NULL, 
                               x="PC1 (41.9%)",
                               y="PC2 (17.3%)",
                               color="Cluster",
                               shape="Cluster",
                               fill="Cluster") + 
                          theme(axis.title.x = element_markdown(),
                                axis.title.y = element_markdown(),
                                text = element_text(size=18),
                                legend.text = element_text(size=18),
                                legend.title = element_text(size=24))
p.risks
p.axes = p.services + p.risks
p.axes
ggsave("Manuscript/Supp_Figures/AppendixD/FigureD1_Service_Risk_PCAs.jpeg", 
       p.axes, width = 20, height = 10, dpi = 800)

# apply rank transformation
x = pca_risks$x[,"PC1"]
y = pca_services$x[,"PC1"]
n = length(x)
u = rank(x) / (n + 1)
v = rank(-y) / (n + 1)

# apply inverse CDF to get z scores
z.df = data.frame(x=qnorm(u), y=qnorm(v), cluster=cluster.df$CLUSTER_NAME)

# select best model with AIC
best_model = BiCopSelect(u, v, selectioncrit = "AIC")
best_model

# compute tau and tail dependence coefficients
BiCopSelect(u, v)$tau
cor.test(u, v, method = "kendall")
BiCopPar2TailDep(BiCopSelect(u, v))$upper
BiCopPar2TailDep(BiCopSelect(u, v))$lower

# create grid matrix and calculate density
grid_seq = seq(-2.75, 2.75, length.out=100)
density_matrix = outer(grid_seq, grid_seq, 
                       function(x, y) {BiCopPDF(pnorm(x), pnorm(y), best_model) * dnorm(x) * dnorm(y)})
dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
melted_density = melt(density_matrix, varnames = c("x", "y"))

# make density plot
p.copula = ggplot() +
            metR::geom_contour_fill(data=melted_density, 
                                    aes(x = x, y = y, z = value),
                                    breaks=seq(0,0.3,length.out=10)) +
            scale_fill_distiller(palette = "Greens",
                                 direction = 2,
                                 limits=c(0,0.31)) +
            coord_equal() +
            geom_point(data=z.df,
                       aes(x=x,
                           y=y,
                           color=cluster),
                       size=3) +
            stat_ellipse() +
            scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
            labs(y = "Wetland ecosystem service axis (z)",
                 x = "Risk axis (z)",
                 color = "Cluster", fill = "Density") +
            theme(axis.title.x = element_markdown(),
                  axis.title.y = element_markdown(),
                  text = element_text(size=16),
                  legend.text = element_text(size=16),
                  legend.title = element_text(size=18))
p.copula
ggsave("Manuscript/Main_Figures/Figure6_Copula.jpeg", 
       p.copula, width = 12, height = 10, dpi = 1000)

