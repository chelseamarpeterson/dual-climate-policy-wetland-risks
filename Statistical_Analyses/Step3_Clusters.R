

################################################################################
# run simple stats to compare ecosystem services, unprotected wetland areas and
# percents, and climate extremes across clusters



# sum unscaled data by cluster
#unscaled.sum = unscaled_df %>%
#               group_by(CLUSTER_NAME) %>%
#               summarize(total_crops = sum(area_crop),
#                         total_urban = sum(area_urban),
#                         total_unpro = sum(area_spf))



################################################################################
# run PCA and plot clusters as colors

pca_result = prcomp(cluster_df[,pca_cols], center = T, scale. = T)

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
p_biplot = fviz_pca_biplot(pca_result, 
                label="var", 
                habillage=cluster_df$CLUSTER_NAME,
                axes = c(1, 2),
                addEllipses=TRUE,
                mean.point=FALSE,
                repel=TRUE,
                labelsize = 6,
                pointsize = 3) +
                scale_fill_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
                scale_shape_manual(values = seq(15,18)) +
                scale_size_manual(values = c(3,3,3,4)) +
                labs(title = NULL, 
                     x="PC1 (29.8%)",
                     y="PC2 (24.1%)",
                     color="Cluster",
                     shape="Cluster",
                     fill="Cluster",
                     size="Cluster") + 
                #scale_y_continuous(limits=c(-4,8),
                #                   breaks=seq(-4,8,by=2)) +
                theme(axis.title.x = element_markdown(),
                      axis.title.y = element_markdown(),
                      text = element_text(size=18),
                      legend.text = element_text(size=18),
                      legend.title = element_text(size=24))
p_biplot
#ggsave("Manuscript/Main_Figures/Figure6_PCA.jpeg", 
#       p_biplot, width = 14, height = 12, dpi = 800)

# write PCs to file

################################################################################
# bivariate copula




# apply rank transformation
x = pca_risks$x[,"PC1"]
y = pca_services$x[,"PC1"]
n = length(x)
u = rank(x) / (n + 1)
v = rank(y) / (n + 1)

# apply inverse CDF to get z scores
z.df = data.frame(x=qnorm(u),
                  y=qnorm(v), 
                  cluster=cluster.df$CLUSTER_NAME,
                  county=cluster.df$county)

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
                           color=cluster,
                           shape=cluster,
                           size=cluster)) +
            stat_ellipse() +
            scale_color_manual(values = c("gray20","gray60","salmon2","red4")) +
            scale_shape_manual(values = seq(15,18)) +
            scale_size_manual(values = c(3, 3, 3, 4)) +
            labs(y = "Wetland ecosystem service axis (z)",
                 x = "Risk axis (z)",
                 color = "Cluster", 
                 shape = "Cluster",
                 size = "Cluster",
                 fill = "Density") +
            theme(axis.title.x = element_markdown(),
                  axis.title.y = element_markdown(),
                  text = element_text(size=16),
                  legend.text = element_text(size=16),
                  legend.title = element_text(size=18))
p.copula
ggsave("Manuscript/Main_Figures/Figure6_Copula.jpeg", 
       p.copula, width = 12, height = 10, dpi = 1000)




################################################################################
# make correlation matrix for supplement

library(ggcorrplot)

p_corr = ggcorrplot(
  cor(cluster_df[,pca_cols]),
  lab = TRUE,
  type = "lower",
  show.diag = FALSE) +
  scale_fill_gradient2(
    name = "Pearson\ncorrelation\n",
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Manuscript/Supp_Figures/AppendixD/FigureD1_Service_Risk_CorrelatonPlot.jpeg", 
       p_corr, width = 10, height = 10, dpi = 800)

