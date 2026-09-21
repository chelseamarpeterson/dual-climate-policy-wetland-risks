setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project")

library(VineCopula)

################################################################################
# fit copulas between unprotecgted wetland areas/percents and climate extremes

# read in cluster results
cluster.df = read.csv("dual-risk-repo/County_Summaries/County_Ecosystem_Services_Unprotected_Areas_Climate_Extremes_SSP245_2041_2070_Clusters.csv")
colnames(cluster.df)[15:16] = c("county","region")

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
x.df = cluster.eco.un.clim.join

# apply rank transformation to full dataframe: U ~ U(0,1)
var.cols = colnames(x.df)[5:14]
n.v = length(var.cols)
u.df = x.df
n = nrow(x.df)
for (i in 1:n.v) {
  u.df[,var.cols[i]] = rank(x.df[,var.cols[i]]) / (n + 1)
}

# copulas to test
cop.names = c("Gaussian","t","Gumbel")
cop.nums = c(1,2,4)
all.cop.nums = list("1" = c(1),
                    "2" = c(2),
                    "3" = c(4,14,24,34))
n.c = length(cop.names)

# fit optimal copula model for unprotected wetland area v. each climate extreme
un.area.ex.fits = list()
v = u.df[,"area_spf"]
un.area.ex.nums = c()
un.area.ex.taus = c()
un.area.ex.fams = c()
un.area.ex.up.tails = c()
un.area.ex.lo.tails = c()
for (i in 1:n.ex) {
  ex.i = climate.vars.order[i]
  u = u.df[,ex.i]
  un.area.ex.fits[[ex.i]] = list()
  
  # fit each copula type
  for (j in 1:n.c) {
    un.area.ex.fits[[ex.i]][[cop.names[j]]] = BiCopSelect(u, v, familyset=cop.nums[j])
  }
  
  # compare copulas with AIC
  ex.fits = un.area.ex.fits[[ex.i]]
  aic.df = sapply(list(ex.fits[["Gaussian"]],ex.fits[["t"]], ex.fits[["Gumbel"]]),
                  function(f) c(family = f$family, AIC = f$AIC))
  
  # save best copula number in matrix
  best.cop.number = as.integer(aic.df[1,which.min(aic.df[2,])])
  un.area.ex.nums = c(un.area.ex.nums, best.cop.number)
  un.area.ex.taus = c(un.area.ex.taus, BiCopSelect(u, v)$tau)
  un.area.ex.up.tails = c(un.area.ex.up.tails, BiCopPar2TailDep(BiCopSelect(u, v))$upper)
  un.area.ex.lo.tails = c(un.area.ex.lo.tails, BiCopPar2TailDep(BiCopSelect(u, v))$lower)
  
  for (j in 1:n.c) {
    potential.cop.nums = all.cop.nums[[j]]
    if (best.cop.number %in% potential.cop.nums) { best.cop.family = cop.names[j] }
  }
  un.area.ex.fams = c(un.area.ex.fams, best.cop.family)
}
un.area.ex.nums
un.area.ex.taus
un.area.ex.up.tails
un.area.ex.lo.tails
un.area.ex.fams

un_area_best_bicop_model_list = list()
un_area_melted_density_all = data.frame(matrix(nrow=0, ncol=4))
colnames(un_area_melted_density_all) = c("x","y","value","extreme")
grid_seq = seq(-2.5, 2.5, length.out=100)
for (i in 1:n.ex) {
  # get model with least AIC/BIC
  ex.i = climate.vars.order[i]
  un_area_best_bicop_model_list[[ex.i]] = un.area.ex.fits[[ex.i]][[un.area.ex.fams[i]]]
  
  # create grid sequence
  density_matrix = outer(grid_seq, grid_seq, 
                         function(x, y) {BiCopPDF(pnorm(x), pnorm(y), un_area_best_bicop_model_list[[ex.i]]) * dnorm(x) * dnorm(y)})
  dimnames(density_matrix) = list(x = grid_seq, y = grid_seq)
  
  # melt dataframe for plotting
  melted_density = melt(density_matrix, varnames = c("x", "y"))
  melted_density$extreme = ex.i
  
  # upend to large matrix
  un_area_melted_density_all = rbind(un_area_melted_density_all, melted_density)
}

ggplot() +
  metR::geom_contour_fill(data=subset(un_area_melted_density_all, extreme == climate.vars.order[1]), 
                          aes(x = x, y = y, z = value),
                          breaks=seq(0,0.3,length.out=10)) +
  scale_fill_distiller(palette = "Greens",
                       direction = 2,
                       limits=c(0,0.31)) +
  coord_equal() +
  labs(x = "Unprotected wetland area (z)",
       y = "Reduction in frost days (z)",
       color = "Density", fill = "Density") +
  facet_wrap(.~extreme)


ggplot() +
  metR::geom_contour_fill(data=subset(un_area_melted_density_all, extreme == climate.vars.order[2]), 
                          aes(x = x, y = y, z = value),
                          breaks=seq(0,0.3,length.out=10)) +
  scale_fill_distiller(palette = "Greens",
                       direction = 2,
                       limits=c(0,0.31)) +
  coord_equal() +
  labs(x = "Unprotected wetland area (z)",
       y = "Reduction in frost days (z)",
       color = "Density", fill = "Density") +
  facet_wrap(.~extreme)

ggplot() +
  metR::geom_contour_fill(data=subset(un_area_melted_density_all, extreme == climate.vars.order[3]), 
                          aes(x = x, y = y, z = value),
                          breaks=seq(0,0.3,length.out=10)) +
  scale_fill_distiller(palette = "Greens",
                       direction = 2,
                       limits=c(0,0.31)) +
  coord_equal() +
  labs(x = "Unprotected wetland area (z)",
       y = "Reduction in frost days (z)",
       color = "Density", fill = "Density") +
  facet_wrap(.~extreme)

ggplot() +
  metR::geom_contour_fill(data=subset(un_area_melted_density_all, extreme == climate.vars.order[4]), 
                          aes(x = x, y = y, z = value),
                          breaks=seq(0,0.3,length.out=10)) +
  scale_fill_distiller(palette = "Greens",
                       direction = 2,
                       limits=c(0,0.31)) +
  coord_equal() +
  labs(x = "Unprotected wetland area (z)",
       y = "Reduction in frost days (z)",
       color = "Density", fill = "Density") +
  facet_wrap(.~extreme)
