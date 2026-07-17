setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/Carbon_Storage_Estimation/NWCA_Model")

library(car)
library(MASS)
library(cv)
library(tidyverse)

# read in NWCA points with raster attributes
tc.df = read.csv("Step8_IL_NWCA_TC_Points_Attributes.csv")

# simplify tc.df
raster.vars = c("MAP","MAT","Elevation","Slope","Aspect","TWI","NDWI","NDVI")
n.r = length(raster.vars)
tc.df = tc.df[,c("TC","DEPTH",raster.vars)]

# remove organic soil wetlands
tc.df = tc.df[-which(tc.df$TC >= 12),]

# scale input dataframe
sd.vars = apply(tc.df[,raster.vars], 2, FUN=sd)
mean.vars = apply(tc.df[,raster.vars], 2, FUN=mean)
ranges = apply(tc.df[,raster.vars], 2, FUN=range)
for (i in 1:n.r) {
  tc.df[,raster.vars[i]] = (tc.df[,raster.vars[i]] - mean.vars[i])/sd.vars[i]
}
ranges = apply(tc.df[,raster.vars], 2, FUN=range)

 
# make linear model
tc.df$DEPTH = tc.df$DEPTH/100
tc.lm = lm(formula = log(TC) ~ DEPTH*(MAP+MAT+Elevation+Slope+Aspect+TWI+NDWI+NDVI), data=tc.df)

# stepwise selection with BIC
n = nrow(tc.df)
bic.results = stepAIC(tc.lm, direction="both", k = log(2),
                      scope = list(lower = log(TC) ~ 1,
                                   upper = log(TC) ~ DEPTH*(MAP+MAT+Elevation+Slope+Aspect+TWI+NDWI+NDVI)))

# obtain best model
best.bic.model = formula(bic.results)
summary(lm(best.bic.model, data=tc.df))

# write coefficients to file
n.coeffs = length(coefficients(bic.results))
model.df = data.frame(labels = names(coefficients(bic.results)),
                      values = as.numeric(coefficients(bic.results)))
write.csv(model.df, "Best_TC_Concentration_Model.csv", row.names=F)

# write means and sds to a file for scaling
scale.df = data.frame(name = raster.vars,
                      sd = sd.vars,
                      mean = mean.vars)
write.csv(scale.df, "Model_Raster_Statistics.csv", row.names=F)

# test
#depths = seq(0,100)
#maps = seq(min(tc.df$MAP),max(tc.df$MAP))
#mats = seq(min(tc.df$MAT),max(tc.df$MAT))
#plot.df = data.frame(matrix(nrow=0,ncol=4))
#colnames(plot.df) = c("depth","map","mat","tc")
#for (i in 1:length(maps)) {
#  plot.df.i = data.frame(matrix(nrow=length(depths), ncol=3))
#  colnames(plot.df.i) = c("depth","map","mat","tc")
#  plot.df.i$depth = depths
#  plot.df.i$map = maps[i]
#  plot.df.i$tc = exp(model.df[1,"values"] + model.df[2,"values"]*(depths/100) + model.df[2,"values"]*maps[i])
#  plot.df = rbind(plot.df, plot.df.i)
#}
#ggplot(plot.df, 
#       aes(y=depth,
#           x=tc,
#           group=map,
#           color=map)) + 
#       geom_line() + 
#       scale_y_reverse()
