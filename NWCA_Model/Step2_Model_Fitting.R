setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/NWCA_Model")

library(car)
library(MASS)
library(cv)

# read in NWCA points with raster attributes
tc.df = read.csv("Step8_IL_NWCA_TC_Points_Attributes.csv")

# simplify tc.df
tc.df = tc.df[,c("TC","DEPTH","MAP","MAT","DEM","NDVI","TWI","NDWI","Slope","Aspect")]

# make linear model
tc.lm = lm(formula = log(TC) ~ (DEPTH+MAP+MAT+DEM+NDVI+NDWI+Slope+Aspect)^2, data=tc.df)
#+SOC_Stock+BiomassC_Stock
# stepwise selection with BIC
n = nrow(tc.df)
bic.results = stepAIC(tc.lm, direction="both", k = log(n),
                      scope = list(lower = log(TC) ~ 1,
                                   upper = log(TC) ~ (DEPTH+MAP+MAT+DEM+NDVI+NDWI+Slope+Aspect)^2))

# obtain best model
best.bic.model = formula(bic.results)
summary(lm(best.bic.model, data=tc.df))

# write coefficients to file
n.coeffs = length(coefficients(bic.results))
model.df = data.frame(labels = names(coefficients(bic.results)),
                      values = as.numeric(coefficients(bic.results)))
write.csv(model.df, "Best_TC_Concentration_Model.csv", row.names=F)
