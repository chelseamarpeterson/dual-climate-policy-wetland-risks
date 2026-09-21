setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_Dual_Wetland_Risk/dual-risk-repo/Ecosystem_Services_Estimation/Herpetofauna_Species_Richness")

# load unstructured dataframe
raw.df = read.csv("county_herp_list_spaces.csv",header=F)
n_in = nrow(raw.df)

# re-arrange dataframe
new.df = data.frame(matrix(nrow=n_in/2,ncol=0))
new.df$sci.name = raw.df[seq(1,n_in,2),1]
new.df$common.name = raw.df[seq(2,n_in+1,2),1]