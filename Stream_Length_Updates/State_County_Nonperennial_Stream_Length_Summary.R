setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch6_CASC_Project/Dual-Risk-Repo/Stream_Length_Updates")

library(ggplot2)

# read in county totals
county_df = read.csv("NHDFlowline_Step12_CountyJoinSums_Brinkerhoff.csv")
n_c = nrow(county_df)

# versions
versions = c("NHD","BRF")
n_v = length(versions)

# flow permanence categories
flow_cats = c("Perennial","Intermittent","Ephemeral","Nonperennial")
n_f = length(flow_cats)

# make new dataframe that for plotting box plots of versions x flow permanenece
plot_df = data.frame(matrix(nrow=0, ncol=4))
colnames(plot_df) = c("County","Dataset","Flow permanence","Length")
for (i in 1:n_v) {
  for (j in 1:n_f) {
    vf_df = data.frame(matrix(nrow=0, ncol=4))
    colnames(vf_df) = c("County","Dataset","Flow permanence","Length")
    for (k in 1:n_c) {
      vf_col = paste("SUM",versions[i],flow_cats[j],"Km", sep="_")
      vf_df[k,"County"] = county_df[k,"NAME"]
      vf_df[k,"Dataset"] = versions[i]
      vf_df[k,"Flow permanence"] = flow_cats[j]
      vf_df[k,"Length"] = county_df[k,vf_col]
    }
    plot_df = rbind(plot_df, vf_df)
  }
}

# histogram of all counities
ggplot(plot_df, 
       aes(x=Length,
           y=`Flow permanence`,
           fill=Dataset)) + 
       geom_boxplot()
