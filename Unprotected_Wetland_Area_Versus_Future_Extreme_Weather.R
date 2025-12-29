setwd("F:/Climate_Data/USGS_Thresholds_WeightedMMM/County")

library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(glue)

## load and prepare data

# read in county wetland statistics
wet.df = read.csv("County_Wetland_Stats.csv")
colnames(wet.df)[1] = "NAME"

# read in county areas
area.df = read.csv("County_Areas.csv")

# join wetland and county area data
wet.cnty.df = left_join(area.df, wet.df, by="NAME", keep=F)

# normalize wetland areas by county areas
wet.cols = colnames(wet.df)[2:25]
n.wc = length(wet.cols)
for (i in 1:n.wc) {
  wet.cnty.df[,wet.cols[i]] = wet.cnty.df[,wet.cols[i]] / wet.cnty.df$County_Area_Ha*1000
}

# ssps
scenarios = c("ssp245","ssp370","ssp585")
n.s = length(scenarios)

# time ranges
ranges = c("2041_2070","2071_2100","2041_2100")
range.labels = c("2041-2070","2071-2100","2041-2100")
n.r = length(ranges)

# load data for all scenarios and time ranges
all.precip.df = data.frame(matrix(nrow=0, ncol=6))
all.temp.df = data.frame(matrix(nrow=0, ncol=6))
colnames(all.precip.df) = c("NAME","max_SPF","variable","value","ssp","range")
colnames(all.temp.df) = c("NAME","max_SPF","variable","value","ssp","range")
for (s in scenarios) {
  for (i in 1:n.r) {
    # read in extreme weather statistics
    r = ranges[i]
    ext.df = read.csv(glue("Thresholds_WMMM_{s}_{r}_Hist_Mean_Diff_County.csv"))
    
    # join extreme weather statistics with wetland area data
    wet.fut.df = left_join(wet.cnty.df, ext.df, by="NAME", keep=F)
    
    # precipitation variables
    precip.vars = c("CDD","CWD","PRCPTOT","R10mm","R1in","R1mm","R20mm","R2in",
                    "R3in","R40mm","R95p","R95pDAYS","R95pTOT","R99p","R99pDAYS",
                    "R99pTOT","Rx1day","Rx5day","SDII")
    n.pv = length(precip.vars)
    wet.precip.melt = melt(wet.fut.df[,c("NAME","max_SPF",precip.vars)], 
                           id.vars=c("NAME","max_SPF"))
    wet.precip.melt$ssp = s
    wet.precip.melt$range = range.labels[i]
    all.precip.df = rbind(all.precip.df, wet.precip.melt)
    
    # temperature variables
    temp.vars = c("DTR","SU","TR","TX90p","TX90pDAYS","TX95p","TX95pDAYS",
                  "TXge100F","TXge105F","TXge110F","TXge90F","TXge95F")
    n.tv = length(temp.vars)
    wet.temp.melt = melt(wet.fut.df[,c("NAME","max_SPF",temp.vars)], 
                         id.vars=c("NAME","max_SPF"))
    wet.temp.melt$ssp = s
    wet.temp.melt$range = range.labels[i]
    all.temp.df = rbind(all.temp.df, wet.temp.melt)
  }                    
}

# fit linear model for each scenario and variable
precip.lm.df = data.frame(matrix(nrow=n.s*n.pv,ncol=8))
temp.lm.df = data.frame(matrix(nrow=n.s*n.tv,ncol=8))
colnames(precip.lm.df) = c("ssp","range","variable","R2","effect","lower","upper","p")
colnames(temp.lm.df) = c("ssp","range","variable","R2","effect","lower","upper","p")
k = 1
x = 1
for (s in scenarios) {
  for (i in 1:n.r) {
    r = range.labels[i]
    for (pv in precip.vars) {
      sp.df = subset(all.precip.df, ssp==s & range == r & variable==pv)
      sp.lm = lm(value ~ max_SPF, data=sp.df)
      sp.lm.sum = summary(sp.lm)
      sp.lm.ci = confint(sp.lm, level = 0.90)
      precip.lm.df[k,"ssp"] = s
      precip.lm.df[k,"range"] = r
      precip.lm.df[k,"variable"] = pv
      precip.lm.df[k,"R2"] = sp.lm.sum$r.squared
      precip.lm.df[k,"effect"] = sp.lm.sum$coefficients["max_SPF","Estimate"]
      precip.lm.df[k,"lower"] = sp.lm.ci["max_SPF","5 %"]
      precip.lm.df[k,"upper"] = sp.lm.ci["max_SPF","95 %"]
      precip.lm.df[k,"p"] = sp.lm.sum$coefficients["max_SPF","Pr(>|t|)"]
      k = k + 1
    }
    
    for (tv in temp.vars) {
      st.df = subset(all.temp.df, ssp==s & range == r & variable==tv)
      st.lm = lm(value ~ max_SPF, data=st.df)
      st.lm.sum = summary(st.lm)
      st.lm.ci = confint(st.lm, level = 0.90)
      temp.lm.df[x,"ssp"] = s
      temp.lm.df[x,"range"] = r
      temp.lm.df[x,"variable"] = tv
      temp.lm.df[x,"R2"] = st.lm.sum$r.squared
      temp.lm.df[x,"effect"] = st.lm.sum$coefficients["max_SPF","Estimate"]
      temp.lm.df[x,"lower"] = st.lm.ci["max_SPF","5 %"]
      temp.lm.df[x,"upper"] = st.lm.ci["max_SPF","95 %"]
      temp.lm.df[x,"p"] = st.lm.sum$coefficients["max_SPF","Pr(>|t|)"]
      x = x + 1
    }
  }
}

# plot intervals for precipitation
ggplot(precip.lm.df, aes(x=effect, y=ssp, 
                         color=factor(range, levels=range.labels))) + 
       geom_point(position=position_dodge(0.5)) +
       geom_errorbar(aes(xmin=lower, xmax=upper, y=ssp, 
                         color=factor(range, levels=range.labels)),
                     width=0.5,position=position_dodge(0.5)) +
       geom_vline(xintercept=0,linetype="dashed") +
       labs(color="Future year interval",
            x="Effect size (Change in climate extreme v. unprotected wetland area)", 
            y="Emissions scenario") + 
       facet_wrap(.~variable, scales="free_x")

# plot intervals for temperature
ggplot(temp.lm.df, aes(x=effect, y=ssp, 
                       color=factor(range, levels=range.labels))) +  
        geom_point(position=position_dodge(0.5)) +
        geom_errorbar(aes(xmin=lower,xmax=upper, y=ssp, 
                          color=factor(range, levels=range.labels)),
                      width=0.5,position=position_dodge(0.5)) +
        geom_vline(xintercept=0,linetype="dashed") +
        labs(color="Future year interval",
             x="Effect size (Change in climate extreme v. unprotected wetland area)", 
             y="Emissions scenario") + 
        facet_wrap(.~variable, scales="free_x")





