setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Chapter5/County_Species_Data")

library(readxl)
library(dplyr)

# read in data files
cc.df = read_excel("Model_masterlist.xlsx")
counties = read.csv("IL_County_records_new.csv")
x = counties[which(counties$County == "Champaign"),]

# subset master list for IL
cc.df.IL = subset(cc.df[,c("Spp.","WV","IL_CC")])
cc.df.IL = cc.df.IL[-which(cc.df.IL$IL_CC == "NA"),]

# isolate wetland species
cc.df.IL.wet = subset(cc.df.IL, cc.df.IL$WV=="FAC" | cc.df.IL$WV=="FACW" | cc.df.IL$WV=="OBL")

# merge species and county dta
d.IL.wet.cnty = merge(counties, cc.df.IL.wet, by="Spp.")

# check for NAs
sum(is.na(d.IL.wet.cnty$WV))
sum(is.na(d.IL.wet.cnty$IL_CC))

# identify unique species 
county.spp = data.frame(unique(counties$Spp.))
names(county.spp)[1] = "Spp."

# mismatches (fix these manually)
potential.mismatches = anti_join(cc.df.IL.wet, county.spp, by="Spp.")
write.csv(potential.mismatches, "taxonomy.mismatches.raw.csv", row.names=F)

# read in taxonomy mismatches with synonyms and add to county database
tax.syns.df = read.csv("taxonomy.mismatches.synonyms.csv")
d.IL.wet.cnty.syn.dfs = list()
d.IL.wet.cnty.join = d.IL.wet.cnty
for (i in 1:16) {
  syn.i = paste("Synonym",i,sep=".")
  counties[,syn.i] = counties$Spp.
  syn.df.i = merge(counties[,c("Spp.","Junk","County",syn.i)], 
                             tax.syns.df[,c(seq(1,3),3+i)], by=syn.i)
  d.IL.wet.cnty.syn.dfs[[as.character(i)]] = syn.df.i
  syn.df.i.trim = syn.df.i[,c("Spp..x","Junk","County","WV","IL_CC")]
  colnames(syn.df.i.trim)[1] = "Spp."
  d.IL.wet.cnty.join = rbind(d.IL.wet.cnty.join, syn.df.i.trim)
}
sort(unique(d.IL.wet.cnty.join$WV))

# check that there are no repeat species within each county
df.IL.wet.cnty.sp.count = data.frame(matrix(nrow=0,ncol=3))
colnames(df.IL.wet.cnty.sp.count) = c("County","Total.species","Unique.species")
county.names = sort(unique(d.IL.wet.cnty$County))
n.c = length(county.names)
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.sp.all = counties[which(counties$County == cnty.i),"Spp."]
  df.IL.wet.cnty.sp.count[i,"Total.species"] = length(cnty.sp.all)
  df.IL.wet.cnty.sp.count[i,"Unique.species"] = length(unique(cnty.sp.all))
}

# remove repeat species within each county
df.IL.wet.cnty.norep = data.frame(matrix(nrow=0,ncol=5))
colnames(df.IL.wet.cnty.norep) = c("Spp.","Junk","County","WV","IL_CC")
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.df.i = d.IL.wet.cnty.join[which(d.IL.wet.cnty.join$County == cnty.i),]
  cnty.spp.i = cnty.df.i[,"Spp."]
  cnty.uni.spp.i = sort(unique(cnty.spp.i))
  n.uni.spp.i = length(cnty.uni.spp.i)
  for (j in 1:n.uni.spp.i) {
    spp.ij = cnty.uni.spp.i[j]
    df.cnty.sp.ij = cnty.df.i[which(cnty.df.i$Spp. == spp.ij)[1],]
    df.IL.wet.cnty.norep = rbind(df.IL.wet.cnty.norep, df.cnty.sp.ij)
  }
}

# estimate county mean C
df.IL.wet.cnty.norep$IL_CC = as.numeric(df.IL.wet.cnty.norep$IL_CC)
county.means = df.IL.wet.cnty.norep %>%
               group_by(County) %>%
               summarize(mean.c = mean(IL_CC, na.rm=TRUE))

# estimate FQI and species richness
county.means$n = rep(0, nrow(county.means))
county.means$fqi = rep(0, nrow(county.means))
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.df.i = df.IL.wet.cnty.norep[which(df.IL.wet.cnty.norep$County == cnty.i),]
  cnty.spp.i = cnty.df.i[,"Spp."]
  cnty.uni.spp.i = sort(unique(cnty.spp.i))
  n.uni.spp.i = length(cnty.uni.spp.i)
  county.means[i,"n"] = n.uni.spp.i
  county.means[i,"fqi"] = county.means[i,"mean.c"] * sqrt(n.uni.spp.i)
}
colnames(county.means) = tolower(colnames(county.means))
county.means[which(county.means$county == "DeWitt"),"county"] = "De Witt"

# read in unprotected wetland area sums
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/JEMA-Paper-Results/JEMA-Paper-Repo/County_Statistics")
wetland.area.df = read.csv("IL_WS_Step13_County_Stats.csv")
wetland.sf.df = wetland.area.df[,c("county","mean_SF")]
colnames(wetland.sf.df) = c("county","unpro.wet.area")

# join wetland area sums to plant data
plant.wetland.df = left_join(county.means, wetland.sf.df, by ="county")

# multiply wetland area by each statistic
plant.wetland.df$mean.c.area = plant.wetland.df$mean.c * plant.wetland.df$unpro.wet.area/100
plant.wetland.df$n.area = plant.wetland.df$n * plant.wetland.df$unpro.wet.area/100
plant.wetland.df$fqi.area = plant.wetland.df$fqi * plant.wetland.df$unpro.wet.area/100

# save
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Chapter5/County_Species_Data")
write.csv(plant.wetland.df, "County_Plant_Diversity_Summary.csv",row.names=F)
