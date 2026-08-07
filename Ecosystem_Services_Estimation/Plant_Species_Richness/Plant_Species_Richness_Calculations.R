setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project/dual-risk-repo/Ecosystem_Services_Estimation/Plant_Species_Richness")

library(readxl)
library(dplyr)

# read in list of all unique species in Illinois with CC values
cc.df = read_excel("Model_masterlist.xlsx") 

# read in list of Illinois plant species by county
counties = read.csv("IL_County_records_new.csv")

# subset master list for IL
cc.df.IL = subset(cc.df[,c("Spp.","WV","IL_CC")])

# remove species that are not in illinois
cc.df.IL = subset(cc.df.IL, IL_CC != "NA")

# isolate wetland species
cc.df.IL.wet = subset(cc.df.IL, WV=="FACW" | WV=="OBL")

# identify unique species 
county.spp = data.frame(unique(counties$Spp.))
names(county.spp)[1] = "Spp."

# mismatches (fix these manually)
#potential.mismatches = anti_join(cc.df.IL.wet, county.spp, by="Spp.")
#write.csv(potential.mismatches, "taxonomy.mismatches.raw.csv", row.names=F)

# read in taxonomy mismatches with synonyms
tax.syns.df = read.csv("taxonomy.mismatches.synonyms.without.fac.csv")
n.syns = nrow(tax.syns.df)

# add synonym column to the Illinois list
cc.df.IL.wet$Synonym = cc.df.IL.wet$Spp.
counties$Synonym = counties$Spp.
for (i in 1:n.syns) {
  spp.i = tax.syns.df[i,"Spp."]
  syn.i = tax.syns.df[i,"Synonym"]
  cc.df.IL.wet[cc.df.IL.wet$Spp. == spp.i,"Synonym"] = syn.i
  counties[counties$Spp. == spp.i,"Synonym"] = syn.i
}

# join CC data to county database
county.cc.join.df = inner_join(counties, cc.df.IL.wet, by="Synonym")

# check for NAs
sum(is.na(county.cc.join.df$WV))
sum(is.na(county.cc.join.df$IL_CC))

# check that there are no repeat species within each county
df.IL.wet.cnty.sp.count = data.frame(matrix(nrow=0,ncol=3))
colnames(df.IL.wet.cnty.sp.count) = c("County","Total.species","Unique.species")
county.names = sort(unique(county.cc.join.df$County))
county.df = df.IL.wet.cnty.norep
#county.df = county.cc.join.df
n.c = length(county.names)
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.sp.all = county.df[county.df$County == cnty.i,"Synonym"]
  df.IL.wet.cnty.sp.count[i,"County"] = cnty.i
  df.IL.wet.cnty.sp.count[i,"Total.species"] = length(cnty.sp.all)
  df.IL.wet.cnty.sp.count[i,"Unique.species"] = length(unique(cnty.sp.all))
}
sum(df.IL.wet.cnty.sp.count$Total.species != df.IL.wet.cnty.sp.count$Unique.species)
    
# remove repeat species within each county
df.IL.wet.cnty.norep = data.frame(matrix(nrow=0,ncol=7))
colnames(df.IL.wet.cnty.norep) = colnames(county.cc.join.df)
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.df.i = county.cc.join.df[county.cc.join.df$County == cnty.i,]
  cnty.uni.spp.i = sort(unique(cnty.df.i[,"Synonym"]))
  n.uni.spp.i = length(cnty.uni.spp.i)
  for (j in 1:n.uni.spp.i) {
    spp.ij = cnty.uni.spp.i[j]
    df.cnty.sp.ij = cnty.df.i[which(cnty.df.i$Synonym == spp.ij)[1],]
    df.IL.wet.cnty.norep = rbind(df.IL.wet.cnty.norep, df.cnty.sp.ij)
  }
}

# estimate county mean C
df.IL.wet.cnty.norep$IL_CC = as.numeric(df.IL.wet.cnty.norep$IL_CC)
county.means = df.IL.wet.cnty.norep %>%
               group_by(County) %>%
               summarize(mean.c = mean(IL_CC, na.rm=TRUE))

# estimate FQI and species richness
county.means$plantN = 0
county.means$fqi = 0
for (i in 1:n.c) {
  cnty.i = county.names[i]
  cnty.df.i = df.IL.wet.cnty.norep[df.IL.wet.cnty.norep$County == cnty.i,]
  cnty.uni.spp.i = sort(unique(cnty.df.i$Synonym))
  n.uni.spp.i = length(cnty.uni.spp.i)
  county.means[i,"plantN"] = n.uni.spp.i
  county.means[i,"fqi"] = county.means[i,"mean.c"] * sqrt(n.uni.spp.i)
}
colnames(county.means) = tolower(colnames(county.means))
county.means[county.means$county == "DeWitt","county"] = "De Witt"

# save
write.csv(county.means, "County_Wetland_Dependent_Plant_Species_Richness.csv",row.names=F)
