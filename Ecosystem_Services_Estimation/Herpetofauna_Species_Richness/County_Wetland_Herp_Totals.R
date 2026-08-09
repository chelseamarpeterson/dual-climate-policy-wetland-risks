setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Ch5_CASC_Project/dual-risk-repo/Ecosystem_Services_Estimation/Herpetofauna_Species_Richness")

library(dplyr)

# read in county-level species lists
il.cnty.lists = read.csv("Illinois_Herp_Species_By_County.csv")
il.cnty.lists$INHS.scientific.name = iconv(il.cnty.lists$INHS.scientific.name, to = "UTF-8", sub = " ") 
il.cnty.lists$Updated.scientific.name = iconv(il.cnty.lists$Updated.scientific.name, to = "UTF-8", sub = " ") 
il.cnty.lists$INHS.scientific.name = trimws(il.cnty.lists$INHS.scientific.name)
il.cnty.lists$Updated.scientific.name = trimws(il.cnty.lists$Updated.scientific.name)

# identify species that aren't the same between INHS and IUCN
not.same.ind = which(il.cnty.lists$INHS.scientific.name != il.cnty.lists$Updated.scientific.name)
common.names = unique(il.cnty.lists[not.same.ind,"Common.name"])
for (i in 1:length(common.names)) {
  ind = which(il.cnty.lists$Common.name == common.names[i])
  print(il.cnty.lists[ind,"INHS.scientific.name"][1])
  print(il.cnty.lists[ind,"Updated.scientific.name"][1])
  print(il.cnty.lists[ind,"Common.name"][1])
}

# write file of unique Illinois wetland species
df.uni.sp = data.frame(unique.species = sort(unique(il.cnty.lists$Updated.scientific.name)))
write.csv(df.uni.sp, "Illinois_Unique_Herp_Species.csv", row.names=F)

# read in list of all species in Illinois
il.herp.df = read.csv("Illinois_Herp_Species_Wetland_Requirements.csv")
colnames(il.herp.df)[5] = "Threatened.or.endangered"
colnames(il.herp.df)[18:19] = c("Wetland.dependent","Wetland.dependent.threatened.or.endangered")

# Trim white space on scientific name
il.herp.df$Scientific.name = trimws(il.herp.df$Scientific.name)

# Add yes/no columns for groups
il.herp.df$wet.frog.toad = (il.herp.df$Category == "Frogs and toads" & il.herp.df$Wetland.dependent == 1)
il.herp.df$wet.salamander = (il.herp.df$Category == "Salamanders" & il.herp.df$Wetland.dependent == 1)
il.herp.df$wet.turtle = (il.herp.df$Category == "Turtles" & il.herp.df$Wetland.dependent == 1)
il.herp.df$wet.liz.snake = (il.herp.df$Category == "Lizards and snakes" & il.herp.df$Wetland.dependent == 1)

il.herp.df$wet.frog.toad.te = (il.herp.df$wet.frog.toad == 1 & il.herp.df$Threatened.or.endangered == 1)
il.herp.df$wet.salamander.te = (il.herp.df$wet.salamander == 1 & il.herp.df$Threatened.or.endangered == 1)
il.herp.df$wet.turtle.te = (il.herp.df$wet.turtle == 1 & il.herp.df$Threatened.or.endangered == 1)
il.herp.df$wet.liz.snake.te = (il.herp.df$wet.liz.snake == 1 & il.herp.df$Threatened.or.endangered == 1)

#### Join county-level lists with wetland dependence column
colnames(il.cnty.lists)[3] = "Scientific.name"
il.cnty.lists.join.wetland = left_join(il.cnty.lists,
                                       il.herp.df[,c("Scientific.name","Wetland.dependent",
                                                     "Wetland.dependent.threatened.or.endangered",
                                                     "wet.frog.toad","wet.salamander",
                                                     "wet.turtle","wet.liz.snake",
                                                     "wet.frog.toad.te","wet.salamander.te",
                                                     "wet.turtle.te","wet.liz.snake.te")],
                                       by="Scientific.name")
sum(is.na(il.cnty.lists.join.wetland$Wetland.dependent))
sum(is.na(il.cnty.lists.join.wetland$Threatened.or.endangered))
sum(is.na(il.cnty.lists.join.wetland$Wetland.dependent.threatened.or.endangered))

#### Sum total wetland-dependent species by county
il.cnty.sums = il.cnty.lists.join.wetland %>%
               group_by(County) %>%
               summarize(Tot_wetland_dependent = sum(Wetland.dependent),
                         Tot_wetland_threatened_endangered = sum(Wetland.dependent.threatened.or.endangered),
                         Wetland_frog_toad = sum(wet.frog.toad),
                         Wetland_salamander = sum(wet.salamander),
                         Wetland_turtle = sum(wet.turtle),
                         Wetland_liz_snake = sum(wet.liz.snake),
                         Wetland_amphibian = sum(wet.frog.toad) + sum(wet.salamander),
                         Wetland_reptile = sum(wet.turtle) + sum(wet.liz.snake),
                         Wetland_frog_toad_te = sum(wet.frog.toad.te),
                         Wetland_salamander_te = sum(wet.salamander.te),
                         Wetland_turtle_te = sum(wet.turtle.te),
                         Wetland_liz_snake_te = sum(wet.liz.snake.te),
                         Wetland_amphibian_te = sum(wet.frog.toad.te) + sum(wet.salamander.te),
                         Wetland_reptile_te = sum(wet.turtle.te) + sum(wet.liz.snake.te))
sum(il.cnty.sums$Tot_wetland_dependent == il.cnty.sums$Wetland_frog_toad + il.cnty.sums$Wetland_salamander + il.cnty.sums$Wetland_turtle + il.cnty.sums$Wetland_liz_snake)
sum(il.cnty.sums$Tot_wetland_dependent == il.cnty.sums$Wetland_amphibian + il.cnty.sums$Wetland_reptile)
sum(il.cnty.sums$Tot_wetland_threatened_endangered == il.cnty.sums$Wetland_frog_toad_te + il.cnty.sums$Wetland_salamander_te + il.cnty.sums$Wetland_turtle_te + il.cnty.sums$Wetland_liz_snake_te)
sum(il.cnty.sums$Tot_wetland_threatened_endangered == il.cnty.sums$Wetland_amphibian_te + il.cnty.sums$Wetland_reptile_te)

#### Write results to file
il.cnty.sums$County = trimws(il.cnty.sums$County)
il.cnty.sums$County[which(il.cnty.sums$County == "DeWitt")] = "De Witt"
write.csv(il.cnty.sums, "Illinois_County_Herp_Richness.csv", row.names=F)

### Estimate various state-total statistics
sum(il.herp.df$Category == "Frogs and toads")
sum(il.herp.df$Category == "Salamanders")
sum(il.herp.df$Category == "Turtles")
sum(il.herp.df$Category == "Lizards and snakes")

sum(il.herp.df$wet.frog.toad)
sum(il.herp.df$wet.salamander)
sum(il.herp.df$wet.turtle)
sum(il.herp.df$wet.liz.snake)

sum(il.herp.df$wet.frog.toad.te) + sum(il.herp.df$wet.salamander.te)
sum(il.herp.df$wet.turtle.te) + sum(il.herp.df$wet.liz.snake.te)
sum(il.herp.df$wet.frog.toad.te)
sum(il.herp.df$wet.salamander.te)
sum(il.herp.df$wet.turtle.te)
sum(il.herp.df$wet.liz.snake.te)
