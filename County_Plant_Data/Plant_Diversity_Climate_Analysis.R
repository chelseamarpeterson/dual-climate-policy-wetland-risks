setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Chapter5")

# read in county species data
plant.df = read.csv("County_Species_Data/County_Plant_Diversity_Summary.csv")

# read in climate data and join with plant data
ranges = c("2025-2049","2050-2074","2075-2099","2025-2099")
n.r = length(ranges)
clim.df.list= list()
for (i in 1:n.r) {
  df.name = paste(ranges[i], "County_Climate_Differences.csv", sep="_")
  clim.df = read.csv(paste("IL_County_Climate_USGS/", df.name, sep=""))
  clim.plant.df = left_join(plant.df, clim.df, by="county")
  clim.df.list[[ranges[i]]] = clim.plant.df
}

# join plant and climate data
clim.plant.df.all = clim.df.list[[ranges[1]]]
for (i in 2:n.r) {
  clim.plant.df.all = rbind(clim.plant.df.all, clim.df.list[[ranges[i]]])
}

# ssp scenarios
ssps = c("ssp245","ssp370","ssp585")
n.s = length(ssps)

# separate data by SSP scenario
name.cols = colnames(clim.plant.df.all)[1:5]
ssp.cols = colnames(clim.plant.df.all)[6:ncol(clim.plant.df.all)]
clim.cols = c("Mean temperature (deg F)","Max temperature (deg F)","Min temperature(deg F)",
              "Precipitation (in/mo)","Runoff (in/mo)","Snow depth (in)",
              "Soil storage (in)","Evaporation deficit (in/mo)")
ssp.ind.list = list()
ssp.ind.list[[1]] = seq(1,8)
ssp.ind.list[[2]] = seq(9,16)
ssp.ind.list[[3]] = seq(17,24)
ssp.stack.df = data.frame(matrix(nrow=0,ncol=length(c(name.cols, clim.cols))+1))
colnames(c.ssp.stack.df) = c("ssp", name.cols, clim.cols)
for (i in 1:n.s) {
  ssp.i.df = clim.plant.df.all[,c(name.cols, ssp.cols[ssp.ind.list[[i]]])]
  colnames(ssp.i.df) = c(name.cols, clim.cols)
  ssp.i.df$ssp = ssps[i]
  ssp.stack.df = rbind(ssp.stack.df, ssp.i.df[,c("ssp",name.cols,clim.cols)])
}

# melt climate dataframe by variable
ssp.melt = melt(ssp.stack.df, 
                id.vars=c("ssp","county","range","n","mean.c","fqi"))


# scatter plot matrix of mean wetland area v. all other variable
omit.vars = clim.cols[c(2,3)]
ssp.plot = ssp.melt[-which(ssp.melt$variable %in% omit.vars),]
ggplot(ssp.plot, 
       aes(x=value, 
           y=mean.c, 
           color=range)) + 
        geom_point() + 
        geom_smooth(method="lm") +
        ggh4x::facet_grid2(cols=vars(variable),
                           rows=vars(ssp), 
                           scales="free_x") + 
        labs(x="Change in climate variable relative to 1981-2010",
             y="County-level mean C") +
        guides(color=guide_legend(title="Future year interval"))

ggplot(ssp.plot, 
       aes(x=value, 
           y=n, 
           color=range)) + 
        geom_point() + 
        geom_smooth(method="lm") +
        ggh4x::facet_grid2(cols=vars(variable),
                           rows=vars(ssp), 
                           scales="free_x") + 
        labs(x="Change in climate variable relative to 1981-2010",
             y="County-level richness") +
        guides(color=guide_legend(title="Future year interval"))

ggplot(ssp.plot, 
       aes(x=value, 
           y=fqi, 
           color=range)) + 
        geom_point() + 
        geom_smooth(method="lm") +
        ggh4x::facet_grid2(cols=vars(variable),
                           rows=vars(ssp), 
                           scales="free_x") + 
        labs(x="Change in climate variable relative to 1981-2010",
             y="County-level FQI") +
        guides(color=guide_legend(title="Future year interval"))

################################################################################
# fit bayesian linear model for each ssp and year interval

# make data lists
plant.vars = c("n","mean.c","fqi")
n.p = length(plant.vars)
n.c = length(clim.cols)
model.lists = list()
mean.lists = list()
sd.lists = list()
for (i in 1:n.r) {
  range.i = ranges[i]
  df.i = ssp.stack.df[which(ssp.stack.df$range == range.i),]
  model.lists[[range.i]] = list()
  mean.lists[[range.i]] = list()
  sd.lists[[range.i]] = list()
  for (j in 1:n.s) {
    ssp.j = ssps[j]
    df.ij = df.i[which(df.i$ssp == ssp.j),]
    model.lists[[range.i]][[ssp.j]] = list()
    mean.lists[[range.i]][[ssp.j]] = list()
    sd.lists[[range.i]][[ssp.j]] = list()
    for (k in 1:n.p) {
      pvar.k = plant.vars[k]
      model.lists[[range.i]][[ssp.j]][[pvar.k]] = list()
      mean.lists[[range.i]][[ssp.j]][[pvar.k]] = mean(df.ij[,pvar.k])
      sd.lists[[range.i]][[ssp.j]][[pvar.k]] = sd(df.ij[,pvar.k])
      for (l in 1:n.c) {
        cvar.l = clim.cols[l]
        mean.lists[[range.i]][[ssp.j]][[cvar.l]] = mean(df.ij[,cvar.l])
        sd.lists[[range.i]][[ssp.j]][[cvar.l]] = sd(df.ij[,cvar.l])
        model.lists[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]] = list(y=(df.ij[,pvar.k]-mean(df.ij[,pvar.k]))/sd(df.ij[,pvar.k]),
                                                                   x=(df.ij[,cvar.l]-mean(df.ij[,cvar.l]))/sd(df.ij[,cvar.l]))
        
      }
    }
  }
}

df = data.frame(model.lists[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]])
plot(y ~ x, data=df)

# run model for each combination
set.seed(547)
plant.clim.models = list()
for (i in 1:n.r) {
  range.i = ranges[i]
  plant.clim.models[[range.i]] = list()
  for (j in 1:n.s) {
    ssp.j = ssps[j]
    plant.clim.models[[range.i]][[ssp.j]] = list()
    for (k in 1:n.p) {
      pvar.k = plant.vars[k]
      plant.clim.models[[range.i]][[ssp.j]][[pvar.k]] = list()
      for (l in 1:n.c) {
        cvar.l = clim.cols[l]
        m.v = ulam(alist(y ~ normal(mu, sigma),
                         mu <- a + b*x,
                         a ~ dnorm(0, 1),
                         b ~ dnorm(0, 1),
                         sigma ~ dexp(1)),
                   data=model.lists[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]], 
                   chains=1, log_lik=T)
        plant.clim.models[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]] = m.v
      }
    }
  }
}
 
# save samples in list
a = list()
b = list()
for (i in 1:n.r) {
  range.i = ranges[i]
  a[[range.i]] = list()
  b[[range.i]] = list()
  for (j in 1:n.s) {
    ssp.j = ssps[j]
    a[[range.i]][[ssp.j]] = list()
    b[[range.i]][[ssp.j]] = list()
    for (k in 1:n.p) {
      pvar.k = plant.vars[k]
      a[[range.i]][[ssp.j]][[pvar.k]] = list()
      b[[range.i]][[ssp.j]][[pvar.k]] = list()
      for (l in 1:n.c) {
        cvar.l = clim.cols[l]
        samples = extract.samples(plant.clim.models[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]])
        a[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]] = samples$a
        b[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]] = samples$b
      }
    }
  }
}

# save model data in dataframe
stat.df = data.frame(matrix(nrow=n.r*n.s*n.p*n.c*2,ncol=8))
colnames(stat.df) = c("range","ssp","plant.var","clim.var","model.par","mean","5","95")
vars = c("a","b")
n.v = length(vars)
n = 1
for (m in 1:n.v) {
  var.m = vars[m]
  for (i in 1:n.r) {
    range.i = ranges[i]
    for (j in 1:n.s) {
      ssp.j = ssps[j]
      for (k in 1:n.p) {
        pvar.k = plant.vars[k]
        for (l in 1:n.c) {
          cvar.l = clim.cols[l]
          stat.df[n,"range"] = range.i
          stat.df[n,"ssp"] = ssp.j
          stat.df[n,"plant.var"] = pvar.k
          stat.df[n,"clim.var"] = cvar.l
          stat.df[n,"model.par"] = var.m
          if (var.m == "a") {
            stat.df[n,"mean"] = mean(a[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]])
            stat.df[n,"5"] = HPDI(a[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]], prob=0.90)[1]
            stat.df[n,"95"] = HPDI(a[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]], prob=0.90)[2]
          } else {
            stat.df[n,"mean"] = mean(b[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]])
            stat.df[n,"5"] = HPDI(b[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]], prob=0.90)[1]
            stat.df[n,"95"] = HPDI(b[[range.i]][[ssp.j]][[pvar.k]][[cvar.l]], prob=0.90)[2]
          }
          n = n + 1
        }
      }
    }
  }
}

# write to file
write.csv(stat.df, "County_Species_Data/Plant_Versus_Climate_Model_Stats.csv", row.names = F)

b.df = stat.df[which(stat.df$model.par == "b" & stat.df$plant.var == "mean.c"),]
ggplot(b.df) + 
       geom_point(aes(x=mean, y=range, color=range)) +
       geom_errorbarh(aes(xmin=`5`,xmax=`95`, 
                          y=range, color=range), height=0.2) + 
       geom_vline(xintercept=0) + 
       labs(y="Year range",x="Effect size for change in mean C\nv. change in climate variable")+
       ggh4x::facet_grid2(cols=vars(clim.var),
                          rows=vars(ssp), 
                          scales="free_x")
b.df = stat.df[which(stat.df$model.par == "b" & stat.df$plant.var == "fqi"),]
ggplot(b.df) + 
        geom_point(aes(x=mean, y=range, color=range)) +
        geom_errorbarh(aes(xmin=`5`,xmax=`95`, 
                           y=range, color=range), height=0.2) + 
        geom_vline(xintercept=0) + 
        labs(y="Year range",x="Effect size for change in FQI\nv. change in climate variable")+
        ggh4x::facet_grid2(cols=vars(clim.var),
                           rows=vars(ssp), 
                           scales="free_x")   
b.df = stat.df[which(stat.df$model.par == "b" & stat.df$plant.var == "n"),]
ggplot(b.df) + 
      geom_point(aes(x=mean, y=range, color=range)) +
      geom_errorbarh(aes(xmin=`5`,xmax=`95`, 
                         y=range, color=range), height=0.2) + 
      geom_vline(xintercept=0) + 
      labs(y="Year range",x="Effect size for change in richness\nv. change in climate variable")+
      ggh4x::facet_grid2(cols=vars(clim.var),
                         rows=vars(ssp), 
                         scales="free_x")   