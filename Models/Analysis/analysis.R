
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/CircularEconomy/Results/Exploration'))


#res <- as.tbl(read.csv('20160701_gridlocal/2016_07_01_07_18_26_grid_local.csv'))
res <- as.tbl(read.csv('20160703_grid/2016_07_03_20_09_08_grid_uniform.csv'))


sres = res %>% group_by(id)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=mean(totalCost),totalWaste=mean(totalWaste),
  transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
  distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
)

#sres %>% mutate(id=1:nrow(sres))#group_indices(sres))

indics = c("finalTime","nwClustCoef","nwComponents","nwInDegree","nwMeanFlow","nwOutDegree","totalCost","totalWaste")

plotlist = list()
for(indic in indics){
 g = ggplot(sres,aes_string(x="distribSd",y="gravityDecay",fill=indic))
 plotlist[[indic]]=g+geom_raster(hjust=0,vjust=0)+facet_grid(transportationCost~overlapThreshold,scales = "free")+scale_fill_gradient(low='yellow',high='red')
}
multiplot(plotlist=plotlist,cols=4)

####
#

# Pareto Front

g = ggplot(sres,aes(x=totalWaste,y=totalCost,color=gravityDecay))
g+geom_point(size=0.5)+facet_grid(transportationCost~distribSd,scales = "free")


##########
## convergence

param_points = sample.int(2916,size=5)
sample = res[res$id%in%param_points,]
sample$id=as.character(sample$id)

plotlist = list()
for(indic in indics){
  g=ggplot(sample)
  plotlist[[indic]]=g+geom_density(aes_string(x=indic,fill="id",group="id"),alpha=0.4)
}
multiplot(plotlist=plotlist,cols=4)
