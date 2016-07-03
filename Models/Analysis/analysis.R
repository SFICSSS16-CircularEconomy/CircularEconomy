
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/CircularEconomy/Results/Exploration'))


res <- as.tbl(read.csv('20160701_gridlocal/2016_07_01_07_18_26_grid_local.csv'))



sres = res %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=mean(totalCost),totalWaste=mean(totalWaste)
)
sres %>% mutate(id=1:nrow(sres))#group_indices(sres))


g = ggplot(sres,aes(x=transportationCost,y=totalCost,color=gravityDecay,group=gravityDecay))
g+geom_line()+facet_grid(distribSd~overlapThreshold,scales = "free")



####
#

g = ggplot(sres,aes(x=totalWaste,y=totalCost,color=distribSd))
g+geom_point(size=0.5)+facet_grid(transportationCost~gravityDecay,scales = "free")


