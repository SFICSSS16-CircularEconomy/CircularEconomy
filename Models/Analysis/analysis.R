
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/CircularEconomy/Results/Exploration'))
source(paste0(Sys.getenv("CN_HOME"),'/Models/Utils/R/plots.R'))

#ressynth <- as.tbl(read.csv('20160701_gridlocal/2016_07_01_07_18_26_grid_local.csv'))
ressynth <- as.tbl(read.csv('20170127_grid_synthetic/2017_01_27_18_38_23_grid_synthetic.csv'))
resgis <- as.tbl(read.csv('20160706_grid_gis/2016_07_06_08_36_31_grid_gis_corrected.csv'))
#res=res[res$finalTime!="null"&res$nwClustCoef!="null"&res$nwComponents!="null"&res$nwInDegree!="null"&res$nwClustCoef!="null"&res$nwOutDegree!="null"&res$totalCost!="null"&res$totalWaste!="null",]

synthresprefix = '20170127_grid_synthetic'

sressynth = ressynth %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=mean(as.numeric(totalCost)),totalWaste=mean(totalWaste),count=n()#,
  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
)

sressynthmed = ressynth %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=quantile(as.numeric(totalCost),0.5),totalWaste=quantile(totalWaste,0.5),count=n()#,
  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
)

sresgis = resgis %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=mean(as.numeric(totalCost)),totalWaste=mean(totalWaste),count=n()#,
  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
)



# construct comparable df
rgis = sresgis[sresgis$gravityDecay%in%sressynth$gravityDecay,] %>% arrange(distribSd,gravityDecay,overlapThreshold,transportationCost)
rsynth = sressynth %>% arrange(distribSd,gravityDecay,overlapThreshold,transportationCost)
rdiff = rgis-rsynth
rdiff$distribSd=rgis$distribSd;rdiff$gravityDecay=rgis$gravityDecay;rdiff$overlapThreshold=rgis$overlapThreshold;rdiff$transportationCost=rgis$transportationCost

#ids = rep(0,nrow(res))
#for(i in 1:nrow(sres)){ids[]=i}
#sres %>% mutate(id=1:nrow(sres))#group_indices(sres))

#indics = c("finalTime","nwClustCoef","nwComponents","nwInDegree","nwMeanFlow","nwOutDegree","totalCost","totalWaste")
indics = c("finalTime","nwClustCoef","totalCost","totalWaste")


plotlist = list()
for(indic in indics){
 g = ggplot(rdiff,aes_string(x="overlapThreshold",y="gravityDecay",fill=indic))
 plotlist[[indic]]=g+geom_raster(hjust=0,vjust=0)+facet_grid(transportationCost~distribSd,scales = "free")+scale_fill_gradient(low='yellow',high='red')
}
multiplot(plotlist=plotlist,cols=2)


# same with fixed param values
distribSdVal = 0.05
transportationCostVal=0.5
indics = c("finalTime","nwClustCoef","totalCost","totalWaste")

plotlist = list()
for(indic in indics){
  g = ggplot(rdiff[rdiff$distribSd==distribSdVal&rdiff$transportationCost==transportationCostVal,],aes_string(x="overlapThreshold",y="gravityDecay",fill=indic))
  plotlist[[indic]]=g+geom_raster(hjust=0,vjust=0)+scale_fill_gradient(low='yellow',high='red')
}
multiplot(plotlist=plotlist,cols=2)




####
#

# Pareto Front

g = ggplot(sres,aes(x=totalWaste,y=totalCost,color=overlapThreshold))
g+geom_point(size=0.5)+facet_grid(transportationCost~distribSd,scales = "free")


##
# for particular values of distribSd and tarnsportationCost == fixed "economic context"
g = ggplot(sres[sres$distribSd==0.25&sres$transportationCost==1,],
           aes(x=totalWaste,y=totalCost,color=gravityDecay))
g+geom_point(size=2)



###
# for diff
distribSdVal = 0.6
transportationCostVal=0.5

for(distribSdVal in unique(sressynth$distribSd)){
  for(transportationCostVal in unique(sressynth$transportationCost)){
    g = ggplot(sressynth[sressynth$distribSd==distribSdVal&sressynth$transportationCost==transportationCostVal,],
               aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
    g+geom_point()+scale_size_area(max_size = 2)+ggtitle(paste0('Uniform ; distribSd=',distribSdVal,' ; transportationCost=',transportationCostVal))
    ggsave(paste0(synthresprefix,'/pareto_distribSd',distribSdVal,'_trCost',transportationCostVal,'.pdf'),width = 8,height = 6) 
  }
}
  




g = ggplot(sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,],
           aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
g+geom_point()+scale_size_area(max_size = 2)+ggtitle(paste0('GIS ; distribSd=',distribSdVal,' ; transportationCost=',transportationCostVal))


g = ggplot(resgis[resgis$distribSd==distribSdVal&resgis$transportationCost==transportationCostVal,],
           aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
g+geom_point()+geom_smooth()+scale_size_area(max_size = 2)+ggtitle(paste0('GIS ; distribSd=',distribSdVal,' ; transportationCost=',transportationCostVal))




#######
## along 1 param

indic="totalCost"
g = ggplot(sres[sres$distribSd==0.25&sres$transportationCost==1,],aes_string(x="gravityDecay",y=indic,colour="overlapThreshold",group="overlapThreshold"))
g+geom_line()+facet_grid(transportationCost~distribSd,scales = "free")


##########
## convergence

# pb - need to construct id for gis and synthetic city system.

#param_points = sample.int(2916,size=5)
param_points = c(27,182,1717,1533,1178)
sample = res[res$id%in%param_points,]
sample$id=as.character(sample$id)

plotlist = list()
for(indic in c("totalWaste","nwInDegree")){
  g=ggplot(sample)
  plotlist[[indic]]=g+geom_density(aes_string(x=indic,fill="id",group="id"),alpha=0.4)
}
multiplot(plotlist=plotlist,cols=2)



##########
## Pareto score

getParetoFront <- function(o1,o2){
  dominated = rep(FALSE,length(o1))
  for(i in 1:length(o1)){
    if(!dominated[i]){
      dominated[o1[i]<o1&o2[i]<o2] = TRUE
    }
  }
  return(!dominated)
}

distribSdVal = 0.55
transportationCostVal=0.0
pareto = sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,]
length(which(getParetoFront(pareto$totalCost,pareto$totalWaste)))

transportationCosts=c()
distribSds=c()
paretosizes=c()
for(transportationCostVal in unique(sressynth$transportationCost[sressynth$transportationCost>0])){
  for(distribSdVal in unique(sressynth$distribSd)){
    show(paste0(transportationCostVal,distribSdVal))
    #pareto = sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,]
    pareto = sressynth[sressynth$distribSd==distribSdVal&sressynth$transportationCost==transportationCostVal,]
    #pareto = ressynth[ressynth$distribSd==distribSdVal&ressynth$transportationCost==transportationCostVal,]
    paretosizes=append(paretosizes,length(which(getParetoFront(pareto$totalCost,pareto$totalWaste)))/nrow(pareto))
    transportationCosts=append(transportationCosts,transportationCostVal)
    distribSds=append(distribSds,distribSdVal)
  }
}

g=ggplot(data.frame(paretosizes,transportationCost=transportationCosts,distribSd=distribSds),aes(x=transportationCost,y=paretosizes,color=distribSd))
g+geom_point()+geom_smooth()

g=ggplot(data.frame(paretosizes,transportationCost=transportationCosts,distribSd=distribSds),aes(x=distribSd,y=paretosizes,color=transportationCost))
g+geom_point()+geom_smooth()

