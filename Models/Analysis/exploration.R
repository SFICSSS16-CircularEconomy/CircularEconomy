library(dplyr)
library(ggplot2)
library(GGally)


setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/Netlogo/netlogo6'))
source(paste0(Sys.getenv("CS_HOME"),'/CityNetwork/Models/Utils/R/plots.R'))

#resdirpref='2018_06_16_01_09_28_DIRECTSAMPLING_SYNTHETIC_LOCAL'
#resdirpref='2018_06_15_18_20_34_DIRECTSAMPLING_SYNTHETIC'
resdirpref='2018_06_16_21_01_13_DIRECTSAMPLING_SYNTHETIC'
#res <- as.tbl(read.csv(paste0('explo/',resdirpref,'.csv')))
res <- as.tbl(read.csv(paste0('exploration/',resdirpref,'.csv')))
resdir=paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/Exploration/',resdirpref,'/');dir.create(resdir)


res$circularity=1 - res$totalWaste
res$clusteringLevel = 20 - res$averageDistanceVariability
res$regime=ifelse(res$totalWaste>0.45,"low","high") # in terms of circularity

#trcost=3.5
trcost=0.5


## Question : determinants of output regime ?

g=ggplot(res[res$transportationCost==trcost,],aes(x=nwMeanFlow,color=averageDistanceVariability))
g+geom_density()+facet_grid(overlapThreshold~distribSd+gravityDecay)

g=ggplot(res[res$transportationCost==trcost,],aes(x=averageDistanceVariability,y=totalWaste,color=finalTime))
g+geom_point()+facet_grid(overlapThreshold~distribSd+gravityDecay)
hist(res$relativeCost/res$nwMeanFlow,breaks=1000)

lmres = lm(totalWaste~averageDistanceVariability+distribSd+gravityDecay+overlapThreshold+transportationCost,data=res)
summary(lmres)


lmres = glm(regime~averageDistanceVariability+distribSd+gravityDecay+overlapThreshold+transportationCost,data=res,family = "binomial")
summary(lmres)

res$averageDistanceVariabilityCat = cut(res$averageDistanceVariability,5)
ggpairs(data=res,columns = c("totalWaste","nwMeanFlow","nwComponents","relativeCost"),
        aes(colour=averageDistanceVariabilityCat,alpha=0.4)
)
# -> understand the second regime

g=ggplot(res[res$transportationCost==trcost,],aes(x=averageDistanceVariability,y=totalWaste,group=gravityDecay,color=gravityDecay))
g+geom_point(pch='.')+geom_smooth()+facet_grid(distribSd~overlapThreshold)
ggsave(file=paste0(resdir,'totalWaste_facetsd-overlap_trCost',trcost,'.png'),width=30,height=20,units='cm')

# nicer plot
#res$sigma = unlist(sapply(res$distribSd,function(s){bquote(sigma*'='*.(s))}))
# trick the regime when few points only
res$regime[res$overlapThreshold==0&res$distribSd==0.6]="high"
#g=ggplot(res[res$transportationCost==trcost&res$overlapThreshold%in%c(0,0.5),],aes(x=clusteringLevel,y=circularity,group=interaction(gravityDecay,regime),color=gravityDecay,linetype=regime))
g=ggplot(res[res$transportationCost==trcost&res$overlapThreshold%in%c(0.01,0.05),],aes(x=clusteringLevel,y=circularity,group=gravityDecay,color=gravityDecay))

g+geom_point(pch='.')+geom_smooth()+facet_grid(distribSd~overlapThreshold,scales="free")+
  xlab('Level of clustering')+ylab('Level of circularity')+scale_color_continuous(name=expression(d[0]))+stdtheme
ggsave(file=paste0(resdir,'totalWaste_facetsd-overlap_trCost',trcost,'_extract_withRegime.png'),width=18,height=15,units='cm')



