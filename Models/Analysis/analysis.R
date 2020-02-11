
library(dplyr)
library(ggplot2)
library(Matrix)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/Exploration'))
#setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/Netlogo/netlogo6'))
source(paste0(Sys.getenv("CS_HOME"),'/Organisation/Models/Utils/R/plots.R'))


#ressynth <- as.tbl(read.csv('20160701_gridlocal/2016_07_01_07_18_26_grid_local.csv'))
#ressynth <- as.tbl(read.csv('20170127_grid_synthetic/2017_01_27_18_38_23_grid_synthetic.csv'))
#resdirpref='2018_06_19_18_50_44_DIRECTSAMPLING_SYNTHETIC'
#ressynth <- as.tbl(read.csv(paste0('exploration/',resdirpref,'.csv')))
#resgis <- as.tbl(read.csv('20160706_grid_gis/2016_07_06_08_36_31_grid_gis_corrected.csv'))
#res=res[res$finalTime!="null"&res$nwClustCoef!="null"&res$nwComponents!="null"&res$nwInDegree!="null"&res$nwClustCoef!="null"&res$nwOutDegree!="null"&res$totalCost!="null"&res$totalWaste!="null",]

# latest simulation file
resdirpref = '20180713_210239_DIRECTSAMPLING_SYNTHETIC'
res <- as.tbl(read.csv(paste0(resdirpref,'/data/20180713_210239_DIRECTSAMPLING_SYNTHETIC.csv')))
resdir <- paste0(resdirpref,'/')

#####
## raw plots

g = ggplot(res,aes(x=gravityDecay,y = totalWaste,group = interaction(distribSd,setupType),color=distribSd, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(transportationCost~overlapThreshold)+ylab('Total waste')+xlab(expression(d[0]))+
  scale_color_continuous(name=expression(sigma))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'totalWaste_d0_facet.png'),width=60,height = 30, units='cm')

# test sensitivity to setup type
summary(res %>% group_by(gravityDecay,overlapThreshold,distribSd,transportationCost)%>% summarise(waste = mean(totalWaste[as.character(setupType)=="uniform"])-mean(totalWaste[as.character(setupType)=="synthetic-city-system"])))
# -> up to 0.22 difference in totalWaste

g = ggplot(res[res$distribSd%in%c(min(res$distribSd),max(res$distribSd))&res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$overlapThreshold%in%c(min(res$overlapThreshold),max(res$overlapThreshold)),],
           aes(x=gravityDecay,y = totalWaste,group = interaction(distribSd,setupType),color=distribSd, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(transportationCost~overlapThreshold)+ylab('Total waste')+xlab(expression(d[0]))+
  scale_color_continuous(name=expression(sigma))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'totalWaste_d0_facet_extract.png'),width=60,height = 30, units='cm')

g = ggplot(res[res$distribSd%in%c(min(res$distribSd),max(res$distribSd))&res$gravityDecay%in%c(min(res$gravityDecay),max(res$gravityDecay))&res$overlapThreshold%in%c(min(res$overlapThreshold),max(res$overlapThreshold)),],
           aes(x=transportationCost,y = totalWaste,group = interaction(distribSd,setupType),color=distribSd, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(gravityDecay~overlapThreshold)+ylab('Total waste')+xlab(expression(c))+
  scale_color_continuous(name=expression(sigma))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'totalWaste_c_facet_extract.png'),width=60,height = 30, units='cm')

g = ggplot(res[res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$gravityDecay%in%c(min(res$gravityDecay),max(res$gravityDecay))&res$overlapThreshold%in%c(min(res$overlapThreshold),max(res$overlapThreshold)),],
           aes(x=distribSd,y = totalWaste,group = interaction(transportationCost,setupType),color=transportationCost, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(gravityDecay~overlapThreshold)+ylab('Total waste')+xlab(expression(sigma))+
  scale_color_continuous(name=expression(c))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'totalWaste_sigma_facet_extract.png'),width=60,height = 30, units='cm')

g = ggplot(res[res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$gravityDecay%in%c(min(res$gravityDecay),max(res$gravityDecay))&res$distribSd%in%c(min(res$distribSd),max(res$distribSd)),],
           aes(x=overlapThreshold,y = totalWaste,group = interaction(transportationCost,setupType),color=transportationCost, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(gravityDecay~distribSd)+ylab('Total waste')+xlab(expression(theta))+
  scale_color_continuous(name=expression(c))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'totalWaste_theta_facet_extract.png'),width=60,height = 30, units='cm')


## cost

g = ggplot(res[res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$gravityDecay%in%c(min(res$gravityDecay),max(res$gravityDecay))&res$overlapThreshold%in%c(min(res$overlapThreshold),max(res$overlapThreshold)),],
           aes(x=distribSd,y = relativeCost,group = interaction(transportationCost,setupType),color=transportationCost, linetype=setupType))+geom_point(pch='.')+
  geom_smooth()+facet_grid(gravityDecay~overlapThreshold)+ylab('Relative cost')+xlab(expression(sigma))+
  scale_color_continuous(name=expression(c))+scale_linetype_discrete(name="Setup")+stdtheme
ggsave(plot=g,file=paste0(resdir,'relativeCost_sigma_facet_extract.png'),width=30,height = 15, units='cm')


## Pareto

g=ggplot(res[res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$distribSd%in%c(min(res$distribSd),max(res$distribSd)),],
         aes(x=relativeCost,y=totalWaste,color=overlapThreshold))+
  geom_point(size=0.2,alpha=0.3)+
  facet_grid(transportationCost~distribSd)+
  xlab("Total waste")+ylab("Relative cost")+scale_color_continuous(name=expression(theta))+stdtheme
ggsave(plot=g,file=paste0(resdir,'pareto_theta_facet_extract.png'),width=30,height = 15, units='cm')

g=ggplot(res[res$transportationCost%in%c(min(res$transportationCost),max(res$transportationCost))&res$distribSd%in%c(min(res$distribSd),max(res$distribSd)),],
         aes(x=relativeCost,y=totalWaste,color=setupType))+
  geom_point(size=0.2,alpha=0.3)+
  facet_grid(transportationCost~distribSd)+
  xlab("Total waste")+ylab("Relative cost")+stdtheme
ggsave(plot=g,file=paste0(resdir,'pareto_setupType_facet_extract.png'),width=30,height = 25, units='cm')


## Statistics : CI and sharpe ratio

# in the case of n~64, we have |CI|~sigma -> compare |mu_i - mu-j|/sigma

sres = res[res$setupType=="uniform",]%>%group_by(gravityDecay,overlapThreshold,distribSd,transportationCost)%>%summarize(waste=mean(totalWaste),wasteSd=sd(totalWaste),cost=mean(relativeCost),costSd=sd(relativeCost))

#(Matrix(rep(sres$waste,nrow(sres)),nrow = nrow(sres),byrow = T)-Matrix(rep(sres$waste,nrow(sres)),nrow = nrow(sres),byrow = F))

sharpDiffMatrix <- function(x,s){
 res = 2 * abs(Matrix(rep(x,length(x)),nrow = length(x),byrow = T)-Matrix(rep(x,length(x)),nrow = length(x),byrow = F)) /(Matrix(rep(s,length(s)),nrow = length(s),byrow = T)+Matrix(rep(s,length(s)),nrow = length(s),byrow = F))
 diag(res)<- Inf
 return(res@x[is.finite(res@x)])
}

sharpeWaste = sharpDiffMatrix(sres$waste,sres$wasteSd);gc()
summary(sharpeWaste)
quantile(sharpeWaste,seq(0.0,1.0,0.01))
#

sharpeCost = sharpDiffMatrix(sres$cost,sres$costSd);gc()
summary(sharpeCost)
quantile(sharpeCost,seq(0.0,1.0,0.01))
#



#####
## summarized plots

ressynth = res

ressynth$circularity=1 - ressynth$totalWaste
ressynth$clusteringLevel = 20 - ressynth$averageDistanceVariability

#synthresprefix = '20170127_grid_synthetic'
#synthresprefix = '20180619_directsampling_synthetic'
synthresprefix = resdir

#sressynth = ressynth %>%
#sressynth = ressynth[ressynth$setupType=="synthetic-city-system",] %>%
sressynth = ressynth %>%
  group_by(distribSd,gravityDecay,overlapThreshold,transportationCost,clusteringLevel,setupType)%>% summarise(
  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
  totalCost=mean(as.numeric(totalCost)),totalWaste=mean(totalWaste),count=n(),
  cost=mean(relativeCost),waste=mean(totalWaste)
  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
)


#sressynthmed = ressynth %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
#  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
#  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
#  totalCost=quantile(as.numeric(totalCost),0.5),totalWaste=quantile(totalWaste,0.5),count=n()#,
#  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
#  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
#)

#sresgis = resgis %>% group_by(distribSd,gravityDecay,overlapThreshold,transportationCost)%>% summarise(
#  finalTime = mean(finalTime),nwClustCoef=mean(nwClustCoef),nwComponents=mean(nwComponents),
#  nwInDegree=mean(nwInDegree),nwMeanFlow=mean(nwMeanFlow),nwOutDegree=mean(nwOutDegree),
#  totalCost=mean(as.numeric(totalCost)),totalWaste=mean(totalWaste),count=n()#,
#  #transportationCost=mean(transportationCost),gravityDecay=mean(gravityDecay),
#  #distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold)
#)


####
## Heatmaps
#indics = c("nwOutDegree","nwClustCoef","totalCost","totalWaste")
indics = c("waste","cost")

plotlist = list()
for(indic in indics){
  #g = ggplot(sressynth[sressynth$clusteringLevel==5.0,],aes_string(x="overlapThreshold",y="gravityDecay",fill=indic))
  #plotlist[[indic]]=g+geom_raster(hjust=0,vjust=0)+facet_grid(transportationCost~distribSd,scales = "free")+scale_fill_gradient(low='yellow',high='red')
  
  g = ggplot(sressynth)
  g+geom_raster(aes_string(x="transportationCost",y="distribSd",fill=indic),hjust=0,vjust=0)+facet_grid(overlapThreshold~gravityDecay,scales = "free")+stdtheme
  ggsave(file=paste0(resdir,'heatmap_',indic,'_facet.png'),width=32,height=30,units='cm')
  
  g = ggplot(sressynth[sressynth$transportationCost%in%c(min(sressynth$transportationCost),max(sressynth$transportationCost))&sressynth$distribSd%in%c(min(sressynth$distribSd),max(sressynth$distribSd)),])
  g+geom_raster(aes_string(x="overlapThreshold",y="gravityDecay",fill=indic),hjust=0,vjust=0)+facet_grid(transportationCost~distribSd,scales = "free")+stdtheme
  ggsave(file=paste0(resdir,'heatmap_',indic,'_facetextreme.png'),width=32,height=30,units='cm')
}
#multiplot(plotlist=plotlist,cols=2)



####
## Differences

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

sres=sressynth

#g = ggplot(sres,aes(x=totalWaste,y=totalCost,color=overlapThreshold))
g = ggplot(sres,aes(x=waste,y=cost,color=overlapThreshold))
g+geom_point(size=0.5)+facet_grid(transportationCost~distribSd,scales = "free")+stdtheme+scale_color_continuous(name=expression(T[0]))
ggsave(file=paste0(resdir,'pareto_fullfacet.png'),width=32,height=30,units='cm')

##
# for particular values of distribSd and tarnsportationCost == fixed "economic context"
g = ggplot(sres[sres$distribSd==0.25&sres$transportationCost==1,],
           aes(x=totalWaste,y=totalCost,color=gravityDecay))
g+geom_point(size=2)



###
# for diff
distribSdVal = 0.6
transportationCostVal=0.5

dir.create(paste0(synthresprefix,'paretos'))

for(distribSdVal in unique(sressynth$distribSd)){
  for(transportationCostVal in unique(sressynth$transportationCost)){
    for(setupType in unique(sres$setupType)){
      g = ggplot(sressynth[sressynth$distribSd==distribSdVal&sressynth$transportationCost==transportationCostVal&sressynth$setupType==setupType,],
                 #aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
                 aes(x=waste,y=cost,color=overlapThreshold,size=gravityDecay))
      g+geom_point()+scale_size_area(max_size = 2,name=expression(d[0]))+scale_color_continuous(name=expression(T[0]))+xlab("Waste")+ylab("Cost")+
        ggtitle(paste0(setupType,' ; s =',distribSdVal,' ; c =',transportationCostVal))+stdtheme
      ggsave(paste0(synthresprefix,'/paretos/pareto_',setupType,'_distribSd',distribSdVal,'_trCost',transportationCostVal,'.png'),width = 6,height = 5) 
    }
  }
}





# tests paretos for gis
#g = ggplot(sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,],
#           aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
#g+geom_point()+scale_size_area(max_size = 2)+ggtitle(paste0('GIS ; distribSd=',distribSdVal,' ; transportationCost=',transportationCostVal))


#g = ggplot(resgis[resgis$distribSd==distribSdVal&resgis$transportationCost==transportationCostVal,],
#           aes(x=totalWaste,y=totalCost,color=overlapThreshold,size=gravityDecay))
#g+geom_point()+geom_smooth()+scale_size_area(max_size = 2)+ggtitle(paste0('GIS ; distribSd=',distribSdVal,' ; transportationCost=',transportationCostVal))




#######
## along 1 param

indic="totalCost"
g = ggplot(sres[sres$distribSd==0.25&sres$transportationCost==1,],aes_string(x="gravityDecay",y=indic,colour="overlapThreshold",group="overlapThreshold"))
g+geom_line()+facet_grid(transportationCost~distribSd,scales = "free")


##########
## convergence


## histograms

# pb - need to construct id for gis and synthetic city system.

#param_points = sample.int(2916,size=5)
#param_points = c(27,182,1717,1533,1178)

# handmade ids for baseline experiment
ids = paste0("d0=",res$gravityDecay,";T0=",res$overlapThreshold,";sigma=",res$distribSd,";c=",res$transportationCost,";setup=",res$setupType)
uids=unique(ids)
vuids = as.character(1:length(uids));names(vuids) = uids
res$Parameters = vuids[ids]

#param_points = sample(vuids,size=6)
param_points=c("14914","16772","18169","21771","22193","8977")

sample = res[res$Parameters%in%param_points,]

#sample$id=as.character(sample$id)

#plotlist = list()
#for(indic in c("totalWaste","nwInDegree")){
#  g=ggplot(sample)
#  plotlist[[indic]]=g+geom_density(aes_string(x=indic,fill="id",group="id"),alpha=0.4)
#}
#multiplot(plotlist=plotlist,cols=2)

g=ggplot(sample,aes(x=totalWaste,fill=Parameters,group=Parameters))
g+geom_density(alpha=0.4)+xlab("Total waste")+ylab("Density")+stdtheme
ggsave(file=paste0(resdir,'distrib_waste.png'),width=22,height=20,units='cm')

g=ggplot(sample,aes(x=relativeCost,fill=Parameters,group=Parameters))
g+geom_density(alpha=0.4)+xlab("Relative cost")+ylab("Density")+stdtheme
ggsave(file=paste0(resdir,'distrib_cost.png'),width=22,height=20,units='cm')

# write param values for the sample
write.csv((sample%>%group_by(Parameters)%>%summarise(gravityDecay=mean(gravityDecay),distribSd=mean(distribSd),overlapThreshold=mean(overlapThreshold),transportationCost=mean(transportationCost),setupType=setupType[1])),file=paste0(resdir,'distrib_sample.csv'))

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

#distribSdVal = 0.55
#transportationCostVal=0.0
#pareto = sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,]
#length(which(getParetoFront(pareto$totalCost,pareto$totalWaste)))

transportationCosts=c()
distribSds=c()
paretosizes=c()
setups=c()
spreads=c()

#costType="totalCost"
costType="cost"

for(transportationCostVal in unique(sressynth$transportationCost[sressynth$transportationCost>0])){
  for(distribSdVal in unique(sressynth$distribSd)){
    for(setupType in unique(sressynth$setupType)){
      show(paste0(transportationCostVal,distribSdVal))
      #pareto = sresgis[sresgis$distribSd==distribSdVal&sresgis$transportationCost==transportationCostVal,]
      #pareto = ressynth[ressynth$distribSd==distribSdVal&ressynth$transportationCost==transportationCostVal,]
      
      currentdata = sressynth[sressynth$distribSd==distribSdVal&sressynth$transportationCost==transportationCostVal&sressynth$setupType==setupType,]
      
      #currentfront = getParetoFront(currentdata$totalCost,currentdata$totalWaste)
      currentfront = getParetoFront(currentdata[[costType]],currentdata$totalWaste)
      
      paretosizes=append(paretosizes,length(which(currentfront))/nrow(pareto))
      transportationCosts=append(transportationCosts,transportationCostVal)
      distribSds=append(distribSds,distribSdVal);setups=append(setups,setupType)
      # relative spread of pareto fronts
      spreads = append(spreads,(max(currentdata$totalWaste[currentfront])-min(currentdata$totalWaste[currentfront]))/(max(currentdata[[costType]][currentfront])-min(currentdata[[costType]][currentfront])))
    }
  }
}
setups[setups=="synthetic-city-system"]="synthetic"

# linear factor for cost does not change the size of the front ! but the relative spread.

g=ggplot(data.frame(paretosizes,transportationCost=transportationCosts,distribSd=distribSds,Setup=setups),
         aes(x=transportationCost,y=paretosizes,color=distribSd,linetype=Setup,group=Setup,shape=Setup))
g+geom_point()+geom_smooth()+xlab("Transportation cost c")+ylab("Size of Pareto front")+
  scale_color_continuous(name=expression(sigma))+stdtheme
ggsave(file=paste0(resdir,'/pareto_sizes_trCost_',costType,'.png'),width=18,height=15,units = 'cm')


g=ggplot(data.frame(paretosizes,transportationCost=transportationCosts,distribSd=distribSds,Setup=setups),
         aes(x=distribSd,y=paretosizes,color=transportationCost,linetype=Setup,group=Setup,shape=Setup))
g+geom_point()+geom_smooth()+xlab("Distribution width")+ylab("Size of Pareto front")+
  scale_color_continuous(name=expression(c))+stdtheme
ggsave(file=paste0(resdir,'/pareto_sizes_sigma_',costType,'.png'),width=18,height=15,units = 'cm')

# spread


g=ggplot(data.frame(paretosizes,spreads,transportationCost=transportationCosts,distribSd=distribSds,Setup=setups),
         aes(x=transportationCost,y=spreads,color=distribSd,linetype=Setup,group=Setup,shape=Setup))
g+geom_point()+geom_smooth()+xlab("Transportation cost c")+ylab("Relative spread of front")+
  scale_color_continuous(name=expression(sigma))+stdtheme
ggsave(file=paste0(resdir,'/pareto_spreads_trCost_',costType,'.png'),width=18,height=15,units = 'cm')


g=ggplot(data.frame(paretosizes,spreads,transportationCost=transportationCosts,distribSd=distribSds,Setup=setups),
         aes(x=distribSd,y=spreads,color=transportationCost,linetype=Setup,group=Setup,shape=Setup))
g+geom_point()+geom_smooth()+xlab("Distribution width")+ylab("Relative spread of front")+
  scale_color_continuous(name=expression(c))+stdtheme
ggsave(file=paste0(resdir,'/pareto_spreads_sigma_',costType,'.png'),width=18,height=15,units = 'cm')






