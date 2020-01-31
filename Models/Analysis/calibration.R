library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/NetLogo/netlogo6'))
source(paste0(Sys.getenv("CN_HOME"),'/Models/Utils/R/plots.R'))

#resdirpref='20180615_1623_NSGA2_SYNTHETIC_TRCOST1.0_DISTRIBSD0.1'
#res <- as.tbl(read.csv(paste0('explo/',resdirpref,'/population879.csv')))
resdirpref='20180722_1631_NSGA2_SYNTHETIC_TRCOST3_DISTRIBSD0.01'
generation='50000'

res <- as.tbl(read.csv(paste0('explo/',resdirpref,'/population',generation,'.csv')))
resdir=paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/Calibration/',resdirpref);dir.create(resdir)

#minsamples = 5
#minsamples = 1
#minsamples = 0

for(minsamples in c(0,1,5)){
sres = res[res$evolution.samples>minsamples,]

g=ggplot(sres,aes(x=totalWaste,y=relativeCost,size=gravityDecay/2,color=overlapThreshold))
g+geom_point()+xlab("Total waste")+ylab("Relative cost")+
  scale_color_continuous(name=expression(T[0]))+scale_size_continuous(name=expression(d[0]))+stdtheme
ggsave(file=paste0(resdir,'/pareto-waste-cost_minsamples',minsamples,'_gen',generation,'.png'),width=18,height = 15,units = 'cm')

}

