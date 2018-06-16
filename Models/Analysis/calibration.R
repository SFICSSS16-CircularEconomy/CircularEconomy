library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/NetLogo/netlogo6'))
source(paste0(Sys.getenv("CN_HOME"),'/Models/Utils/R/plots.R'))

resdirpref='20180615_1623_NSGA2_SYNTHETIC_TRCOST1.0_DISTRIBSD0.1'
res <- as.tbl(read.csv(paste0('explo/',resdirpref,'/population879.csv')))
resdir=paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/Calibration/',resdirpref);dir.create(resdir)

sres = res[res$evolution.samples>5,]

g=ggplot(sres,aes(x=totalWaste,y=relativeCost,size=gravityDecay/2,color=overlapThreshold))
g+geom_point()+xlab("Total waste")+ylab("Relative cost")+
  scale_color_continuous(name=expression(T[0]))+scale_size_continuous(name=expression(d[0]))+stdtheme
ggsave(file=paste0(resdir,'/pareto-waste-cost.png'),width=18,height = 15,units = 'cm')



