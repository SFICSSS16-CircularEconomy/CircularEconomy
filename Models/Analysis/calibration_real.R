library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/NetLogo/netlogo6'))
source(paste0(Sys.getenv("CS_HOME"),'/Organisation/Models/Utils/R/plots.R'))

resdirpref='20190614_0905_NSGA2_REAL'
#generation='2400'
generation='28700'

res <- as.tbl(read.csv(paste0('calibration/',resdirpref,'/population',generation,'.csv')))
resdir=paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/CalibrationReal/',resdirpref);dir.create(resdir)


objectives = c("errorRelativeCost","errorLinkLength","errorNetworkSize")
parameters = c("gravityDecay","overlapThreshold","transportationCost","distribSd")

# scatter
plot(res[,objectives])

# conditionally to accurate size ?
# 284 target links ; 28.4 ^ 2 = 806 (1/10th error)
summary(res$errorNetworkSize)
alpha=0.1
dim(res[res$errorNetworkSize<(284*alpha)^2,]) # -> still 64 points

###
condres = res[res$errorNetworkSize<(284*alpha)^2,]
#condres = res[res$errorNetworkSize<(284*alpha)^2&res$evolution.samples>10,]


#plot(condres[,c("errorRelativeCost","errorLinkLength")])
# -> rather cool !

paramnames = list(overlapThreshold = expression(T[0]),transportationCost=expression(c),distribSd=expression(sigma),gravityDecay=expression(d[0]))

for(param in parameters){
  g=ggplot(condres,aes_string(x="errorRelativeCost",y="errorLinkLength",color=param))
  g+geom_point()+xlab("Squared error on average relative cost")+ylab("Squared error on average link length")+
    scale_color_continuous(name=paramnames[[param]])+stdtheme#ggtitle("Conditionall")
  ggsave(paste0(resdir,'/pareto_condNwSize_errorRelativeCost-errorLinkLength_color',param,'.png'),width=20,height=18,units='cm')
}
# -> saturation of params -> relaunch ?
# + also launch single obj








