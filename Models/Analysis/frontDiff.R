
# compute cumulated diff between all fronts

library(dplyr)

setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/NetLogo/netlogo6'))

resdirpref='exploration/20180722_1631_NSGA2_SYNTHETIC_TRCOST3_DISTRIBSD0.01'
outdir = 'explo/20180722_1631_NSGA2_SYNTHETIC_TRCOST3_DISTRIBSD0.01/'

latestgen = sort(as.numeric(sapply(strsplit(sapply(strsplit(list.files(resdirpref),'population',fixed=T),function(x){x[2]}),'.',fixed=T),function(x){x[1]})),decreasing=T)[1]

currentres <- as.tbl(read.csv(paste0(resdirpref,'/population1.csv')))

minc=c();minw=c()
frontsize=c();frontdiff=c()
for(generation in 2:latestgen){
  prevres = currentres
  currentres <- as.tbl(read.csv(paste0(resdirpref,'/population',generation,'.csv')))
  
  minc=append(minc,min(currentres$relativeCost));minw=append(minw,min(currentres$totalWaste))
  frontsize=append(frontsize,nrow(currentres))
  
  frontdiff =append(frontdiff, sum(apply(currentres[,c("totalWaste","relativeCost")],1,function(r){
    sum(sqrt(rowSums((matrix(rep(r,nrow(prevres)),nrow = nrow(prevres),byrow=T) - prevres[,c("totalWaste","relativeCost")])^2)))
  }))/(nrow(currentres)*nrow(prevres)))
}

write.csv(data.frame(minc,minw,frontsize,frontdiff),file=paste0(outdir,'frontstats.csv'))


####
res = read.csv(paste0(outdir,'frontstats.csv'))

g=ggplot(res)
g+geom_line(aes(x=X,y=frontsize))

g=ggplot(res)
g+geom_line(aes(x=X,y=c(0,diff(res$frontdiff))))

# quite noisy.
# -> take gen 40000.


