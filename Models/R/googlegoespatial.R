#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type, formatted address
#### Notice ther is a limit of 2,500 calls per day (cheap ass google)
library(RCurl)
library(RJSONIO)
#### This script uses at lot of other packages as well
library(plyr)
library(ggmap)
library(sp)
library(sfsmisc)
library(kernlab)
library(plot3D)
library(MASS)
library(mvtnorm)
library(igraph)
library(dplyr)

###Sourcefile with functions###
setwd("C:/Users/Joris/Google Drive/ABM CE")
source("ABMspatialdistribution.R")


url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

##Test with a single address
#RGS90 <- geoCode("RGS90, Kalundborg, Denmark")

#Test with six companies from Kalundborg
Names<- c("Novo Nordisk, Kalundborg, Denmark", "Novozymes, Kalundborg, Denmark", 
          "Gyproc, Kalundborg, Denmark", "Dong Energy, Kalundborg, Denmark", 
          "RGS 90, Kalundborg, Denmark", "Statoil, Kalundborg, Denmark", 
          "Kara/Novoren, Kalundborg, Denmark")
locations  <- ldply(Names, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)

coords <- cbind(Longitude = as.numeric(locations$lon), Latitude = as.numeric(locations$lat))
coords<-as.data.frame(coords)
map <- qmap('Kalundborg', zoom =12)
map
mapp<-map + geom_point(data= coords, aes(x = Longitude, y = Latitude), color="red", size=3, alpha=0.5) 
mapp

Nb.companies <- 7 #number companies
Threshold <- 0.1 #trade treshold (minimum intergrated density)
lengthcor <- 0#Length correlation function only works with the toymodel, not with coordinates. Will have to rewrite that.
dist.decay <- 0.01#distanced[dd]#Decay in interaction probablity
Numproducts <- 5 #Variance in in/outputdistribution (fixed)
#gridSize <- 100 #Size of the grid (fixed)
#NCities <- 5 #Number o cities (Somehow a minimum of 8, otherwise doesn't work), also (fixed)
#alpha <- 0.7  #Powerlaw city distribution
#Alpha <- 0.7
iter <- 10

geo.distances <- as.matrix(dist(coords))
interaction.proba <- exp(-(geo.distances/dist.decay))
diag(interaction.proba) = 0
interaction.proba <- cbind(interaction.proba, (1-rowSums(interaction.proba)))
interaction.proba[interaction.proba <0] <- 0 #Limiting probabilities to 0 and 1, because, well, you know
interaction.proba[interaction.proba >1] <- 1

Inputdist <- runif(Nb.companies,1, Nb.companies)
Outputdis <- runif(Nb.companies,1, Nb.companies)
SDproduct <-1

#Matching companies based on interaction
Dataframe <- as.data.frame(cbind(Inputdist,Outputdis))
wasteflow <- matrix(c(rep(0,Nb.companies*iter)), ncol=iter)
adjemat <- matrix(c(rep(0,Nb.companies*Nb.companies)), ncol=Nb.companies)
for(t in 1:iter){
  for(i in 1:Nb.companies){
    if(sum(interaction.proba[i,])== 0){interpartner <- (Nb.companies+1)} else{
      interpartner <- sample(c(1:(Nb.companies+1)),1,prob=interaction.proba[i,])}
    company <- i
    if(interpartner != (Nb.companies+1)){
      wasteflow[i,t]<-jointdis(company, interpartner, SDproduct) #Function intergrating densities
      interaction.proba[i,interpartner] <-0
    }
    else{wasteflow[i,t] <- 0}
    if(wasteflow[i,t]<Threshold){wasteflow[i,t] <- 0}
    if(wasteflow[i,t] > 0){
      #mapp <-mapp + geom_segment(data=coords, mapping=aes(x=coords$Longitude[i], y=coords$Latitude[i], xend=coords$Longitude[interpartner], yend=coords$Latitude[interpartner]), arrow=arrow()) 
      #mapp
      adjemat[i,interpartner] <- 1
      interaction.proba[i,] <- 0
    }
  }
}
g <- graph.adjacency(adjemat)
g <- get.edgelist(g)
mapp + geom_segment(data=coords, mapping=aes(x=coords$Longitude[g[,1]], y=coords$Latitude[g[,1]], xend=coords$Longitude[g[,2]], yend=coords$Latitude[g[,2]]), arrow=arrow()) 
TotalWast <-sum(wasteflow[,])
#ActLenCor <-productdist[[2]]
TotalWast
#ActLenCor
