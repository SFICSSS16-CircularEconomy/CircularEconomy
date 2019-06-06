
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)


#transfer <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_WASTETRANSFER.csv'),fileEncoding='latin1'))
#activities <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_ACTIVITY.csv')))
facilities <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_FACILITYREPORT.csv'),fileEncoding='latin1'))


#unique(as.character(transfer$WasteHandlerPartyAddressCity))

#codes = unique(paste0(as.character(transfer$WasteHandlerPartyAddressCountryCode)," ",as.character(transfer$WasteHandlerPartyAddressPostalCode)))
#codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]

codes = unique(paste0(as.character(facilities$CountryCode)," ",as.character(facilities$PostalCode)))
codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]


library(nominatim)

n = length(codes)
#n=10
toget = 1:n

load('geocode_facilities_2.RData')
previnds = inds
toget = setdiff(toget,previnds)

key=as.character(read.csv('apikey',header=F)[1,1])

names=c();lon=c();lat=c();
inds=c()
for(i in toget){
  show(paste0(i,"/",n))
  searchres = osm_search(codes[i],key=key)
  show(codes[i])
  show(searchres)
  if(!is.null(searchres$display_name)){
    names=append(names,searchres$display_name)
    lon=append(lon,searchres$lon)
    lat=append(lat,searchres$lat)
    inds=append(inds,i)
  }
  Sys.sleep(2)
}

#res = data.frame(idrow = 1:n,code = codes[1:n],name = names,lon = lon,lat = lat)
#save(inds,names,lon,lat,file='geocode_wastehandler.RData')
save(inds,names,lon,lat,file='geocode_facilities_3.RData')

show(inds)
show(names)
show(lon)
#load('geocode_facilities.RData')




