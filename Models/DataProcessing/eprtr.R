
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)

transfer <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_WASTETRANSFER.csv'),fileEncoding="latin1"))

#unique(as.character(transfer$WasteHandlerPartyAddressCity))
codes = unique(paste0(as.character(transfer$WasteHandlerPartyAddressCountryCode)," ",as.character(transfer$WasteHandlerPartyAddressPostalCode)))

codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]

library(nominatim)

#osm_geocode(codes[1:10],key='GmFUKXAg4vykLxJOx3Pzt7KGJXKuKG9b')

#n = length(codes)
n=2

key=as.character(read.csv('apikey',header=F)[1,1])

names=c();lon=c();lat=c();
inds=c()
for(i in 1:n){
  show(paste0(i,"/",n))
  searchres = osm_search(codes[i],key=key)
  show(searchres)
  names=append(names,searchres$display_name)
  lon=append(lon,searchres$lon)
  lat=append(lat,searchres$lat)
  inds=append(inds,i)
  Sys.sleep(2)
}

#res = data.frame(idrow = 1:n,code = codes[1:n],name = names,lon = lon,lat = lat)
save(inds,names,lon,lat,file='geocode_wastehandler.RData')

