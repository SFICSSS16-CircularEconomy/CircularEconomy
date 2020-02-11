
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)
library(cartography)
library(ggplot2)

resdir = paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Results/RealData/EPRTR/');dir.create(resdir)

transfer <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_WASTETRANSFER.csv')))#,fileEncoding='latin1'))
facilities <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_FACILITYREPORT.csv')))#,fileEncoding='latin1'))


#unique(as.character(transfer$WasteHandlerPartyAddressCity))

codes_transfer = unique(paste0(as.character(transfer$WasteHandlerPartyAddressCountryCode)," ",as.character(transfer$WasteHandlerPartyAddressPostalCode)))
codes_transfer = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]

codes = unique(paste0(as.character(facilities$CountryCode)," ",as.character(facilities$PostalCode)))
codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]


transfer$code = paste0(as.character(transfer$WasteHandlerPartyAddressCountryCode)," ",as.character(transfer$WasteHandlerPartyAddressPostalCode))
facilities$code = paste0(as.character(facilities$CountryCode)," ",as.character(facilities$PostalCode))

# relaunch as null not taken into account in transfer also
load('geocode_wastehandler_2.RData')
transfer_conso = left_join(transfer,data.frame(code=codes_transfer[inds],lon=lon,lat=lat))
transfer_conso=transfer_conso[!is.na(transfer_conso$lon),]

#load('geocode_facilities.RData')
load('geocode_facilities_2.RData');allinds=inds;alllat=lat;alllon=lon
load('geocode_facilities_3.RData');allinds=append(allinds,inds);alllat=append(alllat,lat);alllon=append(alllon,lon)
facilities_conso = left_join(facilities,data.frame(code=codes[allinds],lon=alllon,lat=alllat))
facilities_conso=facilities_conso[!is.na(facilities_conso$lon),]

#  add transfer volume -> transfer_conso$Quantity  // all are in tons


library(sf)

#nuts <- st_read('../../Data/ref-nuts-2016-60m.shp/NUTS_RG_60M_2016_4326_LEVL_1.shp/','NUTS_RG_60M_2016_4326_LEVL_1')
# should use projected nuts
nuts <- st_read('../../Data/ref-nuts-2016-60m.shp/NUTS_RG_60M_2016_3857_LEVL_1.shp/','NUTS_RG_60M_2016_3857_LEVL_1')

# overlay with nuts id

#facilitiespoints=list();for(i in 1:nrow(facilities_conso)){facilitiespoints[[i]]=st_point(as.matrix(facilities_conso[i,c("lon","lat")]))}
facilitiespoints=list();for(i in 1:nrow(facilities_conso)){facilitiespoints[[i]]=st_point(as.matrix(facilities_conso[i,c("Long","Lat")]))}
facilitiesf = st_sf(facilities_conso,geometry=facilitiespoints)
st_crs(facilitiesf) <- "+proj=longlat +datum=WGS84"
facilitiesf = facilitiesf %>% st_transform(st_crs(nuts))

# plot(nuts[nuts$CNTR_CODE=='DE',"geometry"])
# plot(facilitiesf[1:5000,"geometry"],col='red',add=T)

# overlay
facilities_nuts = st_join(facilitiesf, nuts, join = st_intersects)
# plot(facilitiesf[facilities_nuts$CNTR_CODE=='DE',"geometry"],col='red',add=T)

# FIXME facilities already have Long,Lat

#ocount = facilities_nuts %>% group_by(NUTS_ID) %>% summarise(count=n())
#ocount = data.frame( nuts = ocount$NUTS_ID,count = ocount$count)

# same with waste handler (destination)
transferspoints=list();for(i in 1:nrow(transfer_conso)){transferspoints[[i]]=st_point(as.matrix(transfer_conso[i,c("lon","lat")]))}
transfersf = st_sf(transfer_conso,geometry=transferspoints)
st_crs(transfersf) <- "+proj=longlat +datum=WGS84"
transfersf = transfersf %>% st_transform(st_crs(nuts))

transfer_nuts = st_join(transfersf, nuts, join =  st_intersects)

#g=ggplot(nuts[nuts$CNTR_CODE=='NL',])
#g+geom_sf()+geom_sf(data=facilitiesf[sample(which(facilities_nuts$CNTR_CODE=='NL'),1000),"geometry"],col='red')+
#  geom_sf(data=transfer_nuts %>% group_by('code') %>% filter(CNTR_CODE=='NL') %>% summarise(geometry = geometry[1]) ,col='blue')


# construct flow dataframe

flows = as.data.frame(transfer_nuts[,c("FacilityReportID","Quantity","WasteTreatmentName","code","lon","lat","NUTS_ID","CNTR_CODE")])
flows$geometry=NULL
names(flows)<-c("FacilityReportID","quantity","treatment","destination_code","destination_lon","destination_lat","destination_nuts","destination_country")
origdf = as.data.frame(facilities_nuts[,c("FacilityReportID","ProductionVolumeQuantity","code","lon","lat","NUTS_ID","CNTR_CODE")])
origdf$geometry=NULL
flows = left_join(flows,origdf)
names(flows)[10:14]<-c("origin_code","origin_lon","origin_lat","origin_nuts","origin_country")
# 617,344 flows

flows = flows[!is.na(flows$origin_nuts)&!is.na(flows$destination_nuts)&!is.na(flows$origin_country)&!is.na(flows$destination_country),]
# 566,615 flows

intracountryflows = flows[flows$origin_country==flows$destination_country,]
# 36019
intracountryflows = intracountryflows[nchar(intracountryflows$destination_code)>4&nchar(intracountryflows$origin_code)>4,]



intranutsflows = flows[flows$origin_nuts==flows$destination_nuts,]
#  6621

# filter on existing o/d code ?
intranutsflows = intranutsflows[nchar(intranutsflows$destination_code)>4&nchar(intranutsflows$origin_code)>4,]

nutscount = intranutsflows %>% group_by(origin_nuts) %>% summarize(count=n())
nutscount[nutscount$count==max(nutscount$count),]
# -> best is BE3 - with no dest code => ??? pb, geocoded the country only.
#NL3

countrycount = intracountryflows %>% group_by(origin_country) %>% summarize(count=n())
countrycount[countrycount$count==max(countrycount$count),]

areas = rbind(transfer_nuts[,c('code','NUTS_ID','geometry')],facilities_nuts[,c('code','NUTS_ID','geometry')])
areas=areas[!duplicated(areas),]


#######
# test a flow map for the nuts with most transfers

for(mapped in unique(nuts$CNTR_CODE)){
  show(mapped)
#mapped_nuts = 'ITC'
#mapped_nuts = 'BE3'
#mapped_nuts = 'IE0'
#mapped_nuts = 'NL3'
#mapped='BE'
#mapped='DE'
#mapflows = intranutsflows[intranutsflows$origin_nuts==mapped_nuts,]
mapflows = intracountryflows[intracountryflows$origin_country==mapped,]

if(nrow(mapflows)>10){

# filter flows - geocod errors ?
#st_bbox(c(xmin = min(nuts[nuts$CNTR_CODE==mapped,])), 
#          ymin = ifelse(is.na(ymin),  -90,  ymin), 
#          xmax = ifelse(is.na(xmax), +180, xmax), 
#          ymax = ifelse(is.na(ymax),  +90, ymax)), 
#        crs = st_crs(nuts)) %>% sf::st_as_sfc(.)

#threshold = 0.99
#mapflows = mapflows[mapflows$quantity>quantile(mapflows$quantity,threshold),]
#mapflows$linktype = rep("aggreg",nrow(mapflows))
mapflows$id = paste0(mapflows$origin_code,mapflows$destination_code)
mapflows$treatment=as.character(mapflows$treatment)

#currentareas = areas[areas$NUTS_ID==mapped_nuts&areas$code%in%c(mapflows$origin_code,mapflows$destination_code),]
currentareas = areas[areas$code%in%c(mapflows$origin_code,mapflows$destination_code),]
currentareas=currentareas[!duplicated(currentareas$code),]
currentareas <- currentareas[which(lengths(st_within(currentareas, nuts[nuts$CNTR_CODE==mapped,])) != 0), ]

# refilter flows
mapflows=mapflows[mapflows$origin_code%in%currentareas$code&mapflows$destination_code%in%currentareas$code,]

links <- getLinkLayer(x=currentareas,xid='code',df=mapflows,dfid = c("origin_code","destination_code"))

links$id = paste0(links$origin_code,links$destination_code)
links = left_join(links,mapflows[,c('id','quantity','treatment')])
links$treatment=as.character(links$treatment)

#png(filename = paste0(resdir,'intranutsflows_',mapped_nuts,'.png'),width = 40, height = 35, units = "cm",res=300)
png(filename = paste0(resdir,'intracountryflows_',mapped,'_osm.png'),width = 20, height = 18, units = "cm",res=300)

#plot(st_geometry(nuts[nuts$CNTR_CODE==mapped,]), col = "grey35", border = "grey55", bg = "grey55", lwd = 0.5)

osm <- getTiles(
  x = nuts[nuts$CNTR_CODE==mapped,], 
  type = "osm", 
  zoom = 11, 
  crop = TRUE
)
tilesLayer(x = osm)
plot(st_centroid(st_geometry(nuts[nuts$CNTR_CODE==mapped,])), col = NA, border = "grey", add=TRUE)

gradLinkTypoLayer(
  x = links,
  xid=c("origin_code","destination_code"),
  df = mapflows,
  dfid=c("origin_code","destination_code"),
  var = "quantity",
  breaks = c( min(mapflows$quantity),  quantile(mapflows$quantity,c(0.25)), median(mapflows$quantity), quantile(mapflows$quantity,c(0.995)), quantile(mapflows$quantity,c(0.9995))),
  lwd = c(1,2,5,10)/2,
  var2 = "treatment"
)
layoutLayer(title = paste0("Waste transfer flows in ",mapped),
            frame = FALSE, col = "grey25", coltitle = "white",
            tabtitle = TRUE)
dev.off()

}
}


#######
## Export NL data for calibration

mapped='NL'
mapflows = intracountryflows[intracountryflows$origin_country==mapped,]
mapflows$id = paste0(mapflows$origin_code,mapflows$destination_code)
mapflows$treatment=as.character(mapflows$treatment)

currentareas = areas[areas$code%in%c(mapflows$origin_code,mapflows$destination_code),]
currentareas=currentareas[!duplicated(currentareas$code),] # no duplicated
currentareas <- currentareas[which(lengths(st_within(currentareas, nuts[nuts$CNTR_CODE==mapped,])) != 0), ]

mapflows=mapflows[mapflows$origin_code%in%currentareas$code&mapflows$destination_code%in%currentareas$code,]
mapflows = mapflows[mapflows$origin_code!=mapflows$destination_code,] # ! ok at scale of country ?

# currentareas$code is unique -> can become numerical id
areasids = 1:length(currentareas$code)
names(areasids)<-currentareas$code

rawxcor = sapply(st_geometry(currentareas),function(g){g[[1]]})
rawycor = sapply(st_geometry(currentareas),function(g){g[[2]]})

xcor = (rawxcor - min(rawxcor))/(max(rawxcor)-min(rawxcor))
ycor = (rawycor - min(rawycor))/(max(rawycor)-min(rawycor))

exportdir = '../Netlogo/netlogo6/setup/'

# companies
write.table(data.frame(areasids,xcor,ycor),file=paste0(exportdir,'companies.csv'),sep=";",col.names = F,row.names = F,quote=F)

# flows
# ! pb if only NAs in production volumes -> assume all the same and rescale with a log
# kind of log-normal
# goes with scaling law for company size.
# rq: in the model assumed same size - do the same here - see ability of model to reproduce such a distrib from
# uniform company size

write.table(data.frame(areasids[mapflows$origin_code],areasids[mapflows$destination_code],mapflows$quantity),file=paste0(exportdir,'links.csv'),sep=";",col.names = F,row.names = F,quote=F)







