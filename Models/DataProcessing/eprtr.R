
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)
library(cartography)
library(ggplot2)


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
load('geocode_wastehandler.RData')
transfer_conso = left_join(transfer,data.frame(code=codes_transfer[1:length(lat)],lon=lon,lat=lat))
transfer_conso=transfer_conso[!is.na(transfer_conso$lon),]

#load('geocode_facilities.RData')
load('geocode_facilities_2.RData')
facilities_conso = left_join(facilities,data.frame(code=codes[inds],lon=lon,lat=lat))
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

g=ggplot(nuts[nuts$CNTR_CODE=='NL',])
g+geom_sf()+geom_sf(data=facilitiesf[sample(which(facilities_nuts$CNTR_CODE=='NL'),1000),"geometry"],col='red')+
  geom_sf(data=transfer_nuts %>% group_by('code') %>% filter(CNTR_CODE=='NL') %>% summarise(geometry = geometry[1]) ,col='blue')




#dcount = transfer_nuts %>%  group_by(NUTS_ID) %>% summarise(count=n())
#dcount = data.frame(nuts = dcount$NUTS_ID,dcount=dcount$count)

# this is not correct ! - should construct transfer matrix with transfer origin and destination coordinates
#all = left_join(ocount,dcount)
#all$count[is.na(all$count)] = 0
#all$dcount[is.na(all$dcount)] = 0
#all = all[!is.na(all$nuts),]
#all$total = all$count+all$dcount
#summary(all)
#all[all$total==max(all$total),]
#all[all$count==max(all$count),]

flows = as.data.frame(transfer_nuts[,c("FacilityReportID","Quantity","code","lon","lat","NUTS_ID")])
flows$geometry=NULL
names(flows)<-c("FacilityReportID","quantity","destination_code","destination_lon","destination_lat","destination_nuts")
origdf = as.data.frame(facilities_nuts[,c("FacilityReportID","ProductionVolumeQuantity","code","lon","lat","NUTS_ID")])
origdf$geometry=NULL
flows = left_join(flows,origdf)
names(flows)[8:11]<-c("origin_code","origin_lon","origin_lat","origin_nuts")
# 619,722 flows

flows = flows[!is.na(flows$origin_nuts)&!is.na(flows$destination_nuts),]
# 413621 flows

internalnutsflows = flows[flows$origin_nuts==flows$destination_nuts,]
# only 494 !
#  SHIT - pb on nuts overlay

#######
# test a flow map for the nuts with most transfers

# -> test a map for nuts ITC

mapped_nuts = 'ITC'
mapped_transfer

threshold = 0.99
fsflows = sflows[sflows$count>quantile(sflows$count,threshold),]
fsflows$id = paste0(fsflows$from_msoa,fsflows$to_msoa)
fsflows$linktype = rep("aggreg",nrow(fsflows))

links <- getLinkLayer(x=msoa,xid='MSOA11CD',fsflows,dfid = c("from_msoa","to_msoa"))
links$id = paste0(links$from_msoa,links$to_msoa)
links = left_join(links,fsflows[,c('id','count')])


png(filename = "Results/map_aggreg_flows.png",width = 40, height = 35, units = "cm",res=300)
plot(st_geometry(msoa), col = "grey13", border = "grey25", bg = "grey25", lwd = 0.5)
gradLinkTypoLayer(
  x = links,
  df = fsflows,
  var = "count", 
  breaks = c( min(fsflows$count),  quantile(fsflows$count,c(0.25)), median(fsflows$count), quantile(fsflows$count,c(0.995)), quantile(fsflows$count,c(0.9995))),
  lwd = c(0.5,1,2,5),
  var2 = "linktype"
) 
layoutLayer(title = "Aggregated flows between MSOA",
            frame = FALSE, col = "grey25", coltitle = "white",
            tabtitle = TRUE)
dev.off()





