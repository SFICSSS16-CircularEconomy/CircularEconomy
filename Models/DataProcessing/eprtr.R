
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)


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

load('geocode_facilities.RData')
facilities_conso = left_join(facilities,data.frame(code=codes[1:length(lat)],lon=lon,lat=lat))
facilities_conso=facilities_conso[!is.na(facilities_conso$lon),]

# TODO add transfer volume


library(sf)

#nuts <- st_read('../../Data/ref-nuts-2016-60m.shp/NUTS_RG_60M_2016_4326_LEVL_1.shp/','NUTS_RG_60M_2016_4326_LEVL_1')
# should use projected nuts
nuts <- st_read('../../Data/ref-nuts-2016-60m.shp/NUTS_RG_60M_2016_3857_LEVL_1.shp/','NUTS_RG_60M_2016_3857_LEVL_1')

# overlay with nuts id

facilitiespoints=list();for(i in 1:nrow(facilities_conso)){facilitiespoints[[i]]=st_point(as.matrix(facilities_conso[i,c("lon","lat")]))}
facilitiesf = st_sf(facilities_conso,geometry=facilitiespoints)
st_crs(facilitiesf) <- "+proj=longlat +datum=WGS84"
facilitiesf = facilitiesf %>% st_transform(st_crs(nuts))

# overlay
facilities_nuts = st_join(facilitiesf, nuts, join = st_intersects)

ocount = facilities_nuts %>% group_by(NUTS_ID) %>% summarise(count=n())
ocount = data.frame( nuts = ocount$NUTS_ID,count = ocount$count)

# same with waste handler (destination)
transferspoints=list();for(i in 1:nrow(transfer_conso)){transferspoints[[i]]=st_point(as.matrix(transfer_conso[i,c("lon","lat")]))}
transfersf = st_sf(transfer_conso,geometry=transferspoints)
st_crs(transfersf) <- "+proj=longlat +datum=WGS84"
transfersf = transfersf %>% st_transform(st_crs(nuts))

transfer_nuts = st_join(transfersf, nuts, join =  st_intersects)

dcount = transfer_nuts %>%  group_by(NUTS_ID) %>% summarise(count=n())
dcount = data.frame(nuts = dcount$NUTS_ID,dcount=dcount$count)

all = left_join(ocount,dcount)
all$count[is.na(all$count)] = 0
all$dcount[is.na(all$dcount)] = 0
all = all[!is.na(all$nuts),]

all$total = all$count+all$dcount
summary(all)

all[all$total==max(all$total),]

