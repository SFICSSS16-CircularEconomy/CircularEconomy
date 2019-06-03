
setwd(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Models/DataProcessing'))

library(dplyr)


transfer <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_WASTETRANSFER.csv'),fileEncoding='latin1'))
facilities <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/CircularEconomy/Data/EPRTR/E-PRTR_database_v16_csv/dbo.PUBLISH_FACILITYREPORT.csv')))#,fileEncoding='latin1'))


#unique(as.character(transfer$WasteHandlerPartyAddressCity))

#codes = unique(paste0(as.character(transfer$WasteHandlerPartyAddressCountryCode)," ",as.character(transfer$WasteHandlerPartyAddressPostalCode)))
#codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]

codes = unique(paste0(as.character(facilities$CountryCode)," ",as.character(facilities$PostalCode)))
codes = codes[nchar(codes) > 3&codes!="CONFIDENTIAL CONFIDENTIAL"]




load('geocode_facilities.RData')




