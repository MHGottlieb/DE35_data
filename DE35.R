# Dependencies ------------------------------------------------------------

  library(RCurl)
  library(jsonlite)
  library(dplyr)
  library(readxl)
 
# Kommunekoder og -regioner fra dst.dk gøres til elmarked-priszoner -------

  # importing municipality and region codes
  # Source: https://www.dst.dk/da/Statistik/dokumentation/nomenklaturer/nuts
  NUTS <- read_excel("Korrespondancetabel-mellem-kommuner-foer-og-efter-kommunalreformen-i-2007.xlsx")[,5:8]
  NUTS <- NUTS[!duplicated(NUTS),]
  NUTS$MunicipalityNo <- as.character(NUTS$NUTS_KODE)
  
  #DK regions to Energinet Price Zones
  NUTS <- merge(NUTS, list(REGION_KODE=c("081", "082", "083", "084", "085"), 
                           PriceZone=c("DK1", "DK1", "DK1", "DK2", "DK2")))  
  
# Getting data via energidataservice.dk and transforming a bit ------------

  url <- "https://api.energidataservice.dk/dataset/" # API access point

  # Consumption data
  request <- paste0(url, "ConsumptionDE35Hour") # Consumption per DE35 Industry Code per Hour
  limit <- fromJSON(getURL(request))$total # no. of available observations
  cons <- fromJSON(getURL(paste0(request, "?limit=", limit))) # (getting app. 70MB, 15 min.)
  cons <- cons$records
  cons <- cons[-1] # no need for this
  cons$HourDK <- as.POSIXct(cons$HourDK, format="%Y-%m-%dT%H:%M:%S")
  
  # Elspot data since 2020-01-01
  start <- format(min(cons$HourDK), "%Y-%m-%d")
  end <- format(max(cons$HourDK+86400), "%Y-%m-%d") #adding a day
  filter <- '{"PriceArea":"DK1,DK2"}'
  request <- paste0(url, "Elspotprices", "?start=", start, "&end=", end, "&filter=", filter) # getting data
  price <- fromJSON(getURL(request)) # (getting app. 6 MB, 1 min.)
  price <- price$records
  price <- price[-c(1,5)] # we dont need these 
  price$HourDK <- as.POSIXct(price$HourDK, format="%Y-%m-%dT%H:%M:%S")
  
  # Long names for DE35 data
  request <- paste0(url, "Industrycodes_DE35") # Long names
  DE35 <- fromJSON(getURL(request)) # downloading data (38 observations total)
  DE35 <- DE35$records
  
  # Measuring points per DE35 consumer
  request <- paste0(url, "ConsumptionpermunicipalityDE35", "?start=2020-01", "&end=2022-09") # measuring points per municipality (August 2022)
  points <- fromJSON(getURL(request)) # downloading data
  points <- points$records
  points <- inner_join(NUTS, points, by = "MunicipalityNo")
  points <- aggregate(points$MeasurementPoints, 
                      by=list(Industrycode_DE35=points$Industrycode_DE35, 
                              Month=points$Month,
                              PriceZone=points$PriceZone
                              ), FUN=sum)
  points$Month <- as.POSIXct(points$Month)
  names(points) <- c("ConsumerType_DE35", "Month", "PriceZone", "Points")
  points <- inner_join(points, DE35[1:3], by = "ConsumerType_DE35")
  points$year <- format(points$Month, "%Y")
  points$month <- format(points$Month, "%m")
  
# Reordering data and cleaning up a bit -----------------------------------
  
  cons <- merge(cons, DE35[,c(1:2)], by="ConsumerType_DE35") # adding consumer names
  cons <- inner_join(price, cons, by=c("HourDK", "PriceArea")) # merging with prices. Note: 304 Extra rows bc of daylight saving (I hate this!)
  cons$year <- format(cons$HourDK, "%Y")
  cons$month <- format(cons$HourDK, "%m")
  cons$day <- format(cons$HourDK, "%d")
  cons$weekday <- format(cons$HourDK, "%A")
  cons$hour <- format(cons$HourDK, "%H")
  
  
  
  
  
  
  
  
  #MERE HER#
  
  
  
  
  
  
  abe <- inner_join(points, cons, by=)
  cons$AvgCons <- cons$TotalCon*1000 / cons$Points
  
  cons$DE35_DA <- as.factor(cons$DE35_DA)
  cons$PriceArea <- as.factor(cons$PriceArea )
  cons$ConsumerType_DE35 <- as.numeric(cons$ConsumerType_DE35)
  
  
  cons$year <- format(cons$HourDK, "%Y")
  cons$month <- format(cons$HourDK, "%m")
  cons$day <- format(cons$HourDK, "%d")
  cons$weekday <- format(cons$HourDK, "%A")
  cons$hour <- format(cons$HourDK, "%H")
  
  #removing duplicates
  cons <- cons[!duplicated(cons),]
  
  # Removing the stuff we don't need anymore
  remove(points, price, end, filter, limit, request, start, url)
  
  # Saving all data
  saveRDS(cons, file = "DE35_consumption_and_prices.rds")

  
  
  
  
  
  
  
  
  
  
  
# Analysing data ----------------------------------------------------------


  
  # how much energy does each category use?
  
  # ConsumerType_DE35   DE35_DA
  # 111                 Lejligheder m.v. uden elvarme
  # 112                 Lejligheder m.v. med elvarme
  # 119                 Lejligheder m.v. fællesforbrug
  # 121                 Parcel-, række- m.v. huse uden elvarme
  # 122                 Parcel-, række- m.v. huse med elvarme
  # 123                 Parcel-, række- m.v. huse med varmepumpe
  
  library(ggplot2)
  library(ggridges)
  
  p1 <- ggplot(cons[cons$ConsumerType_DE35%in%c(111,112,121:123),], aes(x=AvgCons, y=DE35_DA.x,
                                                         group=ConsumerType_DE35)) +
          geom_boxplot(outlier.shape=1, outlier.color=12) +
          theme_gray() +
          xlim(0, 3) +
          labs(title="Elforbrug efter forbrugskategori",
               caption="kilde: Energinet/Energidataservice",
               x = "Elforbrug per husstand, kWh/t",
               y = "",
               )
  
  p2 <- ggplot(cons[cons$ConsumerType_DE35%in%c(111,112,121:123),], aes(x=AvgCons, y=DE35_DA.x,
                                                                        group=ConsumerType_DE35)) +
          geom_density_ridges()
          theme_gray() +
          xlab("Elforbrug per husstand, kWh/t")
    
    

  # cons <- aggregate(TotalCon ~ HourDK + ConsumerType_DE35, data=cons, FUN=sum) # if we want to collapse price zones
 

  