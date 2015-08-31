library(leaflet)
library(RISmed)

load('omictools.RData')
geocode<-function(address){
  url = paste('http://maps.google.com/maps/api/geocode/xml?address=', address,'&sensor=false',sep='')
  doc = xmlTreeParse(url) 
  root = xmlRoot(doc) 
  list(
    lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]),
    lng = xmlValue(root[['result']][['geometry']][['location']][['lng']])
    )
}

geocode('china')

address_df<-software_df %>%
  mutate(id = gsub('^.*-', '', gsub('.html', '', omictools_link)),
         pid = gsub('.*/', '', PubMed) ) %>%
  select(id, pid, Created_at)


pids<-address_df %>%
  filter(!is.na(pid),
         grepl('^[0-9]+$', pid),
         nchar(pid) >= 6 ) %>%
  "$"('pid') %>%
  unique

results<-EUtilsGet(pids[1:10])
pubmed_address<-data.frame(pid = PMID(results),
                           affiliation = Affiliation(results),
                           stringsAsFactors = F)

addresses<-gsub('\\..*$','',pubmed_address$affiliation)

addresses_lat_lng<-llply(addresses, function(addr){
  geocode(URLencode(addr))
},.progress='text')

names(addresses_lat_lng)<-pubmed_address$pid

addr<-ldply(lapply(addresses_lat_lng, as.data.frame), .id='pid') %>%
  filter(!is.na(lat))

leaflet(addr) %>%
  setView(lng=124, lat=26, zoom=1) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, popup = ~pid,
                   stroke = F)



addresses2<-address_df %>%
  filter(is.na(pid),
         !is.na(Created_at))

addresses<-gsub('\\..*$','',addresses2$Created_at)

addresses_lat_lng2<-llply(addresses, function(addr){
  geocode(URLencode(addr))
},.progress='text')

names(addresses_lat_lng2)<-addresses2$id

addr2<-ldply(lapply(addresses_lat_lng2, as.data.frame), .id='id') %>%
  filter(!is.na(lat))

leaflet(addr2) %>%
  setView(lng=124, lat=26, zoom=1) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, popup = ~id,
                   stroke = F)
