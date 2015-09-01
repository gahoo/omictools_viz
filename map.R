library(leaflet)
library(XML)
library(RISmed)
library(ggmap)

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
         nchar(pid) >= 7 ) %>%
  "$"('pid') %>%
  unique

fetch<-EUtilsGet(pids)

# length(pids)
# 
# pids<-lapply(0:17, function(x){
#   start<-x*500 + 1
#   end<-(x+1)*500
#   pid<-pids[start:end]
#   idx<-!is.na(pid)
#   pid[idx]
# })
# 
# fetch<-llply(pids, function(pid){
#   EUtilsGet(pid)
#   Sys.sleep(10)
#   }, .progress='text')

#save(fetch, address_pubmed_df, file='PubMed.RData')
load('PubMed.RData')

address_pubmed_df<-data.frame(pid = PMID(fetch),
                           affiliation = Affiliation(fetch),
                           cited = Cited(fetch),
                           stringsAsFactors = F) %>%
  merge(address_df, by='pid', all.y=T) %>%
  mutate(na.idx = is.na(affiliation),
         Created_at = as.character(Created_at)) %>%
  within(affiliation[na.idx] <- Created_at[na.idx]) %>%
  select(id, pid, affiliation, cited)

na.idx<-is.na(address_pubmed_df$affiliation)
addresses<-gsub('\\..*$','',address_pubmed_df$affiliation[!na.idx])

address_lat_lng<-geocode(addresses, output='more')

getGeo<-function(addresses){
  llply(addresses, function(addr){
    geocode(URLencode(addr))
    Sys.sleep(0.2)
  },.progress='text')
}


addresses<-lapply(0:17, function(x){
  start<-x*500 + 1
  end<-(x+1)*500
  addresses<-addresses[start:end]
  idx<-!is.na(addresses)
  addresses[idx]
})

addresses_lat_lng<-llply(addresses, function(addr){
  Sys.sleep(20)
  getGeo(addr)
  },.progress='text')


names(addresses_lat_lng)<-address_pubmed_df$id[!na.idx]

addr_lat_lng<-ldply(lapply(addresses_lat_lng, as.data.frame), .id='id') %>%
  filter(!is.na(lat)) %>%
  merge(address_pubmed_df, by='id')

leaflet(addr_lat_lng) %>%
  setView(lng=124, lat=26, zoom=1) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat,
                   radius = ~sqrt(cited),
                   popup = ~id,
                   stroke = F)
