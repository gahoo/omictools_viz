library(leaflet)
library(XML)
library(RISmed)
library(ggmap)
library(tidyr)
library(stringr)
library(reshape2)

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

semicolon_cnt<-address_pubmed_df$affiliation %>%
  str_count(';') %>%
  max(na.rm=T)

address_gather_pubmed_df<-address_pubmed_df %>%
  separate(affiliation, paste0('address.',1:semicolon_cnt),
           sep='; and |; |;', extra = 'merge') %>%
  gather(address_num, address, starts_with('address.') ) %>%
  mutate(address = gsub('\\. *.*@.*$|\\.$', '',address)) %>% #deal with extra email
  filter(!is.na(address))
  
addresses<-address_gather_pubmed_df$address
address_lat_lng<-list()
llply(1:nrow(address_gather_pubmed_df), function(i){
  address_lat_lng[[i]]<<-geocode(addresses[i], source = 'google',
                                 output='all', messaging = F)
  if(geocodeQueryCheck()<10){
    break
  }
  Sys.sleep(0.2)
}, .progress = 'text')

save(address_lat_lng, file='address_lat_lng.RData')

i
geocode(addresses[3], source = 'google', output='more')

getGeo<-function(addresses){
  llply(addresses, function(addr){
    geocode(URLencode(addr))
    Sys.sleep(0.2)
  },.progress='text')
}


addresses<-lapply(0:3, function(x){
  start<-x*2500 + 1
  end<-(x+1)*2500
  addresses<-addresses[start:end]
  idx<-!is.na(addresses)
  addresses[idx]
})

addresses_lat_lng<-llply(addresses, function(addr){
  Sys.sleep(20)
  geocode(addr)
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
