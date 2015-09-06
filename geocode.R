library(ggmap)

load('address_pubmed.RData')
addresses<-address_gather_pubmed_df$address
address_lat_lng<-list()
load('address_lat_lng.RData')
start<-length(address_lat_lng)+1
rest_idx<-start:nrow(address_gather_pubmed_df)

geocodeIdx<-function(idx){
  llply(idx, function(i){
    address_lat_lng[[i]]<<-geocode(addresses[i], source = 'google',
                                   output='all', messaging = F)
    #if(address_lat_lng[[i]]$status == 'OVER_QUERY_LIMIT'){
    if(geocodeQueryCheck()<200){
      break
    }
    Sys.sleep(0.2)
  }, .progress = 'text')
}

#geocodeIdx(rest_idx)
#save(address_lat_lng, file='address_lat_lng.RData')

status<-sapply(address_lat_lng, function(x){unlist(x['status'])})
retry_idx<-which(status == 'OVER_QUERY_LIMIT' |is.na(status))
zero_idx<-which(status == 'ZERO_RESULTS')

geocodeIdx(retry_idx)

#save(address_lat_lng, file='address_lat_lng.RData')