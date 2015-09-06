library(plyr)
library(dplyr)
library(ggmap)
library(tidyr)
library(stringr)

load('PubMed.RData')
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


load('omictools_tree.RData')

cid2sid<-function(cid){
  child.ids<-subset(catalog_tree_df, parent.id == cid)$id
  if(length(child.ids) == 0){
    subset(software_tree_df, parent.id == cid)$id
  }else{
    unique(unlist(lapply(child.ids, cid2sid)))
  }
}
