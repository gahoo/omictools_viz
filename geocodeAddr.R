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
llply(start:nrow(address_gather_pubmed_df), function(i){
  address_lat_lng[[i]]<<-geocode(addresses[i], source = 'google',
                                 output='all', messaging = F)
  if(geocodeQueryCheck()<200){
    break
  }
  Sys.sleep(0.2)
}, .progress = 'text')

save(address_lat_lng, file='address_lat_lng.RData')
