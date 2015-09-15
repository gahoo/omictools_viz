library(plyr)
library(dplyr)

load('omictools.RData')
load('omictools_tree.RData')
load('address_lat_lng.RData')
load('address_pubmed.RData')

build_a<-function(x){
  ifelse(is.na(x), NA, sprintf('<a href="%s" target="_blank">%s</a>', x, x))
}


software_df<-software_df %>%
  mutate(id = gsub('^.*-', '', gsub('.html', '', omictools_link)),
         related = gsub(';', '; ', related),
         catalog = gsub(';', ' > ', catalog),
         img = sprintf("<img src='%s'/>", img),
         link = build_a(link),
         PubMed = build_a(PubMed),
         URL = build_a(URL)
  ) %>%
  left_join(
    address_gather_pubmed_df[c('id', 'cited')]
    %>% unique, by='id')

catalog_desc<-rbind.fill(catalog_folder_df,
                         catalog_software_df) %>%
  mutate( id = gsub('^.*-', '', gsub('-p1.html', '', parent_href)) ) %>%
  select(id,
         alias = parent_alias,
         description = parent_desc) %>%
  unique

tree_df<-rbind(catalog_tree_df, software_tree_df)

name_links<-rbind.fill(catalog_folder_df,
                       catalog_software_df) %>%
  select(href = parent_href,
         name = parent) %>%
  rbind(catalog_folder_df[c('href', 'name')],
        catalog_software_df[c('href', 'name')])  %>%
  unique %>%
  mutate(id = gsub('^.*-', '', gsub('-p1.html|.html', '', href)),
         name = sprintf('<a href="http://omictools.com/%s" target="_blank">%s</a>', href, name) ) %>%
  select(-href)

name_links<-name_links[!duplicated(name_links$id),]

NULLtoNA <- function(x) {
  ifelse(is.null(x), NA, x)
}


addr_list2df<-function(gc){
  if(is.na(gc) || gc$status == 'ZERO_RESULTS' || gc$status == 'OVER_QUERY_LIMIT'){
    return(NULL)
  }
  #codes from ggmap::geocode
  with(gc$results[[1]], {
    data.frame(lng = NULLtoNA(geometry$location$lng),
               lat = NULLtoNA(geometry$location$lat), 
               type = tolower(NULLtoNA(types[1])),
               loctype = tolower(NULLtoNA(geometry$location_type)), 
               #address = location,
               address = formatted_address,
               north = NULLtoNA(geometry$viewport$northeast$lat), 
               south = NULLtoNA(geometry$viewport$southwest$lat), 
               east = NULLtoNA(geometry$viewport$northeast$lng), 
               west = NULLtoNA(geometry$viewport$southwest$lng))
  })
  
}

names(address_lat_lng)<-address_gather_pubmed_df$id
address_lat_lng_df<-lapply(address_lat_lng, addr_list2df) %>%
  ldply(.id='id') %>%
  merge(address_gather_pubmed_df, by='id') %>%
  select(id, pid, lat, lng, cited) %>%
  mutate(cited = ifelse(is.na(cited),0,cited)) %>%
  merge(software_tree_df[c('id', 'name')], by='id', all.x=T) %>%
  unique



save(omictools, address_lat_lng_df, name_links, tree_df, catalog_desc,
     software_df, catalog_tree_df, software_tree_df,
     file='viz/viz.RData')