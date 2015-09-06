library(RISmed)
library(tidyr)
library(stringr)

load('omictools.RData')

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

save(address_gather_pubmed_df, file='address_pubmed.RData')
