library(jsonlite)
library(d3treeR)
library(plyr)
library(dplyr)

load('omictools.RData')
malformed_links<-catalog_folder_df %>%
  filter(grepl('index.html', href) |
         grepl('index.html', parent_href))
malformed_links

#manually fix malformed_links
malform_idx<-grep('index', catalog_folder_df$href)
catalog_folder_df[malform_idx, ]$href<-c('rna-structures-c933-p1.html',
                                         'genomic-variation-c214-p1.html',
                                         'genomic-variation-c214-p1.html',
                                         'alternative-splicing-c436-p1.html')


catalog_tree_df<-catalog_folder_df %>%
  select(parent.id = parent_href,
         parent.name = parent,
         id = href,
         name = name,
         size = count) %>%
  mutate(parent.id = gsub('^.*-', '', gsub('-p1.html', '', parent.id)),
         id = gsub('^.*-', '', gsub('-p1.html', '', id)) ) %>%
  unique

addRoots<-function(tree_df, root_name){
  roots.id<-with(tree_df, setdiff(parent.id, id))

  roots_size<-tree_df %>%
    filter(parent.id %in% roots.id) %>%
    group_by(parent.id) %>%
    summarise(size=sum(size)) %>%
    rename(id = parent.id)
  
  roots_df<-tree_df %>%
    filter(parent.id %in% roots.id) %>%
    select(id = parent.id,
           name = parent.name) %>%
    unique %>%
    merge(roots_size, by='id') %>%
    mutate(parent.id = 'c0',
           parent.name = root_name) %>%
    select(parent.id, parent.name, id, name, size)
  
  rbind(roots_df, tree_df)
}


id2name<-function(tree_df){
  tree_df %>%
    select(id = parent.id,
           name = parent.name) %>%
    rbind(tree_df[,c('id', 'name')]) %>%
    unique %>%
    dlply(.variables = 'id', .fun=function(x){x$name})
}

id2any<-function(tree_df, column){
  if(is.null(tree_df[[column]])){
    stop(column, " not exists")
  }
  
  tree_df %>%
    select_('id', column) %>%
    unique %>%
    dlply(.variables = 'id', .fun=function(x){x[[column]]})
}

buildNestedList<-function(root_df, env){
  getChildren<-function(child.id){
    leaf_df<-subset(tree_df, parent.id==child.id)
    if(nrow(leaf_df) == 0){
      list(name=id_name[[child.id]],
           size=id_size[[child.id]])
    }else{
      buildNestedList(leaf_df, env)
    }
  }
  
  node<-list()
  roots.id<-with(root_df, setdiff(parent.id, id))
  for(root.id in roots.id){
    children.ids<-subset(root_df, parent.id==root.id)$id
    node[['name']]<-id_name[[root.id]]
    node[['children']]<-lapply(children.ids, getChildren)
  }
  node
}

df2NestedList<-function(tree_df, root_name='root', vSize='size'){    
  tree_df<-addRoots(tree_df, root_name)
  id_name<-id2name(tree_df)
  id_size<-id2any(tree_df, vSize)
  
  environment(buildNestedList) <- environment()
  buildNestedList(tree_df, env=env)
}

#load manually fixed roots
catalog_tree_df<-read.table('roots.txt', header=T, sep='\t') %>%
  rbind(catalog_tree_df)

omictools<-df2NestedList(catalog_tree_df, 'omictools')

d3tree2(
  toJSON(omictools, auto_unbox = T),
  celltext = "name"
  #width = 1200
)

############# catalog_software_df
write.utf8 <- function(filename, msg="") {
  con <- file(filename, "w")
  tryCatch({
    cat(iconv(msg, to="UTF-8"), file=con, sep="\n")
  },
  finally = {
    close(con)
  })
}


malformed_links<-catalog_software_df %>%
  filter(grepl('index.html', href) |
           grepl('index.html', parent_href))
malformed_links

software_size <- software_df %>%
  select(id = omictools_link, Overall_rating) %>%
  mutate(stars = as.numeric(gsub(" /.*", "", Overall_rating)),
         rate_cnts = as.numeric(gsub(".*for | rate$","",Overall_rating)),
         size = stars * rate_cnts + 1,
         #size = 1,
         Overall_rating = NULL)

software_tree_df<-catalog_software_df %>%
  select(parent.id = parent_href,
         parent.name = parent,
         id = href,
         name) %>%
  #merge(software_size, by='id') %>%
  mutate(parent.id = gsub('^.*-', '', gsub('-p1.html', '', parent.id)),
         #perfect size must consider parent size, total size should not greater than parent size
         size = log2(nchar(name)),
         id = gsub('^.*-', '', gsub('.html', '', id)) ) %>%
  unique %>%
  select(parent.id, parent.name, id, name, size)
  #filter(size > 1)

save(software_tree_df, file='test.RData')
load('test.RData')

#manually fixed strange name cause json unable to parse by d3tree2
#software_tree_df[c(2819,7523),]$name<-c('EM-SNP', 'PRODORIC')
software_tree_df[c(8878,557),]$name<-c('EM-SNP', 'PRODORIC')
software_tree_df<-rbind(catalog_tree_df, software_tree_df)


omictools<-df2NestedList(software_tree_df, 'omictools')

write.utf8(msg=toJSON(omictools, auto_unbox = T, pretty = F),
           file='omictools.json')

d3tree2(
  'omictools.json',
  celltext = "name"
  #width = 1200
)
