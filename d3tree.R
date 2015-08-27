library(jsonlite)
library(d3treeR)
library(plyr)
library(dplyr)

load('omictools.RData')
malformed_links<-catalog_folder_df %>%
  filter(grepl('index', href) |
         grepl('index', parent_href))
malformed_links

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
    select(parent.id, parent.name) %>%
    unique %>%
    rename(id = parent.id,
           name = parent.name) %>%
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

omictools<-df2NestedList(catalog_tree_df, 'omictools')

d3tree2(
  toJSON(omictools, auto_unbox = T),
  celltext = "name",
  width = 1200
)
