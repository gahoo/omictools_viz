library(jsonlite)
library(treemap)
library(d3treeR)
library(data.tree)
library(d3Network)


d3tree2(
  treemap(
    catalog_folder_df,
    ,index=c("parent", "name")
    ,vSize="count"
    #,vColor="GNI"
    ,type="value"
  )
  , rootname = "Omictools"
)

d3tree2(
  "http://bl.ocks.org/mbostock/raw/4063269/flare.json"
  , celltext = "name"
)

flare<-fromJSON("http://bl.ocks.org/mbostock/raw/4063582/raw/flare.json")
jsonedit(flare)


catalog_tree_df<-catalog_folder_df %>%
  select(parent.id = parent_href,
         parent.name = parent,
         id = href,
         name = name,
         size = count) %>%
  mutate(parent.id = gsub('^.*-', '', gsub('-p1.html', '', parent.id)),
         id = gsub('^.*-', '', gsub('-p1.html', '', id)) ) %>%
  unique

tree_df<-catalog_tree_df

ss_tb<-table(catalog_software_df[['parent']])
#roots<-with(tree_df, setdiff(parent.name, name))
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
  merge(roots_size, by='id')

roots_df[duplicated(roots_df$name),]

tt<-data.frame(parent='omictools',
               parent.id='/root',
               id=roots.id,
               name=roots)
  merge(roots_df, roots_size, by='id')
tree_df<-rbind(tt,tree_df)
tree_df<-tree_df %>% 
  mutate(parent=as.character(parent),
         name=as.character(name))

buildNest<-function(root_df){
  if(nrow(root_df)==0){
    return(NA)
  }
  node<-list()
  roots<-with(root_df, setdiff(parent, name))
  for(root in roots){
    children<-subset(root_df, parent==root)$name
    node[['name']]<-root
    node[['children']]<-lapply(children, function(child){
      leaf_df<-subset(tree_df, parent==child)
      child_node<-buildNest(leaf_df)
      if(length(child_node)==0 || is.na(child_node)){
        #message(child)
        #should not use name and parent, use href and parent_href instead!!!
        v<-unlist(ifelse(is.na(ss_tb[child]), 0, ss_tb[[child]]))
        list(name=child, size=v)
      }else{
        child_node
      }
    })
  }
  node
}
kk<-buildNest(tree_df)
library(listviewer)
jsonedit(kk)


d3tree2(
  toJSON(kk, auto_unbox = T)
  , celltext = "name"
)
