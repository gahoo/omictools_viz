library(jsonlite)
library(treemap)
library(d3treeR)
library(data.tree)
library(d3Network)

# example 1 from ?treemap
data(GNI2010)
d3tree2(
  treemap(
    GNI2010
    ,index=c("continent", "iso3")
    ,vSize="population"
    ,vColor="GNI"
    ,type="value"
  )
  , rootname = "World"
)

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


ccc<-as.character(software_df$catalog)
sss<-strsplit(ccc,split=';')
ss<-sapply(strsplit(ccc,split=';'), function(x){
  idx<-length(x)-1
  paste0(x[1:idx], collapse = ';')
  })

ss<-table(ss)

lapply(names(head(ss)), function(name){
  ss[name]
})


test<-list(name='a',
           children=list(
             list(name='b',
                  children=list(
                    list(name='c',
                         children=list(
                           list(name='d')))))))

tree<-list(name='omictools', children=list())
for(cc in sss[1:200]){
  tree[['children']]<-c(tree[['children']], list(nest(cc)) )
}

jsonedit(tree)

nest<-function(vector){
  n<-length(vector)
  if(n>1){
    list(name=vector[1],
         children=list( nest(vector[2:n])) )
  }else{
    list(name=vector)
  }  
}

test<-nest(sss[[1]])
test[[2]][[1]][[2]][[1]]$name<-c(test[[2]][[1]][[2]][[1]]$name,'test')
jsonedit(test)

d3tree2(
  test
  , celltext = "name"
)

cc_df<-catalog_folder_df[c('parent', 'name')]
ss_tb<-table(catalog_software_df[['parent']])
roots<-with(cc_df, setdiff(parent, name))
tt<-data.frame(parent='omictools', name=roots)
cc_df<-rbind(tt,cc_df)
cc_df<-cc_df %>% 
  mutate(parent=as.character(parent),
         name=as.character(name))

buildNest<-function(df){
  if(nrow(df)==0){
    return(NA)
  }
  node<-list()
  roots<-with(df, setdiff(parent, name))
  for(root in roots){
    children<-subset(df, parent==root)$name
    node[['name']]<-children
    node[['children']]<-lapply(children, function(child){
      sub_df<-subset(cc_df, parent==child)
      child_node<-buildNest(sub_df)
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
kk<-buildNest(cc_df)
library(listviewer)
jsonedit(kk)


d3tree2(
  toJSON(kk, auto_unbox = T)
  , celltext = "name"
)
