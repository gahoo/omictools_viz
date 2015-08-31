library(shiny)
library(jsonlite)
library(d3treeR)
library(DT)
library(reshape2)
library(plyr)
library(dplyr)


load('../omictools.RData')
load('../omictools_tree.RData')

build_a<-function(X){
  build_each_a<-function(x){
    if(is.na(x)){
      return(x)
    }
    
    if(length(grep(';', x))>0){
      x<-unlist(strsplit(x, split=';'))
    }
    
    sapply(x, sprintf, fmt='<a href="%s">%s</a>', x, x) %>%
      paste0(collapse = ' ')
  }
  
  sapply(as.character(X), build_each_a)
}

software_df<-software_df %>%
  mutate(id = gsub('^.*-', '', gsub('.html', '', omictools_link)),
         related = gsub(';', '; ', related),
         catalog = gsub(';', '> ', catalog),
         img = sprintf("<img src='%s'/>", img),
         link = build_a(link),
         PubMed = build_a(PubMed),
         URL = build_a(URL) )

catalog_desc<-catalog_software_df %>%
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
         name = sprintf('<a href="http://omictools.com/%s">%s</a>', href, name) ) %>%
  select(-href)

name_links<-name_links[!duplicated(name_links$id),]

shinyServer(function(input, output) {
  
  output$tree <- renderD3tree2({
    d3tree2(toJSON(omictools, auto_unbox = T),
            celltext = "name",
            width="100%")
  })
  
  v <- reactiveValues(table = NULL)
  
  observeEvent(input$tree_click, {
    #without catalog id, might not be able to get correct info since same name exists
    v$table <- subset(tree_df, parent.name == input$tree_click$name) %>%
      merge(software_df[c('id', 'Type_of_tool')], by='id', all.x=T) %>%
      select(-name) %>%
      unique
  })
  
  output$clickedinfo <- renderText(input$tree_click$name)
  output$catalog <- DT::renderDataTable({
    merge(name_links, v$table, by='id', all.y=T) %>%
    datatable(selection = 'single', rownames=T, escape = FALSE,
              options=list(pageLength=5)) %>%
      formatStyle(
        'size',
        background = styleColorBar(v$table$size, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$detail <- DT::renderDataTable({
    clicked_id = v$table[input$catalog_row_last_clicked,]$id
    if(length(clicked_id) == 0 || is.na(clicked_id)){
      return(NULL)
    }
    if(substr(clicked_id,1,1) == 'c'){
      detail<-catalog_desc %>%
        filter(id == clicked_id) %>%
        melt(id.vars='id', na.rm=T) %>%
        select(-id)
    }else{
      detail<-software_df %>%
        filter(id == clicked_id) %>%
        melt(id.vars='omictools_link', na.rm=T) %>%
        select(-omictools_link)
    }
    
    datatable(detail, selection = 'none', rownames=F,
              width = '500px', escape = FALSE,
              options = list(paging=F,
                             dom = 't')) %>%
      formatStyle('variable', fontWeight='bold')
  })
})
