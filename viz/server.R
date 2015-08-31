library(shiny)
library(jsonlite)
library(d3treeR)
library(DT)
library(reshape2)
library(dplyr)


load('../omictools.RData')
load('../omictools_tree.RData')
software_df<-software_df %>%
  mutate(id = gsub('^.*-', '', gsub('.html', '', omictools_link)) )

tree_df<-rbind(catalog_tree_df, software_tree_df)

shinyServer(function(input, output) {
  
  output$tree <- renderD3tree2({
    d3tree2(toJSON(omictools, auto_unbox = T),
            celltext = "name",
            width="100%")
  })
  
  v <- reactiveValues(table = NULL,
                      last = NULL)
  
  observeEvent(input$tree_click, {
    getUniqId<-function(click_name){
      
      cur<-tree_df %>%
        filter(parent.name == click_name) %>%
        select(parent.id, id) %>%
        unique
      
      str(cur)
      
      nid <- length(unique(cur$parent.id))
      
      
      if(nid == 1){
        parent.id <- cur$parent.id
      }else{
        parent.id<-intersect(v$last$id, cur$parent.id)
        cur <- cur[cur$parent.id == parent.id, ]
      }
      v$last<-cur
      unique(parent.id)
      
    }
    
    #without catalog id, might not be able to get correct info since same name exists
    
    p.id<-getUniqId(input$tree_click$name)
    message(parent.id)
    
    v$table <- subset(catalog_tree_df, parent.id == p.id)
    if(nrow(v$table)==0){
      v$table <- subset(software_tree_df, parent.id == p.id)
    }
    
  })
  
  output$clickedinfo <- renderText(input$tree_click$name)
  output$catalog <- DT::renderDataTable({
    datatable(v$table, selection = 'single', rownames=T) %>%
      formatStyle(
        'size',
        background = styleColorBar(v$table$size, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$detail <- DT::renderDataTable({
    sid = v$table[input$catalog_row_last_clicked,]$id
    #message(sid)
    #message(input$catalog_row_last_clicked)
    
    software_df %>%
      filter(id == sid) %>%
      melt(id.vars='omictools_link', na.rm=T) %>%
      select(-omictools_link) %>%
      datatable(selection = 'none', rownames=F,
                options = list(paging=F,
                               dom = 't')) %>%
      formatStyle('variable', fontWeight='bold')
  })
})
