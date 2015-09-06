library(shiny)
library(jsonlite)
library(d3treeR)
library(DT)
library(reshape2)
library(plyr)
library(dplyr)
library(leaflet)

load('viz.RData')
cid2sid<-function(cid){
  child.ids<-subset(catalog_tree_df, parent.id == cid)$id
  if(length(child.ids) == 0){
    subset(software_tree_df, parent.id == cid)$id
  }else{
    unique(unlist(lapply(child.ids, cid2sid)))
  }
}

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
    
    #leaflet
    pid <- unique(subset(catalog_tree_df, parent.name == input$tree_click$name)$parent.id)
    if(length(pid) == 0){
      sid <- unique(v$table$id)
    }else{
      sid <- unlist(lapply(pid, cid2sid))
    }
    
    clicked_lat_lng <- subset(address_lat_lng_df, id %in% sid)
    message(pid,'\t', nrow(clicked_lat_lng))
    proxy <- leafletProxy("map", data = clicked_lat_lng) %>%
      clearMarkers() 
      
    
    if(nrow(clicked_lat_lng) != 0){
      proxy %>%
        addCircleMarkers(~lng, ~lat,
                         radius = ~log2(cited + 1) + 5,
                         #opacity = ~sqrt(cited) + 10,
                         popup = ~name,
                         stroke = F)
    }
    
  })
  
  output$clickedinfo <- renderText(input$tree_click$name)
  output$catalog <- DT::renderDataTable({
    if(length(v$table) == 0 || is.null(v$table)){
      return(NULL)
    }
    
    merge(name_links, v$table, by='id', all.y=T) %>%
    datatable(selection = 'single', rownames=T, escape = FALSE,
              options=list(paging=F,
                           scrollY="280px",
                           scrollCollapse=F)
              ) %>%
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
              #width = '500px',
              escape = FALSE,
              options = list(paging=F,
                             dom = 't')) %>%
      formatStyle('variable', fontWeight='bold')
  })
  
  output$map<-renderLeaflet({
    leaflet() %>%
      setView(lng=0, lat=0, zoom=1) %>%
      addTiles()
  })
})
