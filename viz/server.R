library(shiny)
library(jsonlite)
library(d3treeR)
library(DT)
library(reshape2)
library(plyr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(RColorBrewer)

load('viz.RData')
cid2sid<-function(cid){
  child.ids<-subset(catalog_tree_df, parent.id == cid)$id
  if(length(child.ids) == 0){
    subset(software_tree_df, parent.id == cid)$id
  }else{
    unique(unlist(lapply(child.ids, cid2sid)))
  }
}

shinyServer(function(input, output, session) {
  
  output$tree <- renderD3tree2({
    d3tree2(toJSON(omictools, auto_unbox = T),
            celltext = "name",
            width="100%")
  })
  
  v <- reactiveValues(table = NULL,
                      detail =NULL)
  
  observeEvent(input$tree_click, {
    #without catalog id, might not be able to get correct info since same name exists
    v$table <- subset(tree_df, parent.name == input$tree_click$name) %>%
      select(-name) %>%
      unique
    
    if(length(grep('^s', v$table$id)) >0){
      v$table <- v$table %>%
        left_join(id_cited, by='id') %>%
        left_join(software_df[c('id', 'Type_of_tool')], by='id') %>%
        unique
    }
    
  })
  
  observeEvent(input$catalog_bound_map & input$map_fit_bound,{
    #make sure catalog_bound_map & map_fit_bound not TRUE at the same time
    if(input$catalog_bound_map & input$map_fit_bound == TRUE){
      updateCheckboxInput(session, 'map_fit_bound', label = 'Fit Bound (Map follow table)', value = F)
      updateCheckboxInput(session, 'catalog_bound_map', label = 'Bound map (Table follow map)', value = F)
    }
  })
  
  
  bound_sid<-reactive({
    bound_lat_lng <- address_lat_lng_df %>%
      filter(lng < input$map_bounds$east &
             lng > input$map_bounds$west &
             lat < input$map_bounds$north &
             lat > input$map_bounds$south)
    bound_lat_lng$id
  })
  
  sid <- reactive({
    #leaflet
    pid <- unique(subset(tree_df,
                         parent.name == input$tree_click$name)$parent.id)
    #message(pid)
    if(length(pid) == 0){
      unique(v$table$id)
    }else{
      unlist(lapply(pid, cid2sid))
    }
  })
  
  observe({
    clicked_lat_lng <- subset(address_lat_lng_df, id %in% sid()) %>%
      merge(software_df[c('id', input$stat)], by='id') %>%
      mutate_(color = input$stat) %>%
      mutate(color = as.character(color))
    
    #message(nrow(clicked_lat_lng))
    proxy <- leafletProxy("map", data = clicked_lat_lng) %>%
      clearMarkerClusters() %>%
      clearMarkers()
      
    cluster_option <- NULL
    if(input$map_cluster){
      cluster_option <- markerClusterOptions()
    }
    
    #color pal
    base_pal<-colorRampPalette(brewer.pal(12,"Set3"))
    
    stat_levels<-unique(clicked_lat_lng$color)
    n_color<-length(stat_levels)
    n_color<-ifelse(n_color==0, 1, n_color)
    pal_factor<-base_pal(n_color) %>%
      colorFactor(domain = stat_levels)
    
    cited_domain <- unique(log10(clicked_lat_lng$cited+1))
    pal_cited <- colorNumeric('YlOrRd', cited_domain)    
    
    if(nrow(clicked_lat_lng) != 0){
      if(input$map_coloring_cited){
        color_formula <- formula("~pal_cited(log10(cited+1))")
      }else{
        color_formula <- formula("~pal_factor(color)")
      }
      
      proxy<-proxy %>%
        addCircleMarkers(~lng, ~lat,
                         layerId = ~id,
                         clusterOptions = cluster_option,
                         radius = ~5 * sqrt(log10(cited + 1) ) + 5,
                         #opacity = ~sqrt(cited) + 10,
                         fillOpacity = 0.4,
                         color = color_formula,
                         popup = ~name,
                         stroke = F)
      
      if(input$map_legend){
        if(input$map_coloring_cited){
          pal_func <- pal_cited
          values <- formula("~log10(cited+1)")
          title <- "log10(cited)"
        }else{
          pal_func <- pal_factor
          values <- formula("~color")
          title <- input$stat
        }
        proxy<-proxy %>%
          clearControls() %>%
          addLegend("bottomleft", pal = pal_func, values = values,
                    title = title,
                    opacity = 1)
        
      }
        
    }
  })
  
  observe({
    clicked_id = v$table[as.numeric(input$catalog_row_last_clicked),]$id
    clicked_soft<-subset(address_lat_lng_df, id %in% clicked_id) %>%
      unique %>%
      filter(!is.na(lat) & !is.na(lng))
    if(nrow(clicked_soft) == 0){
      return(NULL)
    }
    
    proxy <- leafletProxy("map", data = clicked_soft)
    proxy<-proxy %>%
      clearGroup(group = 'pin') %>%
      addMarkers(lat = ~lat, lng = ~lng,
                 layerId = ~paste0('pin_',id),
                 group = 'pin', popup = ~name)
    
    if(input$map_fit_bound){
      proxy %>%
        fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
    }
      
  })
  
  output$clickedinfo <- renderText(input$tree_click$name)
  output$catalog <- DT::renderDataTable({
    if(length(v$table) == 0 || is.null(v$table)){
      return(NULL)
    }
    
    catalog_tbl<-right_join(name_links, v$table, by='id')
      
    if(input$catalog_bound_map){
      catalog_tbl<-catalog_tbl %>%
        filter(id %in% bound_sid() | grepl('^c', id))
    }
    
    catalog_tbl<-catalog_tbl %>%
      datatable(selection = 'single', rownames=T, escape = FALSE,
                options=list(paging=F,
                             scrollX=T,
                             scrollY="280px",
                             scrollCollapse=F)
                ) %>%
      formatStyle(
        'size',
        background = styleColorBar(v$table$size, 'LightSkyBlue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
    if('cited' %in% names(v$table)){
      catalog_tbl <- catalog_tbl %>%
        formatStyle(
          'cited',
          background = styleColorBar(v$table$cited, 'LightSalmon'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
    
    catalog_tbl
  })
  
  observeEvent(input$map_marker_click,{
    str(input$map_marker_click)
    sid<-gsub('^pin_','',input$map_marker_click$id)
    message(sid)
    if(is.null(sid)||is.na(sid)||length(sid==0)){
      v$detail<-NULL
    }
    v$detail<-software_df %>%
      filter(id == sid) %>%
      melt(id.vars='omictools_link', na.rm=T) %>%
      select(-omictools_link)
  })
  
  observeEvent(input$catalog_row_last_clicked,{
    clicked_id = v$table[as.numeric(input$catalog_row_last_clicked),]$id
    #message(as.numeric(input$catalog_row_last_clicked), clicked_id)
    #str(v$table[as.numeric(input$catalog_row_last_clicked),])
    if(length(clicked_id) == 0 || is.na(clicked_id)){
      v$detail<-NULL
    }
    if(substr(clicked_id,1,1) == 'c'){
      v$detail<-catalog_desc %>%
        filter(id == clicked_id) %>%
        melt(id.vars='id', na.rm=T) %>%
        select(-id)
    }else{
      v$detail<-software_df %>%
        filter(id == clicked_id) %>%
        melt(id.vars='omictools_link', na.rm=T) %>%
        select(-omictools_link)
    }
  })
  
  output$detail <- DT::renderDataTable({
    if(is.null(v$detail)){
      return(NULL)
    }
    
    datatable(v$detail, selection = 'none', rownames=F,
              colnames=c('',' '),
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
  
  output$barplot<-renderPlot({
    sids<-sid()
    if(input$catalog_bound_map){
      sids<-intersect(bound_sid(), sids)
    }
    sub_software_df<-software_df %>%
      filter(id %in% sids) %>%
      select(id, Language, License, Interface, Taxonomy,
             Input, Output, Operating_system,
             Type_of_tool, Nature_of_tool)
    if(nrow(sub_software_df)==0){
      return(NULL)
    }
    
    data4plot<-sub_software_df[[input$stat]] %>%
      as.character %>%
      strsplit(split=', ') %>%
      unlist %>%
      table %>%
      as.data.frame %>%
      arrange(Freq) %>%
      mutate(stats = factor(., .))
    
    ggplot(data4plot) +
      aes_string(x='stats', y='Freq') +
      geom_bar(stat='identity') +
      coord_flip() +
      theme(axis.title.y=element_blank())
  })
})
