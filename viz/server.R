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
                      last_click = "",
                      cid_chain = c())
  
  observeEvent(input$tree_click, {
    #without catalog id, might not be able to get correct info since same name exists
    cid <- unique(subset(catalog_tree_df, parent.name == input$tree_click$name)$parent.id)
    last_cid <- v$cid_chain[length(v$cid_chain)]
    cids <- unique(subset(catalog_tree_df, parent.id == last_cid)$id)
    v$table <- subset(catalog_tree_df,
                      parent.name == input$tree_click$name &
                        parent.id %in% cids)
    
    if(nrow(v$table)==0){
      v$table <- subset(software_tree_df, parent.name == input$tree_click$name)
    }
    
    if( !(cid %in% v$cid_chain) ){
      v$last_click <- input$tree_click$name
      v$cid_chain <- c(v$cid_chain, cid)
    }else{
      v$cid_chain <- v$cid_chain[-length(v$cid_chain)]
    }
    print(v$cid_chain)
  })
  
  output$clickedinfo <- renderText(str(input$tree_click))
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
