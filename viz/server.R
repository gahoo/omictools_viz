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

catalog_desc<-catalog_software_df %>%
  mutate( id = gsub('^.*-', '', gsub('-p1.html', '', parent_href)) ) %>%
  select(id,
         alias = parent_alias,
         description = parent_desc) %>%
  unique

tree_df<-rbind(catalog_tree_df, software_tree_df)

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
      merge(software_df[c('id', 'Type_of_tool')], by='id', all.x=T)
  })
  
  output$clickedinfo <- renderText(input$tree_click$name)
  output$catalog <- DT::renderDataTable({
    datatable(v$table, selection = 'single', rownames=T,
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
              options = list(paging=F,
                             dom = 't')) %>%
      formatStyle('variable', fontWeight='bold')
  })
})
