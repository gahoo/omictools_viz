library(shiny)
library(d3treeR)
library(DT)
library(leaflet)

collapsibleDiv<-function(id, ..., label='Show/Hide', .func=actionButton,
                         collapse = FALSE, class=NULL, icon=NULL, width=NULL){
  
  collapse_status<-ifelse(collapse, "on", "in")
  
  list(
    .func(
      sprintf("b_%s",id), label=label, icon=icon, class=class, width=width,
      "data-toggle"='collapse', "data-target"=sprintf('#%s',id)
      ),
    div(
      id = id, class = sprintf("collapse %s", collapse_status),
      ...
    )
  )
}

shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  collapsibleDiv(id='info', collapse = F,
                 label = 'Show/Hide Information',
                 class = 'btn-info btn-xs',
                 icon = icon('info-sign',lib='glyphicon'),
                 #button.width = "100%",
                 includeMarkdown('about.Rmd')),
  fluidRow(
    column(6,
           collapsibleDiv(
             id='customize', collapse = T,
             label = 'Customize',
             class = 'btn-info btn-xs pull-right',
             icon = icon('th-list', lib='glyphicon'),
             
             checkboxInput('map_cluster', 'Clustering', value=F),
             checkboxInput('map_legend', 'Legend', value=T),
             checkboxInput('map_fit_bound', 'Fit Bound (Map follow table)', value=F),
             checkboxInput('catalog_bound_map', 'Bound map (Table follow map)', value=F),
             selectInput('stat', 'stat',
                         choices = c('cited', 'Language', 'License',
                                     'Interface', 'Taxonomy',
                                     'Input', 'Output', 'Operating_system',
                                     'Type_of_tool', 'Nature_of_tool'),
                         selected = 'Type_of_tool')
           ),
           collapsibleDiv(
             id='d3tree', label = 'd3tree', class = 'btn-xs',
             d3tree2Output('tree')
             ),
           collapsibleDiv(
             id='maps', label='maps', class = 'btn-xs',
             leafletOutput('map')
             ),
           collapsibleDiv(
             id='ggplot', label='barplot', class = 'btn-xs',
             plotOutput('barplot')
             )
           ),
    column(6, 
           strong(textOutput("clickedinfo")),
           DT::dataTableOutput('catalog'),
           DT::dataTableOutput('detail', width="100%")
           )
  )
))
