library(shiny)
library(d3treeR)
library(DT)
library(leaflet)

collapsibleDiv<-function(id, ...,
                         button.label='Show/Hide',
                         collapse = FALSE, button.class='btn-info btn-xs',
                         button.icon=NULL, button.width=NULL){
  
  collapse<-ifelse(collapse, "on", "in")
  
  list(
    actionButton(
      sprintf("b_%s",id), label=button.label, icon=button.icon,
      class=button.class, width=button.width, "data-toggle"='collapse',
      "data-target"=sprintf('#%s',id) 
      ),
    div(id = sprintf("%s", id),
        class = sprintf("collapse %s", collapse),
        ...
    )
  )
}

shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  collapsibleDiv(id='info', collapse = F,
                 button.label = 'Show/Hide Information',
                 button.icon = icon('info-sign',lib='glyphicon'),
                 #button.width = "100%",
                 "test info"),
  fluidRow(
    column(6,
           d3tree2Output('tree'),
           leafletOutput('map'),
           collapsibleDiv(
             id='customize', collapse = T,
             button.label = 'Customize',
             button.class = 'btn-info btn-xs pull-right',
             button.icon = icon('th-list', lib='glyphicon'),
             
             checkboxInput('map_cluster', 'Clustering', value=F),
             checkboxInput('map_coloring_cited', 'Coloring by cited', value=F),
             checkboxInput('map_legend', 'Legend', value=T),
             checkboxInput('map_fit_bound', 'Fit Bound (Map follow table)', value=F),
             checkboxInput('catalog_bound_map', 'Bound map (Table follow map)', value=F),
             selectInput('stat', 'stat',
                         choices = c('Language', 'License',
                                     'Interface', 'Taxonomy',
                                     'Input', 'Output', 'Operating_system',
                                     'Type_of_tool', 'Nature_of_tool'),
                         selected = 'Type_of_tool')
           ),
           plotOutput('barplot') ),
    column(6, 
           strong(textOutput("clickedinfo")),
           DT::dataTableOutput('catalog'),
           DT::dataTableOutput('detail', width="100%"))
  )
))
