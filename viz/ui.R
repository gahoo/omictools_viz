library(shiny)
library(d3treeR)
library(DT)
library(leaflet)

collapsibleDiv<-function(
  id, ..., collapse = FALSE,
                         button.text='Show/Hide',
                         button.class='btn-info btn-xs',
                         glyphicon='glyphicon-info-sign'){
  if(!is.null(glyphicon)){
    icon_html<-sprintf("<span class='glyphicon %s' aria-hidden='true'></span>", glyphicon)
  }else{
    icon_html<-""
  }
  
  collapse<-ifelse(collapse, "on", "in")
  
  list(
    HTML(sprintf("<button type='button' class='btn %s'", button.class),
         sprintf("data-toggle='collapse' data-target='#%s'>", id),
         icon_html,
         sprintf(" %s", button.text),
         "</button>"),
    div(id = sprintf("%s", id),
        class = sprintf("collapse %s", collapse),
        ...
    )
  )
}

shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  collapsibleDiv(id='info', collapse = T,
                 button.text = 'Show/Hide Information',
                 "test info"),

  fluidRow(
    column(6,
           d3tree2Output('tree'),
           leafletOutput('map'),
           collapsibleDiv(
             id='customize', collapse = T,
             button.text = 'Customize',
             button.class = 'btn-info btn-xs pull-right',
             glyphicon = 'glyphicon-th-list',
             
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
