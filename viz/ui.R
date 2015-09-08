library(shiny)
library(d3treeR)
library(DT)
library(leaflet)


shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  HTML("<button type='button' class='btn btn-info btn-xs'",
       "data-toggle='collapse' data-target='#info'>",
       "<span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>",
       " Show/Hide Information",
       "</button>"),
  div(id = "info", class = "collapse in",
      'test info'
  ),
  fluidRow(
    column(6,
           d3tree2Output('tree'),
           leafletOutput('map'),
           HTML("<button type='button' class='btn btn-info btn-xs pull-right'",
                "data-toggle='collapse' data-target='#custom'>",
                "<span class='glyphicon glyphicon-th-list' aria-hidden='true'></span>",
                " Customize",
                "</button>"),
           div(id = "custom", class = "collapse",
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
