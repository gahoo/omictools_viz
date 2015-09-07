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
           checkboxInput('cluster', 'Clustering', value=F),
           selectInput('stat', 'stat',
                       choices = c('Language', 'License',
                                   'Interface', 'Taxonomy',
                                   'Type_of_tool', 'Nature_of_tool'),
                       selected = 'Language'),
           plotOutput('barplot') ),
    column(6, 
           strong(textOutput("clickedinfo")),
           DT::dataTableOutput('catalog'),
           DT::dataTableOutput('detail', width="100%"))
  )
))
