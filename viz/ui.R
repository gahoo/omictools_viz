library(shiny)
library(d3treeR)
library(DT)
library(leaflet)


shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  fluidRow(
    column(6,
           d3tree2Output('tree'),
           leafletOutput('map') ),
    column(6, 
           strong(textOutput("clickedinfo")),
           DT::dataTableOutput('catalog'),
           DT::dataTableOutput('detail', width="100%"))
  )
))
