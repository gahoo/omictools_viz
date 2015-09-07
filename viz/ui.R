library(shiny)
library(d3treeR)
library(DT)
library(leaflet)


shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  fluidRow(
    column(6,
           d3tree2Output('tree'),
           leafletOutput('map'),
           selectInput('stat', 'stat',
                       choices = c('Language', 'License',
                                   'Type_of_tool', 'Nature_of_tool'),
                       selected = 'Language'),
           plotOutput('barplot') ),
    column(6, 
           strong(textOutput("clickedinfo")),
           DT::dataTableOutput('catalog'),
           DT::dataTableOutput('detail', width="100%"))
  )
))
