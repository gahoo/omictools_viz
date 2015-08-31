library(shiny)
library(d3treeR)
library(DT)


shinyUI(fluidPage(
  titlePanel('Omictools.com Visualization'),
  fluidRow(
    column(6, d3tree2Output( 'tree' )),
    column(6, 
           textOutput( "clickedinfo", container = p ),
           DT::dataTableOutput('catalog')),
    DT::dataTableOutput('detail')    
#textOutput( "detail", container = p )
  )
))
