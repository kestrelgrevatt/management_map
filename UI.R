library(shiny)
library(leaflet)

text1 <- "Map of Blodgett Forest Research Station showing treatment areas, management allocation, and size. Hover over and click polygons for additional information."

navbarPage("Blodgett Forest Research Station", id="main",
           tabPanel("Map", leafletOutput("compartment", height=1000)),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",text1))