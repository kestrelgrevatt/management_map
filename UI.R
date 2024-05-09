library(shiny)
library(leaflet)

text1 <- "Map of Blodgett Forest Research Station showing treatment areas across forest and their management allocation and size. 
          Hover over and click polygons for additional information. <br>
          Blodgett Forest has been divided into stand-level compartments shown here to 
          demonstrate and research a broad range of management alternatives across the forest. <br>
          As the data shown in the Data tab is cleaned and updated, additional treatment detail will be added to map."


navbarPage("Blodgett Forest Research Station", id="main",
           tabPanel("Map", leafletOutput("compartment", height=1000)),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",text1))