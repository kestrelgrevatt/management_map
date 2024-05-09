library(markdown)
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(readr)
library(sf)
library(leaflet.extras)
library(dbscan)
library(geosphere)
library(magrittr)
library(raster)
library(htmlwidgets)
library(htmltools)


shinyServer(function(input, output) {

  #input property boundary layer
  bound <- read_sf(dsn = "propbound", layer = "prop_bound") |>
    st_transform(4326)
  
  #input compartment layer
  compartment <- read_sf(dsn = "comp2", layer = "COMPSHP") |>
    st_transform(4326)
  
  #rename compartment type
  compartment$TYPE2 <- compartment$TYPE
  
  compartment <- compartment |> mutate(TYPE2 =(
    case_when(
      TYPE2 ==
        "E"  ~ "Even-Aged", 
      TYPE2 ==
        "U"  ~ "Uneven-Aged",
      TYPE2 == 
        "R" ~ "Reserve"
      )))
  
  #rename allocation type
  compartment$ALLOCATION2 <- compartment$ALLOCATION
  
  compartment <- compartment |> mutate(ALLOCATION2 =(
    case_when(
      ALLOCATION2 ==
        "AR"  ~ "Administrative Reserve",
      ALLOCATION2 ==
        "CB"  ~ "Uneven",
      ALLOCATION2 ==
        "E"  ~ "Even-Aged",
      ALLOCATION2 ==
        "ER"  ~ "Ecological Reserve",
      ALLOCATION2 ==
        "GS"  ~ "Group Selection",
      ALLOCATION2 ==
        "OR"  ~ "Understocked/Shrub",
      ALLOCATION2 ==
        "SS"  ~ "Single Tree Selection",
      ALLOCATION2 ==
        "SW"  ~ "Shelterwood",
      ALLOCATION2 ==
        "YG"  ~ "Young Growth Plantation",
      ALLOCATION2 == 
        "YR" ~ "Young Growth Reserve"
      )))
  
  #create color pallet for map
  pal1 <- colorFactor(pal = c("#AD2942", "#d95f02", "#9677BB"), domain = compartment$TYPE2)
  
  #create custom title using HTML
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-680px,10px);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 12px; 
    padding-right: 12px; 
    background: #D4D5CF;
    color: #0E1180;
    font-weight: bold;
    font-size: 30px;
    -moz-border-radius: 10px;
    -webkit-border-radius: 10px;
    -o-border-radius: 10px;
    -ms-border-radius: 10px;
    border-radius: 10px
    }
    "))
  title <- tags$div(
      tag.map.title, HTML("Blodgett Forest Compartments")
    )  
  
  #create custom subtitle using HTML
  tag.map.subtitle <- tags$style(HTML("
  .leaflet-control.map-subtitle { 
    transform: translate(-500px,70px);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 12px; 
    padding-right: 12px; 
    background: #D4D5CF;
    color: #0E1180;
    font-weight: bold;
    font-size: 20px;
    -moz-border-radius: 10px;
    -webkit-border-radius: 10px;
    -o-border-radius: 10px;
    -ms-border-radius: 10px;
    border-radius: 10px
    }
    "))
  
  subtitle <- tags$div(
    tag.map.subtitle, HTML("Click & Hover for Details")
  )  
  
  # create the leaflet map  
  output$compartment <- renderLeaflet({
    leaflet(compartment) |>
      setView(
        lng = -120.663, 
        lat = 38.905, 
        zoom = 13
      )|>
      addPolygons(
        data = bound,
        color = "blue",
        opacity = 1,
        weight = 3,
        fillColor = "blue",
        fillOpacity = 0.0,
        group = "Property Boundary",
      ) |>
      addPolygons(
        data = compartment,
        color = "#D9310D",
        weight = 2,
        fillColor = "blue",
        fillOpacity = 0.0,
        smoothFactor = 0.2, 
        group = "Compartment Boundaries"
        ) |>
      addPolygons(
        data = compartment,
        weight = 2, 
        smoothFactor = 0.2, 
        fillOpacity = 0.6,
        fillColor = ~pal1(compartment$TYPE2),
        group = "Compartments",
        label = compartment$TYPE2,
        popup = paste("<strong>Compartment Number: ", compartment$COMP,"</strong><br>",
                      "Silviculture: ", compartment$TYPE2, "<br>",
                      "Management Allocation: ", compartment$ALLOCATION2, "<br>",
                      "Size (acres): ", compartment$ACRES, "<br>")
        ) |>
      addProviderTiles(
        "Esri.WorldStreetMap",
        group = "Esri.WorldStreetMap"
        ) |>
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
        ) |>
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Esri: WorldImagery"
        ) |>
      addLegend(
        pal=pal1, 
        values = compartment$TYPE2,
        na.label = "Unclassified",
        opacity=1, 
        title = "Silviculture"
        )|>
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Esri: WorldStreetMap", "Esri: WorldImagery"
          ),
        overlayGroups = c(
          "Compartments", "Compartment Boundaries", "Property Boundary"
          ),
        position = "topleft",
        options = layersControlOptions(
          collapsed = FALSE
          )
        ) |>
      addMiniMap(
        position="topright"
        ) |>
      addControl(
        title, 
        position="topleft",
        className="map-title"
        ) |>
      addControl(
        subtitle, 
        position="topleft",
        className="map-subtitle"
        )
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    compartment
  ))
  
})

