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
  pal1 <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = compartment$TYPE2)
  
  # create the leaflet map  
  output$compartment <- renderLeaflet({
    leaflet(compartment) |>
      setView(
        lng = -120.65965, 
        lat = 38.905, 
        zoom = 13
      )|>
      addPolygons(
        data = bound,
        color = "blue",
        weight = 3,
        fillColor = "blue",
        fillOpacity = 0.0,
        group = "Property Boundary",
      ) |>
      addPolygons(data = compartment,
        weight = 2, 
        smoothFactor = 0.2, 
        fillOpacity = 0.8,
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
          "Property Boundary", "Compartments"
          ),
        position = "topleft",
        options = layersControlOptions()
        ) |>
      addMiniMap(position="topright"
      )
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    compartment[5,4]
    #colnames = c( "Compartment Number", "Size (acres)", "Silviculture", "Management Allocation", "Most Recent Inventory", "Last Plant Date", "Last Harvest Date")
  ))
  
  #[5,6,7,9,10,13,16]
  
})

