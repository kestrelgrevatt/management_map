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
library(rgeos)
library(raster)

shinyServer(function(input, output) {
  # Import Data and clean it
  
  bb_data <- read.csv("data_test.csv", stringsAsFactors = FALSE )
  bb_data <- data.frame(bb_data)
  bb_data$Latitude <-  as.numeric(bb_data$Latitude)
  bb_data$Longitude <-  as.numeric(bb_data$Longitude)
  bb_data=filter(bb_data, Latitude != "NA") # removing NA values
  
  # new column for the popup label
  
  # Name	Silviculture	Tx1	Year	Latitude	Longitude
  
  bb_data <- mutate(bb_data, cntnt=paste0('<strong>Compartment Number: </strong>', Name,
                                          '<br><strong>Management Category:</strong> ', Tx1,
                                          '<br><strong>Most Recent Action</strong> ', Year)) 
  
  bound <- read_sf(dsn = "propbound", layer = "prop_bound") |>
    st_transform(4326)
  
  compartment <- read_sf(dsn = "comp2", layer = "COMPSHP") |>
    st_transform(4326)
  
  centers <- data.frame(gCentroid(compartment, byid = TRUE))
  centers$comp <- row.names(compartment)
  
  #shapeData <- spTransform(comp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  
  #adm <- getData('GADM', country='UKR', level=1)
  
  #popup <- paste0("<strong>Name: </strong>", 
  #                adm$NAME_1)
  
  #leaflet() %>% 
  #  addTiles() %>% 
  #  addPolygons(data=adm, weight = 2, fillColor = "yellow", popup=popup)
  
  
  #comp_data <- read_sp(dsn = "comp2", layer = "COMPSHP") |>
  #  st_transform(4326)
  
  # 
  # url <- "https://34c031f8-c9fd-4018-8c5a-4159cdff6b0d-cdn-endpoint.azureedge.net/-/media/calfire-website/what-we-do/fire-resource-assessment-program---frap/gis-data/april-2023/fire221gdb.zip?rev=9e3e1e5e61e242d5b2994d666d72a91a&hash=F424990CD64BB7C4CF01C6CE211C0A59"
  # download.file(url, "fire221.gdb.zip",  mode="wb")
  # 
  # unzip("fire221.gdb.zip")
  # 
  # fire_polys <- 
  #   read_sf("fire22_1.gdb", layer = "firep22_1") |>
  #   st_transform(4326)
  
  # compartments <- st_read("propbound/prop_bound.shp")
  
  # create a color palette for category type in the data file
  
  
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("STS", "Reserve", "GS"))
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) |>
      setView(lng = -120.65965, lat = 38.905, zoom = 13
      )|>
      addPolygons(
        data = bound,
        group = "Property Boundary"
      ) |>
      addPolygons(
        data = compartment,
        group = "Compartment Boundaries"
      ) |>
      #addPolygons(
      #  data=comp, weight = 2, fillColor = "yellow")|>
      addPolygons(data = compartment,
                  stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
                  fillColor = "red",
                  popup = paste("Region: ", compartment$comp, "<br>")) |>
      addLabelOnlyMarkers(data = centers,
                          lng = ~x, lat = ~y, label = ~comp,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) |>
      addProviderTiles(
        "Esri.WorldStreetMap",
        group = "Esri.WorldStreetMap"
      ) |>
      addProviderTiles(
        "OpenStreetMap",
        # give the layer a name
        group = "OpenStreetMap"
      ) |>
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Esri: WorldImagery"
      ) |>
      addCircles(lng = ~Longitude, lat = ~Latitude
      ) |> 
      addTiles(
      ) |>
      addCircleMarkers(data = bb_data, lat =  ~Latitude, lng =~Longitude, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = ~pal(Category),
                       stroke = FALSE, fillOpacity = 0.8
      )|>
      addLegend(pal=pal, values=bb_data$Category,opacity=1, na.label = "Not Available")|>
      #addEasyButton(easyButton(
      #  icon="fa-crosshairs", title="ME",
      #  onClick=JS("function(btn, map){ map.locate({setView: true}); }")))|>
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Esri: WorldStreetMap", "Esri: WorldImagery"
        ),
        overlayGroups = c(
          "Property Boundary", "Compartment Boundaries"
        ),
        # position it on the topleft
        position = "topleft",
        options = layersControlOptions()
      ) |>
      #addMarkers(comp, lng = ~lon, lat = ~lat,label = ~df$city
      #) |>
      addMiniMap(
      )
      #addEasyButton(easyButton(
      #  icon="fa-crosshairs", title="comp",
      #  onClick=JS("function(btn, map){ map.locate({setView: true}); }"))
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    bb_data[,c(-1,-23,-24,-25,-28:-35)],filter = 'top',
    colnames = c( "Name", "Category", "Tx1", "Lat", "Long.")
  ))
  
  
})

