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

compartment2 <- read_sf(dsn = "comp2", layer = "COMPSHP") |>
  st_transform(4326)

compartment2

#compartment2$TYPE2 <- if (compartment2$TYPE=="E"){compartment2$TYPE2[]='even'}

compartment2$TYPE2 <- compartment2$TYPE


compartment2 <- compartment2 |> mutate(TYPE2 =(
  case_when(
    TYPE2 ==
      "E"  ~ "even", 
    TYPE2 == 
      "R" ~ "reserve",
    TYPE2 ==
      "U"  ~ "uneven")))

compartment2$TYPE2

compartment2

compartment2[3]


####

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
  
  # import Data and clean it
  bb_data <- read.csv("data_test.csv", stringsAsFactors = FALSE )
  bb_data <- data.frame(bb_data)
  bb_data$Latitude <-  as.numeric(bb_data$Latitude)
  bb_data$Longitude <-  as.numeric(bb_data$Longitude)
  bb_data=filter(bb_data, Latitude != "NA") # removing NA values
  
  # new column for the popup label
  bb_data <- mutate(bb_data, cntnt=paste0('<strong>Compartment Number: </strong>', Name,
                                          '<br><strong>Management Category:</strong> ', Tx1,
                                          '<br><strong>Most Recent Action</strong> ', Year)) 
  
  #property boundary layer
  bound <- read_sf(dsn = "propbound", layer = "prop_bound") |>
    st_transform(4326)
  
  #compartment layer
  compartment <- read_sf(dsn = "comp2", layer = "COMPSHP") |>
    st_transform(4326)
  # 
  # centers <- data.frame(getSpPPolygonsLabptSlots(compartment))
  # centers$comp <- row.names(compartment)
  
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
  pal1 <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = compartment$TYPE2)
  
  
  # varname <- switch(compartment$TYPE,
  #                  "E" = "Even",
  #                  "R"="Reserve",
  #                  "U"="Uneven",)
  
  # compartment$TYPES2 <- switch(c(compartment$TYPE,
  #                             "E" = "Even",
  #                             "R"="Reserve",
  #                             "U"="Uneven",))
  
  #c("Even", "Reserve", "Uneven")
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) |>
      setView(lng = -120.65965, lat = 38.905, zoom = 13
      )|>
      addPolygons(
        data = bound,
        color = "blue",
        weight = 3,
        fillColor = "blue",
        fillOpacity = 0.1,
        group = "Property Boundary",
      ) |>
      #addPolygons(
      #  data = compartment,
      #  group = "Compartment Boundaries"
      #) |>
      #addPolygons(
      #  data=comp, weight = 2, fillColor = "yellow")|>
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
      addLabelOnlyMarkers(icon = "fa-crosshairs", label = compartment$COMP) |>
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
      addLegend(pal=pal, values=bb_data$Category, opacity=1)|>
      addLegend(
        pal=pal1, 
        values = compartment$TYPE2,
        na.label = "Unclassified",
        opacity=1, 
        title = "Silviculture"
      )|>
      #addEasyButton(easyButton(
      #  icon="fa-crosshairs", title="ME",
      #  onClick=JS("function(btn, map){ map.locate({setView: true}); }")))|>
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Esri: WorldStreetMap", "Esri: WorldImagery"
        ),
        overlayGroups = c(
          "Property Boundary", "Compartments"
        ),
        # position it on the topleft
        position = "topleft",
        options = layersControlOptions()
      ) |>
      #addMarkers(comp, lng = ~lon, lat = ~lat,label = ~df$city
      #) |>
      addMiniMap(position="topright"
      )
    #addEasyButton(easyButton(
    #  icon="fa-crosshairs", title="comp",
    #  onClick=JS("function(btn, map){ map.locate({setView: true}); }"))
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    compartment[5,4]
    #colnames = c( "Compartment Number", "Size (acres)", "Silviculture", "Management Allocation", "Most Recent Inventory", "Last Plant Date", "Last Harvest Date")
  ))
  
  #[5,6,7,9,10,13,16]
  
})










