#
# A Portal for Soil PH data in Tanzania Wards.

#### Required Libraries
library(shiny)
library(shinydashboard)

library(raster)
library(rgdal)

library(DT)
library(leaflet)

# Used to create North arrow
# library(maptools)
# library(cleangeo)
# library(devtools)

#### DATA
# Load PH raster and wards shapefile
TZA_SoilPH <- raster("./data/PH.tif")
TZA_level3 <- shapefile("./data/TZA_level3_data.shp")

# Prepare the Data
# Fix Column headings
TZA_level3@data$AREA_sq <- round(TZA_level3@data$AREA_sq, digits = 2)
TZA_level3@data$Crp_Cv_ <- round(TZA_level3@data$Crp_Cv_, digits = 2)
TZA_level3@data$PH <- round(TZA_level3@data$PH, digits = 2)
names(TZA_level3) <- c("Region", "Disrict", "Ward", "AREA_sqkm", "Crop Cover_perc", "PH")
# Create a map wiget
TZA_level3_leaflet <- leaflet(TZA_level3, options = leafletOptions(minZoom = 5.5, maxZoom = 10)) %>% 
  setView(lng = 35, lat = -6, zoom = 7) %>% 
  addTiles() %>% 
  addPolygons(layerId = 1,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5, 
              fillOpacity = 0.1,
              #fillColor = "brown", #~colorQuantile("YlOrRd", Disrict)(Disrict),
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE))


##### USER INTERFACE
ui <- dashboardPage(
  dashboardHeader(title = "Tanzania Wards Soil PH Portal", titleWidth = 320),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    # District selection and display
    fluidRow( 
      # ui1:check serv1
      box(title = "Select District of Intrest",
          selectInput("region", "Select Region", unique(TZA_level3@data$Region)),
          uiOutput("districtUI"),
          actionButton("zoom", "Show Wards")
          ),
      # ui2:check serv2
      box(title = "Selected Tanzania Wards",
          leafletOutput("picked_wards")
          )
    ),
    
    # Information on wards in selected district
    fluidRow( 
      column = 6,
      # ui3:check serv3
      box(title = "Highligted Ward(s)",
          #verbatimTextOutput("picked_rows")
          DTOutput("picked_rows")
          ),
      # ui4:check serv4
      box(title = "Wards in Selected District",
          DTOutput('wards_tbl')
          )
      )
    )
  )


#### SERVER LOGIC
server <- function(input, output, session) {
  
  # serv1: outputs to ui1
  # Create a Dynamic Districts dropdown
  output$districtUI = renderUI({
    choices_district = TZA_level3@data[is.element(TZA_level3@data$Region, 
                                                  input$region), 
                                       "Disrict"]
    selectInput("district", "Select District", choices_district, selected = choices_district[1])
  })
  
  # serv2: outputs to ui2 
  # Display information on wards in the selected district in a Table
  wards_in_district <- eventReactive(input$zoom, 
                                     {TZA_level3@data[is.element(TZA_level3@data$Disrict,
                                                                 input$district
                                     ),
                                     ]
                                     }
  )
  output$wards_tbl <- renderDT(wards_in_district()) # Render Dynamic table
  
  # serv3: outputs to ui3 
  # Show more details on wards highligted in the table
  output$picked_rows <- renderDT({
    slctd_dscts <- TZA_level3[is.element(TZA_level3@data$Disrict,input$district),]
    slctd_dscts <- slctd_dscts[input$wards_tbl_rows_selected,]
    #print(input$wards_tbl_rows_selected)
    slctd_dscts@data
    
  })

  # serv4: outputs to ui4 
  # Select a district to focus on 
  selected_district <- eventReactive(
    input$zoom,
    {
      # Get the Selected district extent
      slctd_dscts <- TZA_level3[is.element(TZA_level3@data$Disrict,input$district),]
      district_extent <- extent(slctd_dscts)
      distWardsCol <- colorFactor(topo.colors(length(TZA_level3)), slctd_dscts@data$Ward)
      
      # Create a north arrow
      arrow1 <-  layout.north.arrow(type = 1) 
      # shift the coordinates 
      # shift = c(x,y) direction
      Narrow1 <- maptools::elide(arrow1, shift = c(
        district_extent[2],
        district_extent[4]))
      proj4string(Narrow1) <- proj4string(slctd_dscts)
      
      # Create list of popup labels
      labels_list <- lapply(seq(nrow(slctd_dscts@data)), function(i) {
        paste0( '<p>', "Ward: ", slctd_dscts@data[i, "Ward"], '</p><p>', 
                "AREA (sqkm): ",slctd_dscts@data[i, "AREA_sqkm"], '</p><p>', 
                "Crop Cover (%): ",slctd_dscts@data[i, "Crop Cover_perc"],'</p><p>', 
                "PH: ",slctd_dscts@data[i, "PH"], '</p>' ) 
      })
      
      # Add Selected districts to the map widget
      slctd_dscts_map <- TZA_level3_leaflet %>% 
        addPolygons(data = slctd_dscts,
                    layerId = 2,
                    color = "#444444",
                    label = lapply(labels_list, htmltools::HTML),
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 0.2,
                    fillColor = ~distWardsCol(Ward),
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                       bringToFront = TRUE)) 
      # Zoom to the selected district in a map
      slctd_dscts_map %>%
        fitBounds(district_extent[1],
                  district_extent[3],
                  district_extent[2],
                  district_extent[4]) %>% 
       # setMaxBounds( lng1 = 29.32717
       #                , lat1 = -11.74570
       #                , lng2 = 40.4451370
       #                , lat2 = -0.9857875 )  %>% 
        #addPolygons(data = Narrow1 ) %>%
        addMiniMap() %>% addScaleBar(position = "bottomleft") %>% addGraticule(interval = 1, 
                                                                               style = list(color = "brown", 
                                                                                            weight = 1))
      }
    )
  # Render Dynamic map
  output$picked_wards <- renderLeaflet(selected_district())

  
}

# Run application 
shinyApp(ui = ui, server = server)
