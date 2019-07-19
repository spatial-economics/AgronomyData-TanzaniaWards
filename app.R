#
# A Portal for Soil PH data in Tanzania Wards.

library(shiny)
library(shinydashboard)

library(raster)
library(rgdal)

library(DT)
library(leaflet)

# Data
TZA_SoilPH <- raster("./data/PH.tif")
TZA_level3 <- shapefile("./data/gadm36_TZA_3.shp")

TZA_level3_leaflet <- leaflet(TZA_level3, options = leafletOptions(minZoom = 4, maxZoom = 10)) %>% 
  setView(lng = 35, lat = -6, zoom = 5) %>% 
  addPolygons(layerId = 1,
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              fillColor = "brown", #~colorQuantile("YlOrRd", NAME_2)(NAME_2),
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE))
TZA_level3_tbl <- TZA_level3@data


# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Tanzania Wards Soil PH Portal", titleWidth = 320),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow( box(title = "Select Tanzania Wards",
                  selectInput("region", "Select a Region", unique(TZA_level3@data$NAME_1)),
                  uiOutput("districtUI"),
                  actionButton("zoom", "Move to District")),
              box(title = "Selected Tanzania Wards",
                  leafletOutput("picked_wards")
              )
    ),
    fluidRow( column = 6,
              box(title = "Selected Wards Information",
                  verbatimTextOutput("picked_rows")),
              box(title = "Tanzania Wards",
                  DTOutput('wards_tbl'))
    )
    
  )
)


# Server logic 
server <- function(input, output, session) {
  
  # Dynamic Districts dropdown
  output$districtUI = renderUI({
    choices_district = TZA_level3@data[is.element(TZA_level3@data$NAME_1, 
                                                  input$region), 
                                       "NAME_2"]
    selectInput("district", "Select a District", choices_district, selected = choices_district[1 ])
  })
  
  # Display wards in selected district in a Table
  wards_in_district <- eventReactive(input$zoom, 
                                     {TZA_level3@data[is.element(TZA_level3@data$NAME_2,
                                                                 input$district
                                     ),
                                     ]
                                     }
  )
  output$wards_tbl <- renderDT(wards_in_district()) # Render Dynamic table
  
  
  # Show Selected Wards
  output$picked_rows <- renderPrint({
    print(input$wards_tbl_rows_selected)
  })
  
  seleted_district <- eventReactive(input$zoom,
                                    {district_extent <-
                                        extent(TZA_level3[
                                          is.element(TZA_level3@data$NAME_2,input$district), 
                                          ])
                                      TZA_level3_leaflet %>%
                                        fitBounds(district_extent[1],
                                                  district_extent[3],
                                                  district_extent[2],
                                                  district_extent[4])
                                    })
  output$picked_wards <- renderLeaflet(seleted_district())# Render Dynamic map
  
}

# Run application 
shinyApp(ui = ui, server = server)