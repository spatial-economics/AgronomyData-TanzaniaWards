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
                  fluidRow(
                           box(title = "Selected Tanzania Wards",
                               leafletOutput("picked_wards")),
                           box(title = "Selected Wards Information",
                               verbatimTextOutput("picked_rows"))
                           ),
                  fluidRow( column = 12,
                    box(title = "Tanzania Wards",
                               DTOutput('wards_tbl'))
      
      )
      
    )
)

# Server logic 
server <- function(input, output) {
  
  
  output$wards_tbl <- renderDT(TZA_level3_tbl)
  
  output$picked_rows <- renderPrint({
    print("dfghj")
    print(input$wards_tbl_rows_selected)
    })
  
  output$picked_wards <- renderLeaflet(TZA_level3_leaflet %>% 
                                         addPolygons( layerId = 2,
                                                      data = TZA_level3[input$wards_tbl_rows_selected,],
                                                      color = "#444444",
                                                      weight = 1,
                                                      smoothFactor = 0.5,
                                                      opacity = 1.0,
                                                      fillOpacity = 0.5,
                                                      fillColor = "yellow"
                                                      )
                                       )

}

# Run application 
shinyApp(ui = ui, server = server)
