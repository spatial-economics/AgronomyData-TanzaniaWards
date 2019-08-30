#
# A Portal for Soil PH data in Tanzania Wards.

#### Required Libraries
library(shiny)
library(shinydashboard)

library(raster)
library(rgdal)

library(DT)
library(tmap)
library(tmaptools)
library(leaflet)


#### DATA
# Load District and wards shapefile
TZA_level2 <- shapefile("./data/gadm36_TZA_2.shp")
TZA_level3 <- shapefile("./data/TZA_level3_data.shp")

# Prepare the Data
# Fix Column headings
TZA_level3@data$AREA_sq <- round(TZA_level3@data$AREA_sq, digits = 2)
TZA_level3@data$Crp_Cv_ <- round(TZA_level3@data$Crp_Cv_, digits = 2)
TZA_level3@data$PH <- round(TZA_level3@data$PH, digits = 2)
names(TZA_level3) <- c("Region", "District", "Ward", 
                       "AREA_sqkm", "Crop Cover_perc", "PH")
# Create a map wiget
TZA_level2_tm <-  tm_shape(TZA_level2) + 
                  tm_borders() #+
                  # tm_shape(TZA_SoilPH) + 
                  # tm_raster("PH", palette = get_brewer_pal("YlGnBu", 
                  #                                          n = 4, 
                  #                                          contrast = c(0.09, 0.41)
                  #                                          )
                  #           ) 
# + tm_text("Region") 


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
          leafletOutput("picked_wards"),
          tags$h6("NB: Click on a Ward for more information", style = "color: green")
      )
    ),
    
    # Information on wards in selected district
    fluidRow( 
      column = 6,
      # ui4:check serv4
      box(title = "Wards in Selected District",
          DTOutput('wards_tbl')
          ),
      # ui3:check serv3
      box(title = "Highligted Ward(s)",
          #verbatimTextOutput("picked_rows")
          DTOutput("picked_rows"),
          downloadLink('downloadCSV', 'Download CSV |')
          # downloadLink('downloadSHP', '| Download shapefile')
          )
      )
    )
  )


#### SERVER LOGIC
server <- function(input, output, session) {
  
  # serv1: outputs to ui1
  # Create a Dynamic Districts dropdown
  output$districtUI = renderUI({
    choices_district <- TZA_level3@data[is.element(TZA_level3@data$Region, 
                                                  input$region), 
                                       "District"]
    choices_district <- choices_district[!grepl("Lake", choices_district)]
    selectInput("district", "Select District", choices_district)
  })
  
  # serv2: outputs to ui2 
  # Display information on wards in the selected district in a Table
  wards_in_district <- eventReactive(input$zoom, 
                                     {TZA_level3@data[is.element(TZA_level3@data$District,
                                                                 input$district
                                     ),
                                     ]
                                     }
  )
  output$wards_tbl <- renderDT(wards_in_district()) # Render Dynamic table
  
  # serv3: outputs to ui3 
  # Show more details on wards highligted in the table
  output$picked_rows <- renderDT({
    slctd_dscts <- TZA_level3[is.element(TZA_level3@data$District,input$district),]
    slctd_dscts <- slctd_dscts[input$wards_tbl_rows_selected,]
    slctd_dscts@data
    
  })
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0('TZA_SlctWardsPH_', 
             gsub("-", "", Sys.Date(), fixed = TRUE), "_",
             gsub(":", "", format(Sys.time(), "%H:%M:%S"), fixed = TRUE),
             '.csv'
      )
    },
    content = function(con) {
      slctd_dscts_data <- TZA_level3[is.element(TZA_level3@data$District,input$district),]
      slctd_dscts_data <- slctd_dscts_data[input$wards_tbl_rows_selected,]
      write.csv(slctd_dscts_data@data, con)
    }
  )
  
  # serv4: outputs to ui4 
  # Select a district to focus on 
  selected_district <- eventReactive(
    input$zoom,
    {
      # Get the Selected district extent
      slctd_dscts <- TZA_level3[is.element(TZA_level3@data$District,input$district),]
      
      # Create map of selected districts
      mainmap <- tm_shape(slctd_dscts) + tm_borders() + TZA_level2_tm + 
                 tm_shape(slctd_dscts,) + tm_borders() + 
                 tm_fill(col = "PH",
                         popup.vars = c("Ward", "AREA_sqkm", "Crop Cover_perc", "PH")
                         ) +
                  tm_scale_bar(width = 0.5) 
      tmap_leaflet(mainmap) %>% 
        addMiniMap()
      
    }, ignoreNULL = FALSE
  )
  # Render Dynamic map
  output$picked_wards <- renderLeaflet(selected_district())
  
  
  
}

# Run application 
shinyApp(ui = ui, server = server)
