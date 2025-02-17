

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(shinyjs)



ui <- fluidPage(
  useShinyjs(),  # Include shinyjs for loading spinner
  titlePanel("Interactive Species Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", choices = NULL),  # Empty initially
      sliderInput("dot_size", "Dot Size:", min = 1, max = 10, value = 1),
      
      checkboxGroupInput("category",    
                         label = HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Base&nbsp;&nbsp&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-
                                     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Extreme:"), # Adds spaces before the label
                         choices = NULL)  # Empty initially
    ),
    
    mainPanel(
      leafletOutput("map", height = "700px")  
    )
  )
)


server <- function(input, output, session) {
  
  # Use shinyjs to show loading spinner
  showModal(modalDialog(
    title = "Loading Data",
    "Please wait while the data is being loaded...",
    easyClose = FALSE,
    footer = NULL
  ))
  
  # Load and process data asynchronously
  observe({
    # Load the data
    diff_sf3 <- readRDS("diff_sf3.rds")
    #diff_sf <- st_as_sf(All_sp_df3, coords = c("x", "y"), crs = "+init=epsg:2154")
    #diff_sf3 <- st_transform(subset(diff_sf, category != "Absence"), crs = 4326)
    
    # # Extract coordinates for Leaflet
    # diff_sf3 <- diff_sf3 %>%
    #   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])
    # 
    # # Factor conversion for species and category
    # diff_sf3$species <- factor(diff_sf3$species, levels = c(
    #   "1" = "F.sylvatica",
    #   "2" = "Q.petraea",
    #   "3" = "Q.robur",
    #   "4" = "Q.pubescens",
    #   "5" = "Q.ilex"
    # ))
    # 
    # diff_sf3$category <- factor(diff_sf3$category, levels = c(
    #   "Colonization - Colonization", 
    #   "Colonization - Presence", 
    #   "Colonization - Absence", 
    #   "Colonization - Extinction",
    #   "Presence - Colonization", 
    #   "Presence - Extinction", 
    #   "Presence - Absence", 
    #   "Extinction - Extinction", 
    #   "Extinction - Presence",
    #   "Extinction - Absence", 
    #   "Extinction - Colonization",
    #   "Absence - Colonization", 
    #   "Absence - Extinction",  
    #   "Absence - Presence", 
    #   "Presence", 
    #   "Absence"
    # ))
    
    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species", choices = unique(diff_sf3$species))
    updateCheckboxGroupInput(session, "category", choices = levels(diff_sf3$category), selected = levels(diff_sf3$category)[15])
    
    # Hide the loading spinner after the data is ready
    removeModal()
    
    # Store the processed data globally to use for filtering
    assign("diff_sf3", diff_sf3, envir = .GlobalEnv)
  })
  
  # Define color mapping for categories
  bivariate_colors <- c(
    "Colonization - Colonization" = "#174f28",  
    "Colonization - Presence" = "#635929",  
    "Colonization - Absence" = "#a36229",
    "Colonization - Extinction" = "#dd6a29", 
    "Presence - Colonization" = "#166d68",  
    "Presence - Extinction" = "#d9926a",
    "Presence - Absence" = "#a08769", 
    "Extinction - Extinction" = "#00004B",
    "Extinction - Presence" = "#5fb2d1",  
    "Extinction - Absence" = "#9cc4d2",  
    "Extinction - Colonization" = "#169dd0",
    "Absence - Colonization" = "#16869e",
    "Absence - Extinction" = "#d6b3a0",  
    "Absence - Presence" = "#60979f",   
    "Presence" = "#00FF7F",
    "Absence" = "#DCDCDC"
  )
  
  # Create color palette function
  color_pal <- colorFactor(palette = bivariate_colors, domain = NULL)
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    # Ensure diff_sf3 is available in global environment
    diff_sf3 <- get("diff_sf3", envir = .GlobalEnv)
    
    # Filter data based on user input (species and category)
    filtered_data <- diff_sf3 %>%
      filter(species == input$species & category %in% input$category)
    
    leaflet(filtered_data) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Minimal") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addLayersControl(
        baseGroups = c("Minimal", "Grayscale", "Topographic"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        color = ~color_pal(category),
        radius = input$dot_size,
        fillOpacity = 1, stroke = FALSE,
        popup = ~paste("Species:", species, "<br>Category:", category)
      ) %>%
      addLegend(
        "bottomright", 
        pal = color_pal, 
        values = filtered_data$category,  
        title = "Base - Extreme",
        opacity = 1
      )
  })
}

shinyApp(ui, server)








