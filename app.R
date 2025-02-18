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
      selectInput("SSP", "Select SPP:", choices = NULL),
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
  
  # Show loading modal
  showModal(modalDialog(
    title = "Loading Data",
    "Please wait while the data is being loaded...",
    easyClose = FALSE,
    footer = NULL
  ))
  
  # Define a reactive value to store the dataset
  diff_sf3_reactive <- reactiveVal(NULL)
  
  observe({
    # Load the dataset
    diff_sf3 <- readRDS("diff_sf3.rds")
    
    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species", choices = unique(diff_sf3$species))
    updateSelectInput(session, "SSP", choices = unique(diff_sf3$SSP))
    updateCheckboxGroupInput(session, "category", choices = levels(diff_sf3$category), selected = levels(diff_sf3$category)[15])
    
    # Store in reactive container
    diff_sf3_reactive(diff_sf3)
    
    
    # Hide the loading spinner after data is ready
    removeModal()
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
  
  # Load world country borders (Natural Earth dataset)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    req(diff_sf3_reactive())  # Ensure data is available before proceeding
    
    diff_sf3 <- diff_sf3_reactive()  # Access reactive value
    
    # Filter data based on user input
    filtered_data <- diff_sf3 %>%
      filter(species == input$species & category %in% input$category & SSP==input$SSP)
    
    leaflet(filtered_data) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%
      addPolygons(  
        data = world,
        color = "black", weight = 3,
        fillColor = "white",
        fillOpacity = 1
      ) %>%
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
