required_packages <- c(
  "shiny", "leaflet", "sf", "dplyr", "rnaturalearth", "rnaturalearthdata",
  "tidyverse", "shinyjs", "terra", "raster"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

install_if_missing(required_packages)




library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(shinyjs)
library(terra)
library(sf)
library(raster)

ui <- navbarPage("Species Distribution Modeling",




                 
                 
                 tabPanel("Figure 3",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Visualization Parameters"),
                              selectInput("species2", "Select Species:", choices = NULL),
                              selectInput("SSP2", "Select SSP:", choices = NULL),
                              selectInput("Model2", "Select Model:", choices = NULL),
                              
                              # "Select All" Checkbox
                              checkboxInput("select_all2", "Select all", value = FALSE),
                              
                              # Checkbox Group Input for Categories
                              checkboxGroupInput("category2",
                                                 label = HTML("category"), # Adds spaces before the label
                                                 choices = NULL)
                              
                              
                              
                              
                              
                              
                              
                            ),
                            
                    
                   
                          
                            mainPanel(
                              leafletOutput("map_pa", width = "100%", height = 800),
                              leafletOutput("map", width = "100%", height = 800)
                            )
                          )
                 ),
                 
                 
                 
                 

                 tabPanel("Figure 4",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Visualization Parameters"),

                              selectInput("species", "Select Species:", choices = NULL),  # Empty initially
                              selectInput("SSP", "Select SPP:", choices = NULL),
                              # sliderInput("dot_size", "Dot Size:", min = 1, max = 10, value = 1),
                              # "Select All" Checkbox
                              checkboxInput("select_all", "Select all", value = FALSE),

                              checkboxGroupInput("category",
                                                 label = HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Base&nbsp;&nbsp&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-
                                     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Extreme:"), # Adds spaces before the label
                                                 choices = NULL)  # Empty initially







                            ),
                            mainPanel(
                              #leafletOutput("map_pa2", width = "100%", height = 800),
                              leafletOutput("map2", width = "100%", height = 800)
                            )
                          )
                 ),










                 tabPanel("Figure 5-A) Biome shift",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Visualization Parameters"),

                              selectInput("SSP3", "Select SSP:", choices =NULL),

                              # "Select All" Checkbox
                              checkboxInput("select_all3", "Select all", value = FALSE),

                              # Checkbox Group Input for Categories
                              checkboxGroupInput("category3",
                                                 label = HTML("category"), # Adds spaces before the label
                                                 choices = NULL)




                            ),
                            mainPanel(
                              #leafletOutput("map_pa3", width = "100%", height = 800),
                              leafletOutput("map3", width = "100%", height = 800)
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Figure 5-B) Agreement plot",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Visualization Parameters"),
                              
                              selectInput("SSP4", "Select SSP:", choices = c("Loading...")),
                              
                              
                              radioButtons("category4", "Category", choices = c("Loading..."))
                              
                              
                              
                            ),
                            mainPanel(
                              #leafletOutput("map_pa3", width = "100%", height = 800),
                              leafletOutput("map4", width = "100%", height = 800)
                            )
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


  #########################################
  ############## Figure 3 #################
 
  # Show loading modal
  showModal(modalDialog(
    title = "Loading Data",
    "Please wait while the data is being loaded...",
    easyClose = FALSE,
    footer = NULL
  ))
  
  
  # Define a reactive value to store the dataset
  diff_reactive2 <- reactiveVal(NULL)
  
  observe({
    # Load the dataset
    diff_fig2 <- readRDS("diff_sf3_fig2.rds")
    
    
    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species2", choices = unique(diff_fig2$species))
    updateSelectInput(session, "SSP2", choices = unique(diff_fig2$SSP))
    updateSelectInput(session, "Model2", choices = unique(diff_fig2$Model))
    updateCheckboxGroupInput(session, "category2", choices = levels(diff_fig2$category), selected = levels(diff_fig2$category)[3])
    
    
    # Store in reactive container
    diff_reactive2(diff_fig2)
    
    
    # Hide the loading spinner after data is ready
    removeModal()
  })
  

  
  # Observe "Select All" Checkbox
  observeEvent(input$select_all2, {
    if (input$select_all2) {
      updateCheckboxGroupInput(session, "category2", selected = levels(diff_reactive2()$category))
    } else {
      updateCheckboxGroupInput(session, "category2", selected = levels(diff_reactive2()$category)[3])  # Deselect all
    }
  })

  
  fig2_colors= c(#"Absence" = "white",       # No forest
    #"Forest" = "gray",    # Forest present (with transparency)
    "Loss" = "darkblue",       # Existing category colors
    "Maintained" = "saddlebrown", # More natural color (brown/olive)
    "Gained" = "green")
  

  
  # Initialize map when app starts
  output$map_pa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Default empty map
  })
 
  
  output$map <- renderLeaflet({
    
    req(diff_reactive2())  # Ensure data is available before proceeding
    
    diff_fig2 <- diff_reactive2()  # Access reactive value
    
    
    # Filter data based on selected inputs
    filtered_data <- diff_fig2 %>%
      filter(species == input$species2 & category %in% input$category2 & SSP == input$SSP2 & Model  == input$Model2)
    
  
    # Convert 'category' from factor to numeric index
    filtered_data$category <- as.factor(filtered_data$category)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)
    
    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")
    
  
    
    #r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"), res = c(0.01, 0.01))
    
    
    
    category_levels <- levels(filtered_data$category)
    
    pal2 <- colorFactor(palette = fig2_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = fig2_colors, domain = 1:3, na.color = "transparent")
   
    leafletProxy("map_pa",data = filtered_data) %>%
      clearMarkers() %>%
      clearControls() %>%  # Clear previous legends
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered, colors = pal, opacity = 0.8) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = filtered_data$category,
        title = paste("category"),
        opacity = 0.8
      )
    
    
    
  })

  
  #########################################
  #########################################
  ############## Figure 4 #################

  

  # Define a reactive value to store the dataset
  diff_reactive <- reactiveVal(NULL)

  observe({
    # Load the dataset

    diff_fig1 <- readRDS("diff_sf3_fig1.rds")

    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species", choices = unique(diff_fig1$species))
    updateSelectInput(session, "SSP", choices = unique(diff_fig1$SSP))
    updateCheckboxGroupInput(session, "category", choices = levels(diff_fig1$category), selected = levels(diff_fig1$category)[15])

    # Store in reactive container
    diff_reactive(diff_fig1)


    # Hide the loading spinner after data is ready
    removeModal()
  })


  # Observe "Select All" Checkbox
  observeEvent(input$select_all, {
    if (input$select_all) {
      updateCheckboxGroupInput(session, "category", selected = levels(diff_reactive()$category))
    } else {
      updateCheckboxGroupInput(session, "category", selected = levels(diff_reactive()$category)[1])  # Deselect all
    }
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
    "Presence" = "#00FF7F"#,
    #"Absence" = "#DCDCDC"
  )

  output$map2 <- renderLeaflet({

    req(diff_reactive())  # Ensure data is available before proceeding

    diff_fig1 <- diff_reactive()  # Access reactive value


    # Filter data based on selected inputs
    filtered_data <- diff_fig1 %>%
      filter(species == input$species & category %in% input$category & SSP == input$SSP)


    # Convert 'category' from factor to numeric index
    filtered_data$category <- as.factor(filtered_data$category)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)

    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")

  
   # r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"), res = c(0.01, 0.01))


    pal2 <- colorFactor(palette = bivariate_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = bivariate_colors, domain = 1:15, na.color = "transparent")

    # Create Leaflet Map
    leaflet(filtered_data) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered, colors = pal, opacity = 0.8) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = filtered_data$category,
        title = paste("Model                :","Base - Extreme"),
        opacity = 0.8
      )

  })


  #########################################
  ############## Figure 5 #################
  
  ######------ Biomshift plot ------#####
  
  
  # Define a reactive value to store the dataset
  diff_reactive3 <- reactiveVal(NULL)

  observe({
    # Load the dataset
    diff_fig3 <- readRDS("diff_sf3_fig3.rds")


    # Update UI inputs dynamically after loading data

    updateSelectInput(session, "SSP3", choices = unique(diff_fig3$Model_SSP))

    updateCheckboxGroupInput(session, "category3", choices = levels(diff_fig3$category), selected = levels(diff_fig3$category)[3])


    # Store in reactive container
    diff_reactive3(diff_fig3)


    # Hide the loading spinner after data is ready
    removeModal()
  })







  # Observe "Select All" Checkbox
  observeEvent(input$select_all3, {
    if (input$select_all3) {
      updateCheckboxGroupInput(session, "category3", selected = levels(diff_reactive3()$category))
    } else {
      updateCheckboxGroupInput(session, "category3", selected = levels(diff_reactive3()$category)[1])  # Deselect all
    }
  })





  
  
  biome_colors <- c(
    "Med credit" = "#f4a582",
    "Shifted to med" = "darkgreen",
    "Med" = "darkred",
    "Mixed credit"="yellow",
    "Mixed" = "lightgreen",
    "Temp credit" = "darkblue",
    "Temp" = "black",
    "Temp debt" = "#1137de",
    "Temp loss" = "#92c5de",
    "Other-Forest"="darkgray"
  )
  
  
  
  
  
  
  
  
  
  
  

  output$map3 <- renderLeaflet({


    req(diff_reactive3())  # Ensure data is available before proceeding
    diff_fig3 <- diff_reactive3()  # Access reactive value



    # Filter data based on selected inputs
    filtered_data <- diff_fig3 %>%
      filter(category %in% input$category3 & Model_SSP == input$SSP3)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)

    

    
    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")



    category_levels <- levels(filtered_data$category)

    pal2 <- colorFactor(palette = biome_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = biome_colors, domain = 1:10, na.color = "transparent")

    


    # Create Leaflet Map
    leaflet(filtered_data) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered, colors = pal, opacity = 1) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = filtered_data$category,
        title = paste("category"),
        opacity = 1
      )
    
  })

    
    
    
    
    
    
    
    
    
    
    
    
    #########################################
    ############## Figure 6 #################
    
    ######------ Agreement plot ------#####
    
    
    
    
    
    
    diff_reactive4 <- reactiveVal(NULL)
    
    observe({
      
      
      diff_fig4 <- readRDS("diff_sf3_fig4.rds")
      
      
      # req(diff_fig4)  # Make sure it's not NULL
      
      updateSelectInput(session, "SSP4", choices = unique(diff_fig4$SSP))
      
      categories <- sort(unique(subset(diff_fig4, !cat_Base %in% c("Other-Forest", "Non-Forest"))$cat_Base))
      
      updateRadioButtons(session, "category4", choices = categories, selected = categories[1])
      
      diff_reactive4(diff_fig4)
    })
    
    
    
    color_fig4 = c("Both agree" = "darkgreen","Base only" = "#FDE725", "Extreme only" = "#440154")
    
    output$map4 <- renderLeaflet({
      
      req(diff_reactive4())  # Ensure data is available before proceeding
      diff_fig4 <- diff_reactive4()  # Access reactive value
      
      
      filtered_data <- diff_fig4 %>%
        filter(SSP == input$SSP4) %>%
        mutate(agreement = case_when(
          cat_Base == input$category4 & cat_Extreme == input$category4 ~ "Both agree",
          cat_Base == input$category4 & cat_Extreme != input$category4 ~ "Base only",
          cat_Base != input$category4 & cat_Extreme == input$category4 ~ "Extreme only",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(agreement))  # Keep only relevant pixels
      
      
      
      
      # Convert `mean_ens` to factor with proper labels
      filtered_data$agreement <- factor(filtered_data$agreement,
                                        levels = c(
                                          "Both agree",
                                          "Base only" , "Extreme only"))
      
      
      filtered_data$category_numeric <- as.numeric(filtered_data$agreement)
      
      # Create raster using numeric category values
      r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
      crs(r_filtered) <- CRS("+init=epsg:2154")
      
      
      category_levels <- levels(filtered_data$agreement)
      
      pal2 <- colorFactor(palette = color_fig4, domain = filtered_data$agreement, na.color = "transparent")
      pal <- colorFactor(palette = color_fig4, domain = 1:3, na.color = "transparent")
      
      # Create Leaflet Map
      leaflet(filtered_data) %>%
        setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
        addRasterImage(r_filtered, colors = pal, opacity = 1) %>%
        addLegend(
          "bottomright",
          pal = pal2,
          values = filtered_data$agreement,
          title = paste("Agreement"),
          opacity = 1
        )
 
})
    
  }

shinyApp(ui, server)











