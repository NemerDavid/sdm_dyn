library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(shinyjs)



# ui <- fluidPage(
#   useShinyjs(),  # Include shinyjs for loading spinner
#   titlePanel("Interactive Species Map"),
# 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("species", "Select Species:", choices = NULL),  # Empty initially
#       selectInput("SSP", "Select SPP:", choices = NULL),
#       sliderInput("dot_size", "Dot Size:", min = 1, max = 10, value = 1),
# 
#       checkboxGroupInput("category",
#                          label = HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Base&nbsp;&nbsp&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;-
#                                      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Extreme:"), # Adds spaces before the label
#                          choices = NULL)  # Empty initially
#     ),
# 
#     mainPanel(
#       leafletOutput("map", height = "700px")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   # Show loading modal
#   showModal(modalDialog(
#     title = "Loading Data",
#     "Please wait while the data is being loaded...",
#     easyClose = FALSE,
#     footer = NULL
#   ))
# 
#   # Define a reactive value to store the dataset
#   diff_sf3_reactive <- reactiveVal(NULL)
# 
#   observe({
#     # Load the dataset
#     diff_sf3 <- readRDS("diff_sf3.rds")
# 
#     # Update UI inputs dynamically after loading data
#     updateSelectInput(session, "species", choices = unique(diff_sf3$species))
#     updateSelectInput(session, "SSP", choices = unique(diff_sf3$SSP))
#     updateCheckboxGroupInput(session, "category", choices = levels(diff_sf3$category), selected = levels(diff_sf3$category)[15])
# 
#     # Store in reactive container
#     diff_sf3_reactive(diff_sf3)
# 
# 
#     # Hide the loading spinner after data is ready
#     removeModal()
#   })
# 
#   # Define color mapping for categories
#   bivariate_colors <- c(
#     "Colonization - Colonization" = "#174f28",
#     "Colonization - Presence" = "#635929",
#     "Colonization - Absence" = "#a36229",
#     "Colonization - Extinction" = "#dd6a29",
#     "Presence - Colonization" = "#166d68",
#     "Presence - Extinction" = "#d9926a",
#     "Presence - Absence" = "#a08769",
#     "Extinction - Extinction" = "#00004B",
#     "Extinction - Presence" = "#5fb2d1",
#     "Extinction - Absence" = "#9cc4d2",
#     "Extinction - Colonization" = "#169dd0",
#     "Absence - Colonization" = "#16869e",
#     "Absence - Extinction" = "#d6b3a0",
#     "Absence - Presence" = "#60979f",
#     "Presence" = "#00FF7F",
#     "Absence" = "#DCDCDC"
#   )
# 
#   # Create color palette function
#   color_pal <- colorFactor(palette = bivariate_colors, domain = NULL)
# 
#   # Load world country borders (Natural Earth dataset)
#   world <- ne_countries(scale = "medium", returnclass = "sf")
# 
#   # Render Leaflet map
#   output$map <- renderLeaflet({
#     req(diff_sf3_reactive())  # Ensure data is available before proceeding
# 
#     diff_sf3 <- diff_sf3_reactive()  # Access reactive value
# 
#     # Filter data based on user input
#     filtered_data <- diff_sf3 %>%
#       filter(species == input$species & category %in% input$category & SSP==input$SSP)
# 
#     leaflet(filtered_data) %>%
#       setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%
#       addPolygons(
#         data = world,
#         color = "black", weight = 3,
#         fillColor = "white",
#         fillOpacity = 1
#       ) %>%
#       addCircleMarkers(
#         lng = ~lon, lat = ~lat,
#         color = ~color_pal(category),
#         radius = input$dot_size,
#         fillOpacity = 1, stroke = FALSE,
#         popup = ~paste("Species:", species, "<br>Category:", category)
#       ) %>%
#       addLegend(
#         "bottomright",
#         pal = color_pal,
#         values = filtered_data$category,
#         title = "Base - Extreme",
#         opacity = 1
#       )
#   })
# }
# 
# shinyApp(ui, server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 



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










                 tabPanel("Figure 5 - Biome shift",
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
  diff_sf3_reactive2 <- reactiveVal(NULL)
  
  observe({
    # Load the dataset
    diff_fig2 <- readRDS("C:/Users/nemer/Project/sdm_dyn_app_v1/sdm_dyn_app_v1/diff_sf3_fig2.rds")
    
    
    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species2", choices = unique(diff_fig2$species))
    updateSelectInput(session, "SSP2", choices = unique(diff_fig2$SSP))
    updateSelectInput(session, "Model2", choices = unique(diff_fig2$Model))
    updateCheckboxGroupInput(session, "category2", choices = levels(diff_fig2$category), selected = levels(diff_fig2$category)[3])
    
    
    # Store in reactive container
    diff_sf3_reactive2(diff_fig2)
    
    
    # Hide the loading spinner after data is ready
    removeModal()
  })
  
  
  
  
  
  # Observe "Select All" Checkbox
  observeEvent(input$select_all2, {
    if (input$select_all2) {
      updateCheckboxGroupInput(session, "category2", selected = levels(diff_sf3_reactive2()$category))
    } else {
      updateCheckboxGroupInput(session, "category2", selected = levels(diff_sf3_reactive2()$category)[3])  # Deselect all
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
    
    req(diff_sf3_reactive2())  # Ensure data is available before proceeding
    
    diff_sf3_fig2 <- diff_sf3_reactive2()  # Access reactive value
    
    
    # Filter data based on selected inputs
    filtered_data <- diff_sf3_fig2 %>%
      filter(species == input$species2 & category %in% input$category2 & SSP == input$SSP2 & Model  == input$Model2)
    
    
    #
    # # Filter data based on selected inputs
    # filtered_data <- diff_sf3 %>%
    #   filter(species == "F.sylvatica"& category %in% category & SSP == "ssp370")
    
    # Convert 'category' from factor to numeric index
    filtered_data$category <- as.factor(filtered_data$category)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)
    
    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")
    
    #r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"))
    
    
    
    r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"), res = c(0.01, 0.01))
    
    
    
    category_levels <- levels(filtered_data$category)
    
    pal2 <- colorFactor(palette = fig2_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = fig2_colors, domain = 1:3, na.color = "transparent")
    
    # # Create Leaflet Map
    # leaflet(filtered_data) %>%
    #   setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
    #   addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
    #   addRasterImage(r_filtered2, colors = pal, opacity = 0.8) %>%
    #   addLegend(
    #     "bottomright",
    #     pal = pal2,
    #     values = filtered_data$category,
    #     title = paste("category"),
    #     opacity = 0.8
    #   )
    
    
    
    
    
    
    
    
    leafletProxy("map_pa",data = filtered_data) %>%
      clearMarkers() %>%
      clearControls() %>%  # Clear previous legends
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered2, colors = pal, opacity = 0.8) %>%
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
  
  
  
  
  # # Show loading modal
  # showModal(modalDialog(
  #   title = "Loading Data",
  #   "Please wait while the data is being loaded...",
  #   easyClose = FALSE,
  #   footer = NULL
  # ))
  # 
  
  

  # Define a reactive value to store the dataset
  diff_sf3_reactive <- reactiveVal(NULL)

  observe({
    # Load the dataset



    diff_fig1 <- readRDS("C:/Users/nemer/Project/sdm_dyn_app_v1/sdm_dyn_app_v1/diff_sf3_fig1.rds")

    # Update UI inputs dynamically after loading data
    updateSelectInput(session, "species", choices = unique(diff_fig1$species))
    updateSelectInput(session, "SSP", choices = unique(diff_fig1$SSP))
    updateCheckboxGroupInput(session, "category", choices = levels(diff_fig1$category), selected = levels(diff_fig1$category)[15])

    # Store in reactive container
    diff_sf3_reactive(diff_fig1)


    # Hide the loading spinner after data is ready
    removeModal()
  })




  # Observe "Select All" Checkbox
  observeEvent(input$select_all, {
    if (input$select_all) {
      updateCheckboxGroupInput(session, "category", selected = levels(diff_sf3_reactive()$category))
    } else {
      updateCheckboxGroupInput(session, "category", selected = levels(diff_sf3_reactive()$category)[1])  # Deselect all
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

  # Create color palette function
  #color_pal <- colorFactor(palette = bivariate_colors, domain = NULL)





  # # Initialize map when app starts
  # output$map_pa <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = 0, lat = 0, zoom = 2)  # Default empty map
  # })
  



  output$map2 <- renderLeaflet({

    req(diff_sf3_reactive())  # Ensure data is available before proceeding

    diff_sf3_fig1 <- diff_sf3_reactive()  # Access reactive value









    # Filter data based on selected inputs
    filtered_data <- diff_sf3_fig1 %>%
      filter(species == input$species & category %in% input$category & SSP == input$SSP)




    # Convert 'category' from factor to numeric index
    filtered_data$category <- as.factor(filtered_data$category)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)

    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")

    #r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"))



    r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"), res = c(0.01, 0.01))


    pal2 <- colorFactor(palette = bivariate_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = bivariate_colors, domain = 1:15, na.color = "transparent")

    # Create Leaflet Map
    leaflet(filtered_data) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered2, colors = pal, opacity = 0.8) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = filtered_data$category,
        title = paste("Model                :","Base - Extreme"),
        opacity = 0.8
      )
    
    
    
    
    # leafletProxy("map_pa",data = filtered_data) %>%
    #   clearMarkers() %>%
    #   clearControls() %>%  # Clear previous legends
    #   setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
    #   addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
    #   addRasterImage(r_filtered2, colors = pal, opacity = 0.8) %>%
    #   addLegend(
    #     "bottomright",
    #     pal = pal2,
    #     values = filtered_data$category,
    #     title = paste("Model                :","Base - Extreme"),
    #     opacity = 0.8
    #   )
    
    
    
    
    
    
  })











  #########################################
  ############## Figure 5 #################






  # Define a reactive value to store the dataset
  diff_sf3_reactive3 <- reactiveVal(NULL)

  observe({
    # Load the dataset
    diff_fig3 <- readRDS("C:/Users/nemer/Project/sdm_dyn_app_v1/sdm_dyn_app_v1/diff_sf3_fig3.rds")


    # Update UI inputs dynamically after loading data

    updateSelectInput(session, "SSP3", choices = unique(diff_fig3$Model_SSP))

    updateCheckboxGroupInput(session, "category3", choices = levels(diff_fig3$category), selected = levels(diff_fig3$category)[3])


    # Store in reactive container
    diff_sf3_reactive3(diff_fig3)


    # Hide the loading spinner after data is ready
    removeModal()
  })







  # Observe "Select All" Checkbox
  observeEvent(input$select_all3, {
    if (input$select_all3) {
      updateCheckboxGroupInput(session, "category3", selected = levels(diff_sf3_reactive3()$category))
    } else {
      updateCheckboxGroupInput(session, "category3", selected = levels(diff_sf3_reactive3()$category)[1])  # Deselect all
    }
  })




  biome_colors <- c(
    "Med credit" = "#f4a582",
    "Shifted to med" = "darkgreen",
    "Med" = "darkred",
    "Mixed" = "lightgreen",
    "Mixed credit"="yellow",
    "Temp credit" = "darkblue",
    "Temp loss" = "#92c5de",
    "Temp" = "black",
    "Non-Forest"="white",
    "Other-Forest"="darkgray"
  )
  
  
  
  # # Initialize map when app starts
  # output$map_pa3 <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = 0, lat = 0, zoom = 2)  # Default empty map
  # })
  
  
  

  output$map3 <- renderLeaflet({


    req(diff_sf3_reactive3())  # Ensure data is available before proceeding
    diff_sf3_fig3 <- diff_sf3_reactive3()  # Access reactive value



    # Filter data based on selected inputs
    filtered_data <- diff_sf3_fig3 %>%
      filter(category %in% input$category3 & Model_SSP == input$SSP3)
    filtered_data$category_numeric <- as.numeric(filtered_data$category)

    # Create raster using numeric category values
    r_filtered <- rasterFromXYZ(filtered_data[, c("x", "y", "category_numeric")])
    crs(r_filtered) <- CRS("+init=epsg:2154")

    r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"))



    r_filtered2 <- projectRaster(r_filtered, crs = CRS("+proj=longlat +datum=WGS84"), res = c(0.01, 0.01))



    category_levels <- levels(filtered_data$category)

    pal2 <- colorFactor(palette = biome_colors, domain = filtered_data$category, na.color = "transparent")
    pal <- colorFactor(palette = biome_colors, domain = 1:9, na.color = "transparent")

    # Create Leaflet Map
    leaflet(filtered_data) %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
      addRasterImage(r_filtered2, colors = pal, opacity = 1) %>%
      addLegend(
        "bottomright",
        pal = pal2,
        values = filtered_data$category,
        title = paste("category"),
        opacity = 1
      )
    
    
    
    
    # 
    # leafletProxy("map_pa3",data = filtered_data) %>%
    #   clearMarkers() %>%
    #   clearControls() %>%  # Clear previous legends
    #     setView(lng = 1.888334, lat = 46.603354, zoom = 6) %>%  # Focus on France
    #     addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grayscale") %>%
    #     addRasterImage(r_filtered2, colors = pal, opacity = 1) %>%
    #     addLegend(
    #       "bottomright",
    #       pal = pal2,
    #       values = filtered_data$category,
    #       title = paste("category"),
    #       opacity = 1
    #     )
    
    
    
    
    
    
    
    
    
    
    
  })






}

shinyApp(ui, server)











