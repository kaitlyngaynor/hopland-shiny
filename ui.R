# Hopland Camera Trap Data Shiny App

# User interface

source("global.R")


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("By Species", tabName = "species"),
    menuItem("Pairwise Comparison", tabName = "pairwise_compare")
  ),
  
  tags$footer(
    p(
      "Developed by ",
      a(href = 'https://www.kaitlyngaynor.com/', "Kaitlyn Gaynor.")
    ),

    align = "left",
    style = "
            position:absolute;
            bottom:0;
            width:100%;
            height:50px; /* Height of the footer */
            color: white;
            padding: 10px;
            background-color: black;
            z-index: 1000;"
  )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  
  tabItems(
    

# Overview ----------------------------------------------------------------

    tabItem(
      
      tabName = "overview",
      
      fluidRow(
        box(width = 12,
          h1("Hopland Camera Traps"),
          "This dashboard facilitates exploration of the first 'phase' of camera trap data (mid-2016 through 2017) 
          from the systematic grid of the Hopland Research and Extension Center, California. These data were collected, 
          classified, and cleaned by Kaitlyn Gaynor, Alex McInturff, and a number of field associates and undergraduate volunteers,
          supervised by PI Justin Brashares.")
      ),
      
      fluidRow(
        box(width = 12,
            title = "Camera trap study design",
            status = "primary",
            "A) The study was conducted at the Hopland Research and Extension Center in Mendocino County, California (indicated with the blue star). 
            B) Detail of study site, showing vegetation cover, locations of gridded camera traps, buildings and parking, and areas where hunting is prohibited (diagonal lines).",
            div(img(src="https://i.ibb.co/T2RwHVD/Study-area-CLEAN.png", 
                    width=600), style = "text-align: center;"),
            "To evaluate spatiotemporal patterns of mammal activity in the Hopland Research and Extension Center, we conducted a systematic camera trap survey.
            We deployed 36 Reconyx HyperFire camera traps systematically in a grid. We placed cameras at the center points of hexagonal grid cells, with each 
            camera located 750 meters away from its six nearest neighbors. After navigating to the pre-determined GPS location in each grid cell, we identified 
            a suitable camera location within 50 meters of that location. When possible, we pointed cameras towards animal trails to maximize detections. All 
            cameras were placed in security boxes attached to trees. Cameras were motion-activated, and took three consecutive photographs per detection event, 
            with a 30-second refractory interval between detection events."
            )
      )
      
    ),

# By species --------------------------------------------------------------
  
    tabItem(
  
      tabName = "species",
      
      fluidRow(
        box(h2("INDIVIDUAL SPECIES PATTERNS"), width = 12)
      ),
      
      fluidRow(
        
        box(
          title = "Choose a species:",
          selectInput(inputId = "species_select",
                      label = "",
                      selected = "Deer",
                      choices = sort(unique(records$Species)))
        ),
        
        box(
          title = "Subset records further (optional)",
          
          dateRangeInput(inputId = "date_range",
                         label = "Date Range:",
                         start = "2016-04-01",
                         end = "2017-12-22"),
          "The first camera was set on March 24, 2016, and the last camera was checked on December 22, 2017. 
          If you choose dates outside of this range, it will generate an error.",
          br(),
          br(),
          
          numericInput(inputId = "independent_min",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440),
          "Records of a given species will only be counted as one detection if they occur within the set quiet period. 
          This setting addresses bias caused by a single animal sitting in front of a camera for a long period of time and repeatedly triggering the camera. 
          The default setting is 15 minutes."
        )

      ),

      fluidRow(
        
        box(title = "Map of Relative Activity Index (RAI) across camera grid",
            collapsible = TRUE,
            leafletOutput(outputId = "rai_map"),
            "Detections per trap-night at each camera. Note that greyed-out hexagons were not operable during the selected period.",
            "Switch to log scale for easier viewing (small value of 0.001 added to all RAI to address issue with 0s)",
            radioButtons(inputId = "log_select_map", label = "",
                         choices = list("RAI" = 1, "log(RAI)" = 2), 
                         selected = 1)
            
            ),
        
        box(title = "RAI over time",
            collapsible = TRUE,
            "Monthly RAI for the selected time period, calculated for the entire grid network (total detections per total trap-nights across all operating cameras). 
            An RAI of 0 indicates that there were no detections during that month.",
            plotlyOutput(outputId = "monthly_rai_hist")
        )

      ),
            
      fluidRow(
        
        box(title = "Diel activity pattern",
            collapsible = TRUE,
            "Kernel density distribution of the timing of the detections across all cameras across the 24-hour period. All times are scaled to solar time based on the date of the detection.",
            plotOutput(outputId = "activity_plot")
            ),
        
        box(title = "Environmental covariates of Relative Activity Index",
            collapsible = TRUE,
            selectInput(inputId = "metadata_select",
                        label = "Choose an environmental covariate:",
                        choices = c("Elevation", "Slope", "Vegetation", "Vegetation_Coarser", "BLM_Dist",
                                    "Boundary_Dist", "Fence_Dist", "HQ_Dist", "Road_Dist", "Water_Dist",
                                    "Ruggedness9", "Ruggedness25", "Ruggedness49", "Ruggedness81", "Ruggedness121",
                                    "Viewshed", "Viewshed_Reclass", "NDVI2016", "Vegetation_Edge_Dist", "Chaparral_Edge_Dist")),
            
            plotlyOutput(outputId = "rai_metadata"),
            "All covariates have been standardized to have a mean of 0 and standard deviation of 1 in the study area,
            so x-axis units are meaningless.
            Let me know if you are interested in how these data layers were generated."
        )

      )
       
    ),
  
  # Species comparison ------------------------------------------------------
  
    tabItem(
      
      tabName = "pairwise_compare",
      
      fluidRow(
        box(h2("COMPARISON TOOL"), width = 12,
            "This page enables the comparison of two data subsets. It can be used to compare patterns for a given species across time periods, or compare two species.")
      ),
      
      fluidRow(
        
        box(
          title = "Data Subset A:",
          
          selectInput(inputId = "species_select_A",
                      label = "Choose species for dataset A:",
                      selected = "Coyote",
                      choices = sort(unique(records$Species))),
          
          dateRangeInput(inputId = "date_range_A",
                         label = "Date Range:",
                         start = "2016-04-01",
                         end = "2017-12-22"),

          numericInput(inputId = "independent_min_A",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440)
        ),
        
        box(
          title = "Data Subset B:",
          
          selectInput(inputId = "species_select_B",
                      label = "Choose species for dataset B:",
                      selected = "Bear",
                      choices = sort(unique(records$Species))),
          
          dateRangeInput(inputId = "date_range_B",
                         label = "Date Range:",
                         start = "2016-04-01",
                         end = "2017-12-22"),
          
          numericInput(inputId = "independent_min_B",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440)
        )
      ),

      fluidRow(
        
        box(title = "Diel overlap",
            collapsible = TRUE,
            textOutput(outputId = "activity_overlap"),
            plotOutput(outputId = "activity_plot_compare")
            ),
        
        box(title = "Side-by-side trend over time",
            collapsible = TRUE,
            plotlyOutput(outputId = "rai_monthly_AB"))
        
      ),
      
      fluidRow(
        
        box(title = "Plot of RAI A vs B",
            collapsible = TRUE,
            plotlyOutput(outputId = "rai_AB"),
            "Option to switch to log scale for easier viewing (small value of 0.001 added to all RAI to address issue with 0s)",
            radioButtons(inputId = "log_select", label = "",
                         choices = list("RAI" = 1, "log(RAI)" = 2), 
                         selected = 1)
            )
        
      )
      
    )
  )
)


# Dashboard ---------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = "HREC Cameras"),
  sidebar,
  body
)

