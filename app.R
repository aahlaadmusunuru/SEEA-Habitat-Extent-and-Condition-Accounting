## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(sp)
library(sf)
library(leaflet)
library(DT)
library(leaflet.extras)
library(dplyr)
library(data.table)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(rAmCharts)
library(proj4)
library(shinyjs)
library(leaflet.minicharts)
library(manipulateWidget)
library(htmltools)
library(mapview)
library(devtools)
library(rapidjsonr)
library(magrittr)
library(spatialEco)
library(rgeos)
library(maptools)
library(effects)    
library(markdown)
library(leafpop)
library(shinythemes)
library(janitor)
library(openxlsx)
library(writexl)
library(terra)
library(highcharter)
library(shinycssloaders)
library(splitstackshape)

# List of color ramps
# Define the color ramps









# Color ramps
color_ramps <- list(
  "Blues" = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
  "Greens" = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
  "Oranges" = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
  "Reds" = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
  "Purples" = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"),
  "YlOrBr" = c("#FFFFD4", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#993404", "#662506", "#401604"),
  "YlGnBu" = c("#FFFFCC", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238B45", "#006D2C", "#00441B", "#003D1C"),
  "YlOrRd" = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026", "#800026"),
  "BuPu" = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B"),
  "GnBu" = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081"),
  "PuBu" = c("#F1EEF6", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#045A8D", "#023858", "#00234B"),
  "PuRd" = c("#F1EEF6", "#E7D4E8", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F"),
  "RdPu" = c("#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A"),
  "YlGn" = c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"),
  "Greys" = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000")
)

# Define UI for application that draws a histogram
ui <-  
  
  navbarPage("Ocean accoutnig",id = "inTabset",collapsible = TRUE,inverse = TRUE,theme = shinytheme("spacelab"),
             
             ###### Here : insert shinydashboard dependencies ######
             
             tabPanel("Home",
                      includeHTML("index.html")
                      
             ),
             
             
             header = tagList(
               useShinydashboard()
             ),
            # tabPanel(
             #  "Global Habitat Accounting",
              # value = "panel1",
              # tags$style(type ="text/css", "leaflet {height: calc(100vh - 80px) !important;}"),
              # leafletOutput("Habitat_Accounting"  ,height = '90vh') 
           #  ),
             
             tabPanel(
               "Step-1 Upload Admin Boundaries",    value = "panel1", 
               fluidPage(
                 br(),               
                 sidebarLayout(
                   sidebarPanel(
                     width = 3, # adjust the width here
                     # Upload Political Boundaries 
                     tags$head(
                       tags$style(
                         HTML("
        /* Custom CSS for side navigation bar */
        .sidebar {
          font-family: 'Arial', sans-serif;
          font-size: 16px;
          /* Add any other custom styles for the sidebar here */
        }
.gray-heading {
          color: #555555; /* Darker gray color */
          font-size: 24px;
          font-weight: bold;
          margin: 10px 0;
        }
      ")
                       )
                     ),                     
                     tags$div(class = "sidebar",
                              # Add your side navigation bar content here
                              # This could include menu items, links, icons, etc.
                              # Example:
                              tags$h2(class = "gray-heading", "Upload the total geographical extent of the habitat.  ", tags$img(src = "coral.png", height = "30px", width = "30px")),
                              
                     ),
                     
                     #  p("Upload the shapefile data, including the shapefile (shp), database file (dbf), spatial index files (sbn, sbx, shx), and projection file (prj), in the WGS-1984 coordinate system."),
                     # Upload Political Boundaries 
                     fileInput(
                       inputId = "filemap",
                       label = "Upload the shapefile data, including the shapefile (shp), database file (dbf), spatial index files (sbn, sbx, shx), and projection file (prj), in the WGS-1984 coordinate system.",
                       multiple = TRUE,
                       accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
                     ), 
                     fileInput("csvFile6", "Meta Data"),
                     
                     # Political Boundaries Attribute Header Selector
                     uiOutput("Politecal_boundaries"),
                     # Attribute  Selector
                     uiOutput("Politecal_boundaries_Selection"),
                     
                     
                     
                     
                     actionButton('jumpToP2', 'Next Step-2')
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     
                     title = "", status = "primary", 
                     
                     
                     
                     leafletOutput("adming", height = '80vh', width = '70vw') 
                     
                     
                   )
                 )
               )
             ),
             
             # step 2 data for uploading the hesitates Opening and Closing stock Data sets  
             
             
             tabPanel("Step-2 Upload Habitat Extent Data", value = "panel2", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3, # # adjust the width here
                          tags$div(class = "sidebar",
                                   # Add your side navigation bar content here
                                   # This could include menu items, links, icons, etc.
                                   # Example:
                                   tags$h2(class = "gray-heading", "Upload habitate opening extent and closing extent raster data-set." ,tags$img(src = "coral.png", height = "30px", width = "30px")),
                          ),
                          
                          p("The opening and closing raster datasets should be in the WGS 1984 geographic coordinate system."),
                          
                          # opening Extent Raster data sets  
                          
                          fileInput('layer', 'Opening Extent', multiple=FALSE, accept='asc',), 
                          # Closing Extent Raster data sets 
                          
                          fileInput('layer2', 'Closing Extent', multiple=FALSE, accept='asc') ,
                          tags$div(class = "sidebar",
                                   # Add your side navigation bar content here
                                   # This could include menu items, links, icons, etc.
                                   # Example:
                                   tags$h2(class = "gray-heading", "Upload the necessary parameters.",tags$img(src = "settings.png", height = "30px", width = "30px")),
                          ),
                          p("The parameters to style the raster datasets should be in CSV file format."),
                          
                          fileInput("csvFile1", "Habitats  Change Class Colors"),
                          
                          fileInput("csvFile8", "Time Period"),
                          
                          
                          
                          tags$br(),
                          uiOutput("Habitent_Change_Hotspots"),
                                  
                          actionButton('jumpToP3', 'Next Step-3')
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                        ),
                        mainPanel(
                          # Habituate Opening Extent 
                          width = 9, 
                          tabBox(
                            # Title can include an icon
                            title = tags$img(src = "coral.png", height = "30px", width = "30px"),
                            tabPanel("Opening Extent Maps",
                                     
                                     
                                     
                                     
                                     
                                     tags$div(
                                       style = "position:relative;",
                                       leafletOutput("Habituate_Opening_Extent" ,height = "470px")%>% withSpinner(color="#0dc5c1"),
                                       tags$div(
                                         style = "position:absolute; top:105px; right:10px;",
                                         downloadButton("Opening",  "", icon("download"),
                                                        
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                       ),
                                       tags$div(
                                         style = "position:absolute; top:105px; left:10px;",
                                         actionButton("RefreshPlotHabitate",  "", icon("refresh"),
                                                      
                                                      
                                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                       )
                                     ),
                                     
                                     
                                     
                                     
                                     tags$style(
                                       HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                     )
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Opening Extent Statistics",
                                     div(class = 'main_menu_theme',style="height:470px; padding-top:20px; padding-bottom:20px; overflow-y: scroll; overflow-y: scroll",
                                       
                                         
                                         
                                         tags$h4(
                                           class = "gray-heading",   style = 'font-size: 18px;',
                                           
                                           "Habitat Extent Accounting Opening Year:",
                                           
                                         ),
                                         tags$br(),
                                         
                                           downloadButton("Habitent_oPening_Table",  "", icon("download"),
                                                        
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")  ,
                                         tags$br(),
                                    
                                       
                                         
                                         p("In this specific section, we are presented with a table that displays the different habitat classes, their respective areas in hectares, and the percentage of each habitat within the total area. This table provides valuable
                                           information about the distribution and relative importance of various habitats in the area being analyzed"),
                                         tags$br(),
                                         
                                          dataTableOutput("Habitent_Table"),
                                         
                                      #   amChartsOutput("barchart_Area_Hectores"),
                                         
                                         amChartsOutput("barchart_Area_Hectores_Percentage")
                                     )
                                     
                            )
                            
                            
                          ),
                          
                          
                          tabBox(
                            # Title can include an icon
                            title =  tagList(
                              tags$img(src = "coral.png", height = "30px", width = "30px"),
                            ),
                            
                            tabPanel("Closing Extent Maps",
                                     
                                     tags$div(
                                       style = "position:relative;",
                                       leafletOutput("HabituateClosing_Extent" ,height = "470px")%>% withSpinner(color="#0dc5c1"),
                                       
                                       
                                       
                                       
                                       
                                       tags$div(
                                         style = "position:absolute; top:100px; right:10px;",
                                         
                                         
                                         downloadButton("Closing", "", icon("download"),
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                       ),
                                       tags$div(
                                         style = "position:absolute; top:105px; left:10px;",
                                         
                                         
                                         actionButton("RefresHabitateClosing", "", icon("refresh"),
                                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                       ),
                                     ),
                                     tags$style(
                                       HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                     )
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Closing Extent Statistics",
                                  
                                     div(class = 'main_menu_theme',style="height:470px; padding-top:20px; padding-bottom:20px; overflow-y: scroll; overflow-y: scroll",
                                         
                                         tags$h4(
                                           class = "gray-heading",   style = 'font-size: 18px;',
                                           
                                           "Habitat Extent Accounting Report:",
                                           
                                         ),
                                         tags$br(),
                                         
                                         
                                            downloadButton("Habitent_oPening_TableClosing",  "", icon("download"),
                                                        
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")  ,
                                         
                                         tags$br(),
                                         
                                         
                                         p("In this specific section, we are presented with a table that displays the different habitat classes, their respective areas in hectares, and the percentage of each habitat within the total area. This table provides valuable
                                           information about the distribution and relative importance of various habitats in the area being analyzed"),
                                         tags$br(),
                                         dataTableOutput("Habitent_Table_Closing")
                                     )
                                     
                            ),
                            tabPanel("Change Statistics",
                                     
                                     div(class = 'main_menu_theme',style="height:470px; padding-top:20px; padding-bottom:20px; overflow-y: scroll; overflow-y: scroll",
                                         tags$br(),
                                         
                                         
                                         tags$h4(
                                           class = "gray-heading",   style = 'font-size: 18px;',
                                           
                                           "Download the total habitat extent accounting statistics:",
                                           
                                         ),
                                         tags$br(),
                                         tags$p("Hear, you can download the Excel workbook of different tables
                                                (Ecosystem Extent Asset Account, SEEA change matrix, Habitat percentage change table, and Habitat Transformation table"),
                                         downloadButton("Habitate_change_table",  "", icon("download"),
                                                        
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"), 
                                         
                                         tags$h4(
                                           class = "gray-heading",   style = 'font-size: 18px;',
                                           
                                           "Download the total habitat extent accounting report:",
                                           
                                         ),
                                         
                                         
                                         tags$p(
                                           
                                           "Hear we can download the total habitat extent accounting her we different habitat change maps and different accounting table which includes 
                                           (SEEA change matrix, Habitat Percentage Change and Ecosystem Extent Asset Account Table) by click on the download buttons as Html file."
                                         ),
                                         downloadButton("HabitentChange",  "", icon("download"),
                                                        
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),

                                         tags$br(),
                                         
                                         
                                      #   tags$br(),
                                         
                                          div(class = 'main_theme',style=" padding-top:5px; padding-bottom:5px; ",
                                              tabPanel('Ecosystem Extent Asset Account Table',
                                                       
                                                       
                                                       
                                                       
                                                       tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),
                                                       tags$br(),
                                                   
                                                       tags$h4(
                                                         class = "gray-heading",   style = 'font-size: 18px;',
                                                         
                                                         "Ecosystem asset accounting tables:",
                                                         
                                                       ),
                                                       
                                                       tags$p("This table illustrate the changes in different ecosystem types with the addition 
                                                              and substation and net change of the habitats moreover it also giver the total 
                                                              aggregation sum of all the habits within two years."),
                                             dataTableOutput("Habitae_Change_Table")
                                             ),
                                             tags$h4(
                                               class = "gray-heading",   style = 'font-size: 18px;',
                                               
                                               "Habitat Transition Change Matrix:",
                                               
                                             ),
                                             tags$p(
                                               "This table provides an explanation of the changes that occur in habitats over two consecutive intervals. During these intervals, 
                                               the habitat areas undergo transformations, shifting from one class to another between the two time frames. "
                                             ),
                                             dataTableOutput("Habitae_Change_Matrix_Table"),
                                             tags$h4(
                                               class = "gray-heading",   style = 'font-size: 18px;',
                                               
                                               "Habitat Percentage Change Table:",
                                               
                                             ),
                                             tags$p("This table gives the detailed information how the habitats are been changing in the opening and closing time intervals. We can also see the
                                                    total area of habitats and the percentages of habitats and more over the net change of habits in two creational intervals .  "),
                                             dataTableOutput("Habitae_Percentage_Change_Table"),
                                             
                                             tags$h4(
                                               class = "gray-heading",   style = 'font-size: 18px;',
                                               
                                               "Habitat Transformation Table:",
                                               
                                             ),
                                             tags$p(
                                               "This table provides how the habitats is been 
                                               transforming from one form to other habitats hear we can understand how the habitat transformations is been happening.   "
                                             ),
                                             dataTableOutput("Habitate_Tranformation")
                                             
                                           
                                           #  downloadButton('Habitate_change_table',"Download the data")
                                             
                                         )
                                     )
                                     
                            )
                          ),
                          
                          
                          tabBox(
                            # Title can include an icon
                            title =  tagList(
                              tags$img(src = "coral.png", height = "30px", width = "30px"),
                            ),
                            
                            tabPanel("Habitate transformation hot spot maps",
                                     
                                     
                                     
                                     
                                     tags$div(
                                       style = "position:relative;",
                                       
                                       leafletOutput("Habitent_Change_Heatmap" ,height = "470px"),
                                       tags$div(
                                         style = "position:absolute; top:105px; left:10px;",
                                         actionButton("RefresHabitatechange", "", icon("refresh"),
                                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")   
                                         
                                       ),
                                       tags$div(
                                         style = "position:absolute; top:105px; right:10px;",
                                         
                                         
                                         downloadButton("Hot_Spot_Reports", "", icon("download"),
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                       ),
                                       
                                     ),
                                     tags$style(
                                       HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                     )
                                     
                                     
                            )),
                          tabBox(
                            # Title can include an icon
                            title =  tagList(
                              tags$img(src = "coral.png", height = "30px", width = "30px"),
                            ),
                            
                            tabPanel("Habitate transformation markers maps",
                                     
                                     
                                     tags$div(
                                       style = "position:relative;",
                                       leafletOutput("Habitent_Change_markers",height = "470px"),
                                       tags$div(
                                         style = "position:absolute; top:105px; right:10px;",
                                         
                                         downloadButton("Hot_Spot_Markers_Reports", "", icon("download"),
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                         tags$style(
                                           HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                         )
                                       )
                                       
                                     )
                                     
                            )
                            
                          )
                          
                          
                        )
                      )
                      
             ),
             tabPanel("Step-3 Upload  Condition Data", value = "panel3",
                      fluidPage(
                        br(),               
                        sidebarLayout(
                          shinyjs::hidden(
                          sidebarPanel(
                            width = 3,
                            tags$h2(class = "gray-heading", "Condition Accounting.", `data-toggle` = "your-value", tags$img(src = "thermometer.png", height = "30px", width = "30px")),
                            fileInput("layer3", "Condition Opening Extent (Raster File format)", multiple = TRUE),
                            fileInput("csvFile2", "Parameters of the Condition Data (.csv File format)"),
                            selectInput(
                              inputId = "color_ramp",
                              label = "Select a color ramp:",
                              choices = names(color_ramps),
                              selected = "PuRd"
                            ),
                            fileInput("layer4", "Condition Closing Extent (Raster File format)", multiple = TRUE),
                            fileInput("csvFile3", "Parameters of the condition data (.csv File format)"),
                            selectInput(
                              inputId = "color_ramp2",
                              label = "Select a color ramp:",
                              choices = names(color_ramps),
                              selected = "PuRd"
                            ),
                            fileInput("csvFile5", "Reference"),
                            
                            actionButton('jumpToP4', ' Next Step-4')

                          )),
                          mainPanel(
                            
                            width = 9, 
                            
                            tabBox(
                              # Title can include an icon
                              title = tags$img(src = "thermometer.png", height = "30px", width = "30px"),
                              tabPanel("Opening",
                                       
                                       
                                       leafletOutput("Habitent_condition", height = "760px")%>% withSpinner(color="#0dc5c1"),
                                       tags$div(
                                         style = "position:absolute; top:180px; right:30px;",
                                         downloadButton("OpeningStastics_Condition", "", icon("download"),
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                         
                                       ),
                                       tags$div(
                                         style = "position:absolute; top:120px; left:30px;",
                                         uiOutput("Opening_stock_Condition"),
                                         actionButton("RefreshPlotHabitateOpeningYear", "", icon("refresh"),
                                                      
                                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                         
                                         
                                         div(style = "max-height: 250px; max-width: 400px; overflow: auto;",
                                             
                                             
                                         ),
                                         
                                         
                                         tags$style(
                                           HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                         )
                                         
                                       )
                                       
                                       
                              ),
                              tabPanel(
                                "Opening Statistics",
                                div(class = 'main_menu_theme',style="height: 760px; padding-top: 20px; padding-bottom: 20px; overflow-y: scroll;",
                                    
                                  
                               
                                    div(class = 'main_habitates',style="padding-top:5px;" ,
                                        
                                        
                                        
                                        tags$h4(
                                          class = "gray-heading",   style = 'font-size: 18px;',
                                          
                                          "Overall condition accounting table :",
                                          
                                        ),
                                        
                                        tags$p(style = "justify-content:center;",
                                               
                                               'The overall condition accounting table provides information about the condition of the entire geographical area. The minimum value represents the lowest observed condition, the maximum value represents the highest observed condition, and the mean value represents the average condition across the area. The standard deviation indicates the degree of variation or spread in the condition values.'
                                               
                                        ),
                                        
                                        downloadButton(
                                          "Condition_Opening_Total_Step_3",
                                          "",
                                          icon("download"),
                                          style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                        ),
                                        tags$br(),
                                        
                                        tabBox(
                                          title = tags$img(
                                            src = "thermometer.png",
                                            height = "30px",
                                            width = "30px"
                                          ),
                                          width = "100%",
                                          tabPanel('Condition Stats',
                                                   
                                                   
                                                   
                                                   
                                                   tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                                   
                                                   dataTableOutput("Condition_Opening"),
                                                   
                                                   
                                                   
                                                   
                                          )
                                          
                                          
                                          
                                        ),
                                        div(
                                          style = "display: flex; align-items: center;",
                                          p(
                                            style = "padding: 10px;justify-content:center;

                                    ",
                                            "Table-1: Condition stasticies  (Min, Mean, Max and SD)."
                                          )
                                          
                                          
                                        )
                     
                                        
                                    )
                                    
                                )
                              )
                              
                              
                            ) ,
                            
                            
                            tabBox(
                              title = tags$img(src = "thermometer.png", height = "30px", width = "30px"),
                              
                              tabPanel("Closing",
                                       
                                       
                                       leafletOutput("Closing_stock_Habitent_condition", height = "760px")%>% withSpinner(color="#0dc5c1"),
                                       tags$div(
                                         style = "position:absolute; top:180px; right:30px;",
                                         downloadButton("Closing_Report", "", icon("download"),
                                                        
                                                        style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                         
                                       ),
                                       tags$div(
                                         style = "position:absolute; top:120px; left:30px;",
                                         uiOutput("Closing_stock_Condition"),
                                         actionButton("RefreshPlotHabitateClosingYear", "", icon("refresh"),
                                                      
                                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                         
                                         
                                         div(style = "max-height: 250px; max-width: 400px; overflow: auto;",
                                             
                                             
                                         ),
                                         
                                         
                                         tags$style(
                                           HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                         )
                                         
                                       )
                                       
                                       
                              ),
                              tabPanel(
                                "Closing Statistics",
                                div(class = 'main_menu_theme',style="height: 760px; padding-top: 20px; padding-bottom: 20px; overflow-y: scroll;",
                                 
                               
                       
                                    
                                    
                                    div(class = 'main_habitates',style="padding-top:5px;" ,
                                        
                                        
                                        
                                        tags$h4(
                                          class = "gray-heading",   style = 'font-size: 18px;',
                                          
                                          "Overall condition accounting table :",
                                          
                                        ),
                                        
                                        tags$p(style = "justify-content:center;",
                                               
                                               'The overall condition accounting table provides information about the condition of the entire geographical area. The minimum value represents the lowest observed condition, the maximum value represents the highest observed condition, and the mean value represents the average condition across the area. The standard deviation indicates the degree of variation or spread in the condition values.'
                                               
                                        ),
                                        
                                        downloadButton(
                                          "Condition_Closing_Total_Step_3",
                                          "",
                                          icon("download"),
                                          style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                        ),
                                        tags$br(),
                                        
                                        tabBox(
                                          title = tags$img(
                                            src = "thermometer.png",
                                            height = "30px",
                                            width = "30px"
                                          ),
                                          width = "100%",
                                          tabPanel('Condition Stats',
                                                   
                                                   
                                                   
                                                   
                                                   tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                                   
                                                   
                                                   dataTableOutput("HabitateChangeClosing"),
                                                   
                                                   
                                                   
                                                   
                                          )
                                          
                                          
                                          
                                        ),
                                        div(
                                          style = "display: flex; align-items: center;",
                                          p(
                                            style = "padding: 10px;justify-content:center;

                                    ",
                                            "Table-1: Condition stasticies  (Min, Mean, Max and SD)."
                                          )
                                          
                                          
                                        )
                                        
                                        
                                    )
                                    
                                )
                              ),
                              tabPanel(
                                "Condition Change Statistics",
                                
                                div(class = 'main_menu_theme',style="height: 760px; padding-top: 20px; padding-bottom: 20px; overflow-y: scroll;",
                                    tags$h4(
                                      class = "gray-heading",   style = 'font-size: 18px;',
                                      
                                      "Habitat condition change accounting report :",
                                      
                                    ),
                                    downloadButton(
                                      "Condition_Change_Stasticeis",
                                      "",
                                      icon("download"),
                                      style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                    ),
                                    tags$br(),
                                    
                                    tags$p(style = "justify-content:center;",
                                           
                                           'In this section, we will analyze the changes in the overall condition of the total area. 
                                           We will explore the minimum, mean, maximum, and standard deviation, as well as the net change in condition. Additionally,
                                           we will compare these measurements with the reference and targeted elements to monitor the variation in condition change within the region.'
                                           
                                    ),
                                tabBox(
                            
                                  width = "100%",
                          
                                      
                                  tabPanel('Total Condition Change Statistics',
                                           tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),
                                          downloadButton(
                                            "Condition_Change_stats",
                                            "",
                                            icon("download"),
                                            style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                          ),
                                          
                                          
                                          
                                           dataTableOutput("Condition_Change")%>% withSpinner(color="#0dc5c1"),
                                          
                                          
                                          
                                           )
                                  
                                  
                                
                                  
                                  )                              
                                )
                                
                              )
                            )
                            
                          ))))
             ,
             tabPanel("Step-4 Total Habitat Extent and Condition Accounting", value = "panel4", 
                      fluidPage(
                        br(),               
                        sidebarLayout(
                          shinyjs::hidden(
                            sidebarPanel(
                              width = 3,
                              tags$h4(class = "gray-heading", "In this section we can download the total 
                                      habitate extent and condition stasticies" ,tags$img(src = "coral.png", height = "30px", width = "30px")),
                              uiOutput("Merain_Habitent_projection4_stat"),
                              uiOutput("Merain_Habitentes_Markers"),

                              fileInput("csvFile7", "Habitent_Condition Reference"),
                         tags$p("Please upload all the necessary parameters from step one to step four. Once completed, you can generate the overall accounting tables for the habitat extent and
                                habitat condition data sets by clicking on the download button below."),
                              #  style = "position:absolute; top:105px; left:10px;",
                              downloadButton("Total_Habitate_Extent_condition_change_table",  "", icon("download"),
                                             
                                             
                                             style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")
                              
                            )),
                          mainPanel(
                            
                            fluidRow(
                              
                                  
                        box(
                          
                              id = 'foo',  # use an id to only select this box
                              
                              infoBoxOutput("vbox1", width = 6)%>% withSpinner(color="#0dc5c1"),
                              infoBoxOutput("vbox2", width = 6)%>% withSpinner(color="#0dc5c1"),
                          
                                      
                              infoBoxOutput("vbox3", width = 6)%>% withSpinner(color="#0dc5c1"),
                              infoBoxOutput("vbox4",width = 6)%>% withSpinner(color="#0dc5c1")
                              

                              
                            ),
                        
                        
                        
                               box(
                                 id = 'foo',  # use an id to only select this box
                                 
                          column( 6,
                                  amChartsOutput("gauge",                    height = "21rem")%>% withSpinner(color="#0dc5c1")),
                          
                          column( 6,
                                  amChartsOutput("gauges",                    height = "21rem")%>% withSpinner(color="#0dc5c1")
                                  
                                  
                                  
                          )
                          
                          
                          
                          
                          
                        ),
                        tags$head(tags$style('#foo .box-header{ display: none}')) # target the box header of foo
                     
                        
                            
                            
                          ),
                        fluidRow(
                          column(
                            12,  # This sets the column width to col-sm-12
                            tabBox(
                              width = 12, 

                            tabPanel(
                              "Habitat Transformation",
                              tags$div(
                                style = "position:relative;",
                                leafletOutput("Condetion_Habitent_Tranformaion_Markers",height = "500px"),
                                tags$div(
                                  style = "position:absolute; top:105px; right:10px;",
                                  
                                  downloadButton("Markers_Reports_Condition_Integrated", "", icon("download"),
                                                 style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                  tags$style(
                                    HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                  )
                                )
                                
                              )
                             
                            ),
                            tabPanel(
                              "Habitat Opening",
                       
                          
                              tags$h4(class = "gray-heading",   style = 'font-size: 18px;',
                                      
                                      'Habitat condition accounting table :'),
                              
                              downloadButton("Condition_Habitate_Opening_Total_Step_3", "", icon("download"),
                                             
                                             style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                              tags$br(),
                              tags$p(style = "justify-content:center;",
                                     "The habitat condition accounting table provides specific information about the condition of different habitat classes within the opening time.
                                     This table includes the minimum, mean, maximum, and standard deviation statistics for each habitat class"),
                              tags$br(),
                              
                         
                              
                              tabBox(
                                title = tags$img(
                                  src = "thermometer.png",
                                  height = "30px",
                                  width = "30px"
                                ),
                                width = "100%",
                                
                                
                                tabPanel('Min',
                                         downloadButton(
                                           "Condition_Opening_Habitate_Min_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         
                                         
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Opening_Stockes_Min_Data")
                                ),
                                tabPanel('Mean',
                                         downloadButton(
                                           "Condition_Opening__Habitate_Mean_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Opening_Stockes_Mean_Data")
                                ),
                                tabPanel('Max',
                                         downloadButton(
                                           "Condition_Opening_Habitate_Max_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Opening_Stockes_Max_Data")
                                ),
                                tabPanel('SD',
                                         downloadButton(
                                           "Condition_Opening_Habitate_SD_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Opening_Stockes_SD_Data")
                                         
                                         
                                         
                                )
                              )
                                     
                                     
                                     
                                     
                                     
                                     ),
                            tabPanel(
                              "Habitat Closing",
                              
                              
                              tags$h4(class = "gray-heading",   style = 'font-size: 18px;',
                                      
                                      'Habitat condition accounting table :'),
                              
                              downloadButton(
                                "Condition_Habitate_Closing_AccountingReport_Step_3",
                                "",
                                icon("download"),
                                style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                              ),
                         
                              
                      
                              
                              tags$br(),
                              tags$p(style = "justify-content:center;",
                                     "The habitat condition accounting table provides specific information about the condition of different habitat classes within the closing time.
                                     This table includes the minimum, mean, maximum, and standard deviation statistics for each habitat class"),
                              
                              
                              
                              tabBox(
                                title = tags$img(
                                  src = "thermometer.png",
                                  height = "30px",
                                  width = "30px"
                                ),
                                width = "100%",
                                
                                
                                tabPanel('Min',
                                         downloadButton(
                                           "Condition_Closing_Habitate_Min_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Closing_Stockes_Min_Data")
                                ),
                                tabPanel('Mean',
                                         downloadButton(
                                           "Condition_Closing_Habitate_Mean_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Closing_Stockes_Mean_Data")
                                ),
                                tabPanel('Max',
                                         downloadButton(
                                           "Condition_Closing_Habitate_Max_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         
                                         dataTableOutput("Habitent_Condition_Closing_Stockes_Max_Data")
                                ),
                                tabPanel('SD',
                                         downloadButton(
                                           "Condition_Closing_Habitate_SD_Total_Step_3",
                                           "",
                                           icon("download"),
                                           style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                         ),
                                         tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),  # Set overflow and dimensions for min tab content
                                         
                                         dataTableOutput("Habitent_Condition_Closing_Stockes_SD_Data")
                                )
                              )
                            
                            ),  
                            tabPanel('Habitate Condition Change Statistics',
                                     tags$style("#min-tab-content {overflow-x: auto; overflow-y: auto; width: 100%; height: 100%;}"),
                                     
                                     
                                     
                                     tags$h4(class = "gray-heading",   style = 'font-size: 18px;',
                                             
                                             'Habitat condition accounting table :'),
                                     
                                     downloadButton("Condition_Habitate_Finel_Result", "", icon("download"),
                                                    
                                                    style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                     tags$br(),
                                     tags$p(style = "justify-content:center;",
                                            "The habitat condition accounting table provides specific information about the condition of different habitat classes within the opening time and closing time and the netchagne .
                                     This table includes the minimum, mean, maximum, and standard deviation statistics for each habitat class"),
                                     tags$br(),
                                     
                                     
                                     
                                     downloadButton(
                                       "Condition_Change_habit_stats",
                                       "",
                                       icon("download"),
                                       style = "background-color:black;color:white;
    height:40px;width:40px;border:none;
    padding:0;display:flex;justify-content:center;
    align-items:center;"
                                     ),
                                     
dataTableOutput("Condition_habitentes") %>% withSpinner(color="#0dc5c1"),style = "height:60rem; overflow-y: scroll;overflow-x: scroll;"
                                     
                            )))))))),
  
tabPanel("User-Guide", value = "panel5", 
         fluidPage(
           sidebarPanel(
             tags$h4(class = "gray-heading", " In this section, we can find the user manual which provides the step-by-step process for data preprocessing using QGIS." ) ,      
             
             tags$h4(
               
               class = "gray-heading",
               "It also helps to Producing Accounts using the R-Shiny application." ,tags$img(src = "coral.png", height = "30px", width = "30px"))
           ,
             
             tags$h3(style="font-with:bold;","Help & feedback"),
             p("For help, feedback, and bug reports, please contact:"),
             
             tags$p(style = "font-weight: bold;", "Aahlaad Musunuru"),
             tags$p("Email: aahlaadmusunuru1995@gmail.com")
             
             
                    
             
           ),
           mainPanel(
             
           
           tabBox(
             title = tags$img(
               src = "settings.png",
               height = "30px",
               width = "30px"
             ),
             width = "100%",
             tabPanel('Manual',
                   
                      tags$iframe(src = "README.html", style = "width:100%; height:90vh;")
                      
                      
             )
  
           )
         )
         )
         
         )
  
  
  
  
  )
server <- function(input, output,session) {
  
  # upload the shape file or image file data in R shiny  wiht maximum my of data 
  
  options(shiny.maxRequestSize=300000*1024^2) 
  
  
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  
  
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel5")
  })
  
  
  
  # Upload Admin Boundaries 
  map <- reactive({
    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/" ))
    map
  })
  
  # Extract Admin Boundaries Data by Attribute Header
  Admin_Boundaries <- reactive({
    map() %>%
      subset(select = input$variable_mapAdminshp)
  }) %>% bindCache(map(), input$variable_mapAdminshp)
  
  # Extract Admin Boundaries Data by Attribute Name
  Admin_boundaries_Data <- reactive({
    as.data.frame(Admin_Boundaries()) -> Admin_extent
    split(Admin_extent, Admin_extent)
  }) %>% bindCache(Admin_Boundaries())
  
  # Create a reactive map with the base layer
  basemap <- reactive({
    leaflet() %>%
      addProviderTiles(
        "Esri.WorldImagery",
        options = providerTileOptions(
          minZoom = 0,
          maxZoom = 19
        )
      )
  })
  
  # Add the filtered polygons on top of the base map
  filtered_polygons <- reactive({
    Admin_Boundaries()[Admin_Boundaries()[[1]] %in% input$variable_PoliticalBoundaries_Data, ] -> adminBoundaries
    
    p <- colorFactor(palette = c("#d5b43c"), domain = c("#d5b43c"))
    
    basemap() %>%
      addPolygons(data = adminBoundaries, fillColor = "#d5b43c", stroke = TRUE,color = "black" ,weight = 1,fillOpacity = 1) %>% addScaleBar()%>% 
      addLegend("bottomright", colors="#d5b43c", labels='Habitat', title="Legend")
  })
  
  output$adming <- renderLeaflet({
    filtered_polygons()
  })
  
  
  
  
  # Step 2 habitate  and stasticies 
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".shp", sep = "")
    },
    content = function(file) {
      reado(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # step 2 Upload Habitent Extetn Data sets
  
  
  habitentData1<-reactive({
    
    
    
    inFile1 <- input$layer
    
    if (is.null(inFile1))
      return(NULL)
    
    Hb_1 <- raster(inFile1$datapath)
    
    
    
  })
  
  habitentData2<-reactive({
    
    inFile2<-input$layer2
    
    
    if(is.null(inFile2))
      
      return(NULL)
    
    Hb2<-raster(inFile2$datapath)
    
    
    
    
  })
  
  dt<-reactive({
    
    
    file1 <- input$csvFile8
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    years<-data.frame(data)
    
    
  }
  
  )
  
  
  
  habitent_crop1<-reactive({
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    masked <- mask(x = habitentData1(), mask = adminBoundaries)    
    
    
    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    
    return(cropped)
    
    
    
    
  })
  habitates_plote<-eventReactive(input$RefreshPlotHabitate,{
    
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    
    
    
    
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    
    leaflet()%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addRasterImage(habitent_crop1(),group="Opening Extent"  
                                                                                                                         
                                                                                                                         ,colors=c(unique(data$Colors)))%>%addPolygons(data =adminBoundaries,fill = F, weight = 2, color = "#FFFFCC", group = "Admin" ) %>%addLayersControl(
                                                                                                                           baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Admin","Opening Extent"),
                                                                                                                           options = layersControlOptions(collapsed = TRUE))%>% addScaleBar()%>% addLegend("bottomright", colors=unique(data$Colors), labels=unique(data$Habitat), title="Legend")
    
    
  })
  output$Habituate_Opening_Extent<-renderLeaflet({
    
    habitates_plote()  
    
  })
  
  
  habitates_data <- eventReactive(input$RefreshPlotHabitate, {
    habitent_crop1() %>% na.omit() %>% as.data.frame(xy = TRUE) -> habitentes
    
    # Extract the latitude values
    min_lat <- habitent_crop1()@extent@ymin
    max_lat <- habitent_crop1()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop1())[2]
    resolution_meters <- resolution_degrees * conversion_factor
    print(resolution_meters)
    # Select the required columns from habitentes data
    habitentes%>%dplyr::select(-x,-y)->habitantes
    
    file1 <- input$csvFile1
    if (is.null(file1)) {
      return()
    }
    data <- read.csv(file = file1$datapath)
    
    # Perform left join on habitat and data
    R1 <- left_join(habitantes, data, by = c("X0" = "ID"))
    
    # Extract specific column
    habitat <- R1[2]
    
    # Calculate area and percentage
    result <- data.frame(table(habitat))
    result$Freq <- result$Freq * resolution_meters * resolution_meters / 10000
    result$Percentage <- result$Freq / sum(result$Freq) * 100
    names(result) <- c("Habitat Classes", "Area ha", "Percentage")
    
    # Round area and percentage values
    Area_ha <- round(result[2], 0)
    Percentage <- round(result[3], 0)
    
    data.frame(result[1], Area_ha, Percentage) -> Habitat_Area
  })
  
  
  
  output$Habitent_Table<-renderDataTable({
    
    
    
    data.frame(habitates_data())
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  output$Habitent_oPening_Table <- downloadHandler(
    filename = function(){"Habitate Opening.csv"}, 
    content = function(fname){
      
      
      write.csv(habitates_data(), fname)
    }
  )
  
  output$Habitent_oPening_TableClosing <- downloadHandler(
    filename = function(){"Habitate Closing.csv"}, 
    content = function(fname){
      
      
      write.csv(habitates_data_Closing(), fname)
    }
  )
  
  
  habitates_Percentage<-eventReactive(input$RefreshPlotHabitate,{
    
    habitates_data()%>%dplyr::select(Habitat.Classes,Percentage)->result
    names(result)<-c("label","value")
    amBarplot(main = "Habitat Percentage",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
    
  })
  
  
  
  habitates_Aea_hectarea<-eventReactive(input$RefreshPlotHabitate,{
    
    habitates_data()%>%dplyr::select(Habitat.Classes,Area.ha)->result
    
    
    names(result)<-c("label","value")
    
    
    
    amBarplot(main = "Habitat Area Hectores",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
  })
  
  
  
  
  # generate OpeningExtentreport
  admin<-reactive({
    
    
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    
    p = colorFactor(palette = c("#B3541E"),domain = c("#B3541E"))
    leaflet()%>%addTiles()%>%addPolygons(data = adminBoundaries,fillColor = "#B3541E",stroke = FALSE,fillOpacity = 1)%>% addLegend(position = "bottomright",pal = p, values = c("Geographical_Extent"),title = "Legend")%>% addScaleBar()
    
  })
  
  
  
  metardata<-reactive({
    file2 <- input$csvFile6
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    data.frame(data)
    
  })
  
  
  conditon_min<-eventReactive(input$refreshPlot_Habitates,{
    
    
    
    
    condtion_habitents_opening_stackes()->opening
    
    opening$Tume_period<-Years_Uploded()$Years[1]
    
    
    Combine_Habitates_filter<- as.data.frame(opening)%>%dplyr::select(-1,-3)
    
    
    agg = aggregate(Combine_Habitates_filter,
                    by = list( Combine_Habitates_filter$Habitat,Combine_Habitates_filter$Tume_period),
                    FUN = min)
    Filter_aggrigation<-agg%>%dplyr::select(-Habitat,-Tume_period)
    
    
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data_n1 = read.csv(file=file2$datapath)
    
    
    
    names(Filter_aggrigation)<-c("Habitat","Tume_period",    data_n1$Condition.Indicator)
    
    
    data.frame(t(Filter_aggrigation))->result
    
    data.frame(result[1,])->h1
    
    result[-1:-2,]->result
    
    
    names(result)<-c(h1)
    
    
    result
    
    
    data.frame(rownames(result))->Condition.Indicator
    
    names(Condition.Indicator)<-"Condition.Indicator"
    data.frame(Condition.Indicator,result)->result
    
    rownames(result)<-NULL
    
    result
    
    
    left_join(result,data_n1,by=c("Condition.Indicator"))->R
    
    
    R%>%dplyr::select(Condition.Indicator,Measurement.Unit)->r1
    # R%>%dplyr::select(-Condition.Indicator,-Measurement.Unit,-Time.Intervel)->r1
    
    data.frame(R)%>%dplyr::select(-Condition.Indicator,-Measurement.Unit,-Time.Intervel)->r3
    
    
    
    
    cbind((R%>%dplyr::select(Condition.Indicator,Measurement.Unit,Time.Intervel)),data.frame(R)%>%dplyr::select(-Condition.Indicator,-Measurement.Unit,-Time.Intervel))->result
    
    
    data.frame(result)->rty
    
    
  })
  
  
  output$ConditionOPeningHabitates<-renderDataTable({
    
    
    data.frame(conditon_min())
    
    
    
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  # generate OpeningExtentreport
  admin<-reactive({
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    p = colorFactor(palette = c("#B3541E"),domain = c("#B3541E"))
    leaflet()%>%addTiles()%>%addPolygons(data = adminBoundaries,fillColor = "#B3541E",stroke = FALSE,fillOpacity = 1)%>% addLegend(position = "topright",pal = p, values = c("Geographical_Extent"),title = "Legend")
    
  })
  output$Opening <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Opening.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Opening.Rmd")
      file.copy("Opening.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(m= habitates_plote(),o=habitates_data(),p=habitates_Percentage(),q=habitates_Aea_hectarea(),s=metardata()) 
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  # habitate Closing 
  
  habitent_crop2<-reactive({
    
    
    
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    
    
    masked2 <- mask(x = habitentData2(), mask = adminBoundaries)    
    
    cropped2 <- crop(x = masked2 , y = extent(adminBoundaries))
    
    
    return(cropped2)
    
    
    
    
    
  })
  
  habitates_Closing<-eventReactive(input$RefresHabitateClosing,{
    
    
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    
    
    
    
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    
    
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    
    leaflet()%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%
      addRasterImage(habitent_crop2(),group="Closing Extent"  
                     
                     
                     ,colors=c(unique(data$Colors)))%>%addPolygons(data =adminBoundaries,fill = F, weight = 2, color = "#FFFFCC", group = "Admin" ) %>%addLayersControl(
                       baseGroups = c( "Esri", "OSM (default)"),overlayGroups = c("Admin","Closing Extent"),
                       options = layersControlOptions(collapsed = TRUE))%>% addScaleBar()%>% addLegend("bottomright", colors=unique(data$Colors), labels=unique(data$Habitat), title="Legend")%>% addScaleBar()
    
    
    
    
  })
  
  output$HabituateClosing_Extent<-renderLeaflet({
    
    
    
    habitates_Closing()
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  habitates_data_Closing<-eventReactive(input$RefresHabitateClosing,{
 
    habitent_crop2() %>% na.omit() %>% as.data.frame(xy = TRUE) -> habitentes
    
    # Extract the latitude values
    min_lat <- habitent_crop2()@extent@ymin
    max_lat <- habitent_crop2()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop2())[2]
    resolution_meters <- resolution_degrees * conversion_factor
    print(resolution_meters)
    # Select the required columns from habitentes data
    habitentes%>%dplyr::select(-x,-y)->habitantes
    
    file1 <- input$csvFile1
    if (is.null(file1)) {
      return()
    }
    data <- read.csv(file = file1$datapath)
    
    # Perform left join on habitat and data
    R1 <- left_join(habitantes, data, by = c("X0" = "ID"))
    
    # Extract specific column
    habitat <- R1[2]
    
    # Calculate area and percentage
    result <- data.frame(table(habitat))
    result$Freq <- result$Freq * resolution_meters * resolution_meters / 10000
    result$Percentage <- result$Freq / sum(result$Freq) * 100
    names(result) <- c("Habitat Classes", "Area ha", "Percentage")
    
    # Round area and percentage values
    Area_ha <- round(result[2], 0)
    Percentage <- round(result[3], 0)
    
    data.frame(result[1], Area_ha, Percentage) -> Habitat_Area
  })
  
  
  
  
  
  output$Habitent_Table_Closing<-renderDataTable({
    data.frame(habitates_data_Closing())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  
  habitates_Aea_hectarea_Closing<-eventReactive(input$RefresHabitateClosing,{
    habitates_data_Closing()%>%dplyr::select(Habitat.Classes,Area.ha)->result
    names(result)<-c("label","value")
    amBarplot(main = "Habitat Area Hectores",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
  })
  
  
  
  
  habitates_Aea_Percentage_Closing<-eventReactive(input$RefresHabitateClosing,{
    habitates_data_Closing()%>%dplyr::select(Habitat.Classes,Percentage)->result
    names(result)<-c("label","value")
    amBarplot(main = "Habitat Area Percentage",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
  })
  
  
  
  
  
  
  
  
  habitate_Change<-eventReactive(input$RefresHabitatechange,{
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    D1
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening_Extent","Closing_Extent")
    
    
    data.frame(table(Result[1]))->v1
    
    names(v1)<-c("classes","pixcelcount")
    data.frame(table(Result[2]))->v2
    
    names(v2)<-c("classes2","pixcelcount2")
    left_join(data, v1, by = c("Habitat"="classes"))->R1
    left_join(data, v2, by = c("Habitat"="classes2"))->R2
    
    data.frame(R1,R2)%>%dplyr::select(Habitat,pixcelcount,pixcelcount2)->Percentage_chagne_table
    names(Percentage_chagne_table)<-c("Classes","Opening_Extent","Closing_Extent")
    
    # Extract the latitude values
    min_lat <- habitent_crop2()@extent@ymin
    max_lat <- habitent_crop2()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
     resalution_1<- res(habitent_crop2())[2]
     resalution<-resalution_1*conversion_factor
    
    Percentage_chagne_table$Opening_Extent<-round((Percentage_chagne_table$Opening_Extent)  *resalution*resalution*1/10000,0)
    Percentage_chagne_table$Closing_Extent<-round((Percentage_chagne_table$Closing_Extent)  *resalution*resalution*1/10000,0)
    names(Percentage_chagne_table)<-c("Classes",Years_Uploded()$Years[1],Years_Uploded()$Years[2])
    Percentage_chagne_table_total<- Percentage_chagne_table %>%   adorn_totals("row")
    Percentage_chagne_table_total$Netchange<-(Percentage_chagne_table_total[[3]]-(Percentage_chagne_table_total[[2]]))
    Percentage_chagne_table_total$Additions<-fifelse(Percentage_chagne_table_total$Netchange>0,Percentage_chagne_table_total$Netchange,0)
    Percentage_chagne_table_total$Additions[Percentage_chagne_table_total$Additions == 0] <- NA
    Percentage_chagne_table_total$Subtractions<-fifelse(Percentage_chagne_table_total$Netchange<0,Percentage_chagne_table_total$Netchange,0)
    Percentage_chagne_table_total$Subtractions[Percentage_chagne_table_total$Subtractions == 0] <- NA
    Habitent_Change<- Percentage_chagne_table_total%>%dplyr::select(Years_Uploded()$Years[1],Additions,Subtractions,Years_Uploded()$Years[2],Netchange)
    t(Habitent_Change)->Result
    data.frame(Result)->r4
    fd<-rbind(data,"Total")
    names(r4)<-fd$Habitat
    r4%>%dplyr::select(Total)->cq
    r4%>%as.data.frame.matrix()->cv
    cv[3,]->subb
    t(subb)->subbT
    rownames(subbT)<-NULL
    data.frame(subbT)%>%na.omit()->subbTD
    sum(subbTD)->substraction
    names(substraction)<-NULL
    substraction
    cv[2,]->add
    t(add)->addT
    rownames(addT)<-NULL
    data.frame(addT)%>%na.omit()->addTD
    sum(addTD)->Addition
    names(Addition)<-NULL
    Addition
    data.frame(Addition,substraction)
    cv%>%dplyr::select(Total)->cqs
    rownames(cqs)<-NULL
    data.frame( cqs$Total[1],Addition,substraction,cqs$Total[4],cqs$Total[5])->habitaTotal
    t(habitaTotal)->habitaTotalT
    row.names(habitaTotalT)<-NULL
    data.frame(habitaTotalT)->Total
    names(Total)<-"Total"
    df <- subset(cv, select = -Total)
    
    data.frame(df,Total)
    
    
  })
  
  
  
  
  
  output$Closing <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Closing.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Closing.Rmd")
      file.copy("Closing.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(m= habitates_Closing(),o=habitates_data_Closing(),q=habitates_Aea_hectarea_Closing(),p=habitates_Aea_Percentage_Closing(),s=metardata()) 
      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  
  
  
  
  # Hot spot maps 
  projectionHot_spotmaps<-eventReactive(input$RefresHabitatechange,{
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent,xy=TRUE)%>%na.omit()->D1
    
    D1$NewTemp<-ifelse(D1$X0.1==D1$X0.2,0, 1)
    
    D1[D1$NewTemp==1,]->Res
    
    Res
    
    names(Res)<-c("lon","lat","Opening_Extent","Closing_Extent")
    Res%>%dplyr::select("lon","lat","Opening_Extent","Closing_Extent")->Reselt_data
    
    
    
    left_join(Reselt_data, data, by = c("Opening_Extent"="ID"))->R1
    
    left_join(R1, data, by = c("Closing_Extent"="ID"))->R2
    
    R2%>%dplyr::select("lon","lat","Habitat.x","Habitat.y")->R3
    
    R3$Habitents_Change<-paste0(R3$Habitat.x," to ",R3$Habitat.y)
    
    
    R3%>%dplyr::select(lon,lat,Habitents_Change)
    
    
    
  })
  
  projection4_Hotspotmaps<-eventReactive(input$RefresHabitatechange,{
    
    split(projectionHot_spotmaps(),projectionHot_spotmaps()$Habitents_Change)
    
  })
  
  
  Habitate_Change_hotspot<-reactive({
    
    
    leaflet(data = projection4_Hotspotmaps()[[input$Heat_habitent]])%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%
      addHeatmap(lng = ~lon , lat = ~lat, intensity = ~lat, blur = 20, max = 0.05, radius = 15 )%>%addLegend("bottomright", colors =c("#fa1e0e",  "#fff600", "#54e346", "#2978b5"),labels= c("High", "","", "Low"),title= "Legend") %>%addLayersControl(
        baseGroups = c( "Esri","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
  })
  output$Habitent_Change_Heatmap<-renderLeaflet({
    
    Habitate_Change_hotspot()
    
  })
  
  
  Habitate_Hotspot<-reactive({
    
    
    data.frame(names(projection4_Hotspotmaps()))->df
    names(df) <- NULL
    
    split(df,df)->r
    
    res<- paste0('Habitate Transformation From       ',r[[input$Heat_habitent]])
    
    
    leaflet(data = projection4_Hotspotmaps()[[input$Heat_habitent]])%>%
      # add other map elements here
      addControl(htmltools::div(
        style = "text-align: center; font-size: 18px; font-weight: bold;", 
        res), 
        position = "topright")%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%
      addHeatmap(lng = ~lon , lat = ~lat, intensity = ~lat, blur = 20, max = 0.05, radius = 15 )%>%addLegend("bottomright", colors =c("#fa1e0e",  "#fff600", "#54e346", "#2978b5"),labels= c("High", "","", "Low"),title= "Legend") %>%addLayersControl(
        baseGroups = c( "Esri","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
  })
  # habite markers 
  
  
  
  rlaa<-reactive({
    data.frame(names(projection4_Hotspotmaps()))->df
    names(df) <- NULL
    
    split(df,df)->r
    
    res<-  r[[input$Heat_habitent]]
    
    
    return(res)
  })
  
  
  
  awesome <- makeAwesomeIcon(
    icon = "info",
    iconColor = "black",
    markerColor = "blue",
    library = "fa"
  )
  
  Habitate_Change_markers<-reactive({
    
    
    leaflet(data =projection4_Hotspotmaps()[[input$Heat_habitent]])%>%
      
      addTiles(group = "OSM (default)")%>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addAwesomeMarkers(lng = ~lon   , lat = ~lat, icon = awesome,popup = popupTable(projection4_Hotspotmaps()[[input$Heat_habitent]]), clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)                              
      ) %>%addLayersControl(
        baseGroups = c( "Esri","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
    
    
    
  })
  Habitate_Change_markers_data<-reactive({
    
    
    data.frame(names(projection4_Hotspotmaps()))->df
    names(df) <- NULL
    
    split(df,df)->r
    
    res<- paste0('Habitate Transformation From       ',r[[input$Heat_habitent]])
    
    leaflet(data =projection4_Hotspotmaps()[[input$Heat_habitent]])%>%    # add other map elements here
      addControl(htmltools::div(
        style = "text-align: center; font-size: 18px; font-weight: bold;", 
        res), 
        position = "topright")%>%
      addTiles(group = "OSM (default)")%>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addAwesomeMarkers(lng = ~lon   , lat = ~lat, icon = awesome,popup = popupTable(projection4_Hotspotmaps()[[input$Heat_habitent]]), clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)                              
      ) %>%addLayersControl(
        baseGroups = c( "Esri","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
    
    
    
  })
  
  
  output$Habitent_Change_markers<-renderLeaflet({
    
    Habitate_Change_markers()
    
  })
  
  
  
  habchange<-eventReactive(input$RefresHabitatechange,{
    
    
    stack(habitent_crop1(),habitent_crop2())->hbs
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    leaflet()%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%
      addRasterImage(habitent_crop1(),colors=c(unique(data$Colors)),group ="Habitate Opening")%>%addRasterImage(habitent_crop2(),colors=c(unique(data$Colors)),group ="Habitate Closing")%>%addPolygons(data =adminBoundaries,fill = F, weight = 2, color = "#FFFFCC", group = "Admin" ) %>%addLayersControl(
        baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Admin","Habitate Opening","Habitate Closing"),
        options = layersControlOptions(collapsed = FALSE))%>%addMeasure( )%>%  addScaleBar()%>% addLegend("bottomright", colors=unique(data$Colors), labels=unique(data$Habitat), title="Legend")
    
  })
  
  output$Habitae_Change_Table<-renderDataTable({
    
    data.frame(habitate_Change())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  Habitae_Change_Transetion_Matrix<-eventReactive(input$RefresHabitatechange,{
    
    
    # Extract the latitude values
    min_lat <- habitent_crop2()@extent@ymin
    max_lat <- habitent_crop2()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop2())[2]
    resalution <- resolution_degrees * conversion_factor
    
  
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    
    
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening Extent","Closing Extent")
    Result%>%dplyr::select("Opening Extent")->RE1
    
    Result%>%dplyr::select("Closing Extent")->RE2
    
    
    v <-data.frame(RE1,RE2)
    
    
    LandtransetionMatrix_Table<-table(v)*resalution*resalution*1/10000
    
    RE1<-table(RE1)*resalution*resalution*1/10000
    RE2<-table(RE2)*resalution*resalution*1/10000
    
    opening<-paste0(Years_Uploded()$Years[1]," Total Area ha")
    closing<-paste0(Years_Uploded()$Years[2]," Total Area ha")
    
    
    rbind(RE1,LandtransetionMatrix_Table,RE2) ->result
    
    rownames(result)<-c(opening,data$Habitat,closing)
    
    
    data.frame(result)->final   
    
    
    round( final,0)->res
    
    data.frame(res)->habitent_Translation
    
    
  })
  
  
  output$Habitae_Change_Matrix_Table<-renderDataTable({
    
    
    data.frame(Habitae_Change_Transetion_Matrix())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  habitate_extent_Change<-eventReactive(input$RefresHabitatechange,{
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    
    D1
    
    
    
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening_Extent","Closing_Extent")
    
    
    data.frame(table(Result[1]))->v1
    
    names(v1)<-c("classes","pixcelcount")
    data.frame(table(Result[2]))->v2
    
    names(v2)<-c("classes2","pixcelcount2")
    
    
    
    
    left_join(data, v1, by = c("Habitat"="classes"))->R1
    left_join(data, v2, by = c("Habitat"="classes2"))->R2
    
    
    data.frame(R1,R2)%>%dplyr::select(Habitat,pixcelcount,pixcelcount2)->Percentage_chagne_table
    
    
    names(Percentage_chagne_table)<-c("Classes","Opening_Extent","Closing_Extent")
    
    
    
    # Extract the latitude values
    min_lat <- habitent_crop2()@extent@ymin
    max_lat <- habitent_crop2()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop2())[2]
    resalution <- resolution_degrees * conversion_factor
    
    
    
    
    Percentage_chagne_table$Opening_Extent<-round((Percentage_chagne_table$Opening_Extent)  *resalution*resalution*1/10000,0)
    
    Percentage_chagne_table$Closing_Extent<-round((Percentage_chagne_table$Closing_Extent)  *resalution*resalution*1/10000,0)
    
    Percentage_chagne_table%>%mutate(Difference_Area=Percentage_chagne_table[[3]]-(Percentage_chagne_table[[2]]))->change
    
    
    change$Difference_Percentage<-round((change$Difference_Area/ Percentage_chagne_table$Opening_Extent)*100,0)
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    
    
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening Extent","Closing Extent")
    Result%>%dplyr::select("Opening Extent")->RE1
    
    Result%>%dplyr::select("Closing Extent")->RE2
    
    
    v <-data.frame(RE1,RE2)
    
    LandtransetionMatrix_Table<-table(v)
    
    re1 <-as.data.frame.matrix(addmargins(LandtransetionMatrix_Table*resalution*resalution*1/10000),digits = 1)
    as.data.frame(re1%>%last()  )->dfg
    
    data.frame(    dfg%>%last()%>%dplyr::select(Sum) )->Reselet
    
    
    
    change$Opening_Extent_Percentag<-round((change$Opening_Extent/sum(Reselet$Sum))*100,0)
    
    change$Closing_Extent_Percentag<-round((change$Closing_Extent/sum(Reselet$Sum))*100,0)
    
    first_percentage<-paste0(Years_Uploded()$Years[1],"Percentag")
    secondt_percentage<-paste0(Years_Uploded()$Years[2],"Percentag")
    
    names(change)<-c("Classes",	Years_Uploded()$Years[1],	Years_Uploded()$Years[2],	"Difference_Area",	"Difference_Percentage",	first_percentage,	secondt_percentage)
    
    
    as.data.frame(change)
    
    
  })
  
  output$Habitae_Percentage_Change_Table<-renderDataTable({
    
    
    data.frame(habitate_extent_Change())
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  habitate_Tranformation_Table<-eventReactive(input$RefresHabitatechange,{
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent,xy=TRUE)%>%na.omit()->D1
    
    D1$NewTemp<-ifelse(D1$X0.1==D1$X0.2,0, 1)
    
    D1[D1$NewTemp==1,]->Res
    
    Res
    
    
    
    names(Res)<-c("lon","lat","Opening_Extent","Closing_Extent")
    Res%>%dplyr::select("lon","lat","Opening_Extent","Closing_Extent")->Reselt_data
    
    
    
    left_join(Reselt_data, data, by = c("Opening_Extent"="ID"))->R1
    
    left_join(R1, data, by = c("Closing_Extent"="ID"))->R2
    
    R2%>%dplyr::select("lon","lat","Habitat.x","Habitat.y")->R3
    
    R3$Habitents_Change<-paste0(R3$Habitat.x," to ",R3$Habitat.y)
    
    
    R3%>%dplyr::select(lon,lat,Habitents_Change)->result
    
    
    
    
    data.frame(result[3])->agg_R1
    
    
    
    table(agg_R1)->R3
    
    
    
    
    # Extract the latitude values
    min_lat <- habitent_crop1()@extent@ymin
    max_lat <- habitent_crop1()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop1())[2]
    resalution <- resolution_degrees * conversion_factor
    
    
    
    
    data.frame(R3)%>%mutate(Area_ha=R3*(resalution*resalution)*1/10000)->Result
    
    Result%>%dplyr::select('Habitents_Change','Area_ha')->Result_final
    
    
    data.frame(Result_final)->R2
    
    names(R2)<-c("Habitent Transfermation","Habitent Transfermation Area_ha")
    R2
    
    
    
    
  
 

  })
  
  output$Habitate_Tranformation<-renderDataTable({
    
    
    data.frame(habitate_Tranformation_Table())
    
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  habitate_Change_Matric <-eventReactive(input$RefresHabitatechange,{
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    D1
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening_Extent","Closing_Extent")
    
    
    data.frame(table(Result[1]))->v1
    
    names(v1)<-c("classes","pixcelcount")
    data.frame(table(Result[2]))->v2
    
    names(v2)<-c("classes2","pixcelcount2")
    left_join(data, v1, by = c("Habitat"="classes"))->R1
    left_join(data, v2, by = c("Habitat"="classes2"))->R2
    
    data.frame(R1,R2)%>%dplyr::select(Habitat,pixcelcount,pixcelcount2)->Percentage_chagne_table
    names(Percentage_chagne_table)<-c("Classes","Opening_Extent","Closing_Extent")

    
    
    # Extract the latitude values
    min_lat <- habitent_crop1()@extent@ymin
    max_lat <- habitent_crop1()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop1())[2]
    resalution <- resolution_degrees * conversion_factor
    
    
    
    
    
    
    Percentage_chagne_table$Opening_Extent<-round((Percentage_chagne_table$Opening_Extent)  *resalution*resalution*1/10000,0)
    Percentage_chagne_table$Closing_Extent<-round((Percentage_chagne_table$Closing_Extent)  *resalution*resalution*1/10000,0)
    names(Percentage_chagne_table)<-c("Classes",Years_Uploded()$Years[1],Years_Uploded()$Years[2])
    Percentage_chagne_table_total<- Percentage_chagne_table %>%   adorn_totals("row")
    Percentage_chagne_table_total$Netchange<-(Percentage_chagne_table_total[[3]]-(Percentage_chagne_table_total[[2]]))
    Percentage_chagne_table_total$Additions<-fifelse(Percentage_chagne_table_total$Netchange>0,Percentage_chagne_table_total$Netchange,0)
    Percentage_chagne_table_total$Additions[Percentage_chagne_table_total$Additions == 0] <- NA
    Percentage_chagne_table_total$Subtractions<-fifelse(Percentage_chagne_table_total$Netchange<0,Percentage_chagne_table_total$Netchange,0)
    Percentage_chagne_table_total$Subtractions[Percentage_chagne_table_total$Subtractions == 0] <- NA
    Habitent_Change<- Percentage_chagne_table_total%>%dplyr::select(Years_Uploded()$Years[1],Additions,Subtractions,Years_Uploded()$Years[2],Netchange)
    t(Habitent_Change)->Result
    data.frame(Result)->r4
    fd<-rbind(data,"Total")
    names(r4)<-fd$Habitat
    r4%>%dplyr::select(Total)->cq
    r4%>%as.data.frame.matrix()->cv
    cv[3,]->subb
    t(subb)->subbT
    rownames(subbT)<-NULL
    data.frame(subbT)%>%na.omit()->subbTD
    sum(subbTD)->substraction
    names(substraction)<-NULL
    substraction
    cv[2,]->add
    t(add)->addT
    rownames(addT)<-NULL
    data.frame(addT)%>%na.omit()->addTD
    sum(addTD)->Addition
    names(Addition)<-NULL
    Addition
    data.frame(Addition,substraction)
    cv%>%dplyr::select(Total)->cqs
    rownames(cqs)<-NULL
    data.frame( cqs$Total[1],Addition,substraction,cqs$Total[4],cqs$Total[5])->habitaTotal
    names(habitaTotal)<-NULL
    cv$Total<-t(habitaTotal)
    data.frame(cv)
  })
  
  Change_matriex <-reactive({
    
    myDF <- cbind(Habitates = rownames(Habitae_Change_Transetion_Matrix()), Habitae_Change_Transetion_Matrix())
    
    
  })
  Asset_Account<-reactive({
    
    myDF <- cbind(Habitates = rownames(habitate_Change()), habitate_Change())
    
    
  })
  
  
  
  
  
  output$Habitate_change_table <- downloadHandler(
    
    filename = function() {
      paste0("SEEAChange", "_Table", ".xlsx")
    },
    content = function(file){
      tbla<-data.frame(Change_matriex())
      tblb<-data.frame(Asset_Account())
      tblc<-data.frame(habitate_extent_Change())
      tbld<-data.frame(habitate_Tranformation_Table())
      
      
      
      sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
      names(sheets) <- c("SEEA change matrix",
                        "Ecosystem Extent Asset Account",
                        "Habitat percentage change table",
                        "Habitent Transfermation",
                        
                         
                         
      )
      
      
      writexl::write_xlsx( sheets, path = file) # saving the file
    }
  ) 
  
  Years_Uploded<-reactive({
    
    file1 <- input$csvFile8
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    years<-data.frame(data)
  })%>%bindCache(input$csvFile8)
  
  
  
  
  habitates_Change_Plot<-eventReactive(input$RefresHabitatechange,{
    
    data.frame(habitate_extent_Change()[1],habitate_extent_Change()[2],habitate_extent_Change()[3])->result
    
    amBarplot(x = names(result)[1], y = c( names(result)[2],  names(result)[3]), data = result, legend = TRUE, legendPosition = "right",
              width = 0.5)%>%setChartCursor()
  })
  
  habitates_Change_Percentage_Plot<-eventReactive(input$RefresHabitatechange,{
    
    data.frame(habitate_extent_Change()[1],habitate_extent_Change()[6],habitate_extent_Change()[7])->result
    
    amBarplot(x = names(result)[1], y = c( names(result)[2],  names(result)[3]), data = result, legend = TRUE, legendPosition = "right",
              width = 0.5)
  })
  
  
  habitates_dt_area<-eventReactive(input$RefresHabitatechange,{
    
    
    data.frame(habitate_extent_Change()[1],habitate_extent_Change()[4])->habitatePercentages
    
    
    
    names(habitatePercentages)<-c(	"Classes","Difference In Area")
    
    amBarplot(x = "Classes", y = c("Difference In Area"), data = habitatePercentages)%>% 
      amOptions(legend = TRUE,legendPosition = "right")%>%setChartCursor()
  })
  
  
  
  
  habitates_dt_Percentage<-eventReactive(input$RefresHabitatechange,{
    
    
    data.frame(habitate_extent_Change()[1],habitate_extent_Change()[5])->habitatePercentages
    
    
    
    names(habitatePercentages)<-c(	"Classes","Difference In Percentage")
    
    amBarplot(x = "Classes", y = c("Difference In Percentage"), data = habitatePercentages)%>% 
      amOptions(legend = TRUE,legendPosition = "right")%>%setChartCursor()
  })
  
  
  
  
  
  
  # habituate Condition data Opening_Year  
  Opening_Year<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    masked <- mask(x = data1, mask = adminBoundaries)    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    return(cropped)
    
  })
  
  
  output$Habitent_condition <- renderLeaflet({
    
    pal <- colorNumeric(input$color_ramp, values(Opening_Year()[[input$Opening_stock]]),
                        na.color = "transparent")
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addScaleBar() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values(Opening_Year()[[input$Opening_stock]]),
        title = "Legend"
      ) %>%
      addRasterImage(
        Opening_Year()[[input$Opening_stock]], 
        colors = pal, 
        opacity = 0.8, 
        group = "Condition Indicator"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Condition Indicator"),
        options = layersControlOptions(collapsed = TRUE))
  }) %>% 
    bindEvent(Opening_Year(), input$Opening_stock)
  
  
  Opening_Condition_statis<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    as.data.frame(Opening_Year())%>%na.omit()->stats_Opening
    
    data.frame(lapply(stats_Opening, min))->minCondition
    
    data.frame(lapply(stats_Opening, mean))->mannCondition
    data.frame(lapply(stats_Opening, max))->maxCondition
    data.frame(lapply(stats_Opening, max))->SDCondition
    
    rbind(minCondition,mannCondition,maxCondition,SDCondition)->Result_Closing_Condition
    
    
    row.names(Result_Closing_Condition)<-c("Min","Mean","Max","SD")
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    names(data) -> dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    as.data.frame(data) -> df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    data
    Re<-round(as.data.frame(Result_Closing_Condition),3)
    
    names(Re)<-data$habitents
    Re
    
  })
  
  
  
  
  habitentes_DT<-reactive({
    
    
    
    min_Opening<-as.data.frame(      Habitent_condetion_data_min_reactive())
    min_Opening_Melt<-melt(min_Opening)
    min_Opening_Melt$Statistics<-'Min'
    mean_Opening<-as.data.frame(      Habitent_condetion_data_mean_reactive())
    mean_Opening_Melt<-melt(mean_Opening)
    mean_Opening_Melt$Statistics<-'Mean'
    
    mex_Opening<-as.data.frame(      Habitent_condetion_data_mex_reactive())
    mex_Opening_Melt<-melt(mex_Opening)
    mex_Opening_Melt$Statistics<-'Max'
    
    SD_Opening<-as.data.frame(      Habitent_condetion_data_SD_reactive())
    SD_Opening_Melt<-melt(SD_Opening)
    SD_Opening_Melt$Statistics<-'SD'
    
    # Hear We Can Generate the Condition of the habitat  Opening Time Period 
    
    data.frame(bind_rows(min_Opening_Melt,mean_Opening_Melt,mex_Opening_Melt,SD_Opening_Melt))->Opening_Condition_Habitates
    
    # Hear We Can Generate the Condition of the habitat  Closing Time Period 
    
    
    min_Closing<-as.data.frame(      Habitent_condetion_data_min_reactiveClosing())
    min_Closing_Melt<-melt(min_Closing)
    min_Closing_Melt$Statistics<-'Min'
    
    mean_Closing<-as.data.frame(      Habitent_condetion_data_mean_reactiveClosing())
    mean_Closing_Melt<-melt(mean_Closing)
    mean_Closing_Melt$Statistics<-'Mean'
    
    mex_Closing<-as.data.frame(      Habitent_condetion_data_mean_reactiveClosing())
    mex_Closing_Melt<-melt(mex_Closing)
    mex_Closing_Melt$Statistics<-'Max'
    
    SD_Closing<-as.data.frame(      Habitent_condetion_data_mean_reactiveClosing())
    
    SD_Closing_Melt<-melt(SD_Closing)
    SD_Closing_Melt$Statistics<-'SD'
    
    data.frame(bind_rows(min_Closing_Melt,mean_Closing_Melt,mex_Closing_Melt,SD_Closing_Melt))->Cloasing_Condition_Habitates
    
    data.frame(Opening_Condition_Habitates,Cloasing_Condition_Habitates)->Habitates_Dt
    names(Habitates_Dt)<-c('Habitat',	'Condition.variable.Opening',	'Opening.Stockes'	,'Statistics','Habitat.1',	'Condition.variable.Clsoing',	'Clsoing.Stockes','Statistics.1')
    Result<-Habitates_Dt%>%dplyr::select('Habitat','Statistics',	'Condition.variable.Opening',	'Opening.Stockes'	,	'Condition.variable.Clsoing',	'Clsoing.Stockes')

    Result$Nectchange<-round(Result$Clsoing.Stockes-Result$Opening.Stockes,3)
  #  split_data <- strsplit(as.character(Result$Condition.variable.Opening), '/')
    split_data <- cSplit(Result, "Condition.variable.Opening", "/", fixed=FALSE)
    
    split_data2 <- cSplit(Result, "Condition.variable.Clsoing", "/", fixed=FALSE)

    Finale_Dt<-Result%>%dplyr::select('Habitat','Statistics','Opening.Stockes','Clsoing.Stockes','Nectchange')
    
    Result_final<-data_frame(Finale_Dt,split_data2$Condition.variable.Clsoing_2,split_data2$Condition.variable.Clsoing_4)

    
    names(Result_final)<-c('Habitat','Statistics',unique(split_data$Condition.variable.Opening_1),unique(split_data2$Condition.variable.Clsoing_1),'Nectchange','Condition','Units')
    Result_final
    
    file1 <- input$csvFile7
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    merged_df <- merge(Result_final, data, by = c("Habitat", "Statistics","Condition" , "Habitat","Statistics","Condition"),all.x = TRUE)
    
    merged_df
    
      })
  
  output$Condition_habitentes<-renderDataTable({
    
    
    data.frame(habitentes_DT())
  },    
  style = "bootstrap",
  class = "table table-hover",
  filter = "none",
  options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    scrollX = TRUE, # enable horizontal scrolling
    scrollY = TRUE
  ))
  
  # Step 4 Condition Opening Stasticies  
  
  output$Condition_Change_habit_stats <- downloadHandler(
    filename = function(){"Habitate Condition Change Stasticies.csv"}, 
    content = function(fname){
      
      
      write.csv(habitentes_DT(), fname)
    }
  )
  
  
  output$Condition_Opening_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Opening.csv"}, 
    content = function(fname){
      
      
      write.csv(Opening_Condition_statis(), fname)
    }
  )
  
  output$Condition_Opening<-renderDataTable({
    
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    names(data) -> dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    as.data.frame(data) -> df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    data
    Re<-round(as.data.frame(Opening_Condition_statis()),3)
    names(Re)<-data$habitents
    Re
  
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))%>%bindEvent(Opening_Condition_statis(),input$Opening_stock,Opening_Year())
  
  
  
  condtion_habitents_opening_stackes<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    as.data.frame(habitent_crop1(),xy=TRUE)->habitent_opening
    as.data.frame(habitent_crop1(),xy=TRUE)[1:2]->long_lat
    extract(Opening_Year(),long_lat)->CM_RESss
    names(Opening_Year())->data_names_opne
    as.data.frame(data_names_opne)->reselet   
    names(CM_RESss) <-c(data_names_opne)
    data.frame(habitent_opening,CM_RESss)->resulT_Dt
    na.omit(resulT_Dt)->final
    final[-1:-2]->Result
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(Result[1])<-"Habiten"
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    left_join(data,Result, by = c("ID"="X0"))->R1
    R1
    
  })
  
  
  
  
  
  
  
  Habitent_condetion_data_min_reactive<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_opening_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_opening_stackes(),
                    by = list( condtion_habitents_opening_stackes()$Habitat),
                    FUN = min)
    
    agg_mean<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_mean)<-new_dat$habitents
    as.data.frame( agg_mean)->min
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(min,3)
    
  })
  
  Habitent_condetion_data_mean_reactive<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    as.data.frame(data)->df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_opening_stackes())%>%dplyr::select(-1,-3)
    
    agg = aggregate(condtion_habitents_opening_stackes(),
                    by = list( condtion_habitents_opening_stackes()$Habitat),
                    FUN = mean)
    agg_mean<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_mean)<-new_dat$habitents
    as.data.frame( agg_mean)->mean
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(mean,3)
  })
  
  Habitent_condetion_data_mex_reactive<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_opening_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_opening_stackes(),
                    by = list( condtion_habitents_opening_stackes()$Habitat),
                    FUN = max)
    agg_mean<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_mean)<-new_dat$habitents
    as.data.frame( agg_mean)->mex
    
    
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(mex,3)
    
    
  })
  
  
  Habitent_condetion_data_SD_reactive<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_opening_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_opening_stackes(),
                    by = list( condtion_habitents_opening_stackes()$Habitat),
                    FUN = sd)
    agg_sd<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_sd)<-new_dat$habitents
    as.data.frame( agg_sd)->sd
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(sd,3)
    
  })
  
  
  output$Habitent_Condition_Opening_Stockes_Min_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_min_reactive())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  output$Habitent_Condition_Opening_Stockes_Mean_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_mean_reactive())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  output$Habitent_Condition_Opening_Stockes_Max_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_mex_reactive())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  output$Habitent_Condition_Opening_Stockes_SD_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_SD_reactive())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  # Download min Condition 
  output$Condition_Opening_Habitate_Min_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Min Opening.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_min_reactive(), fname)
    }
  )
  
  # Download Mean Condition 
  output$Condition_Opening__Habitate_Mean_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Mean Opening.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_mean_reactive(), fname)
    }
  )
  # Download Max Condition 
  output$Condition_Opening_Habitate_Max_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Max Opening.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_mex_reactive(), fname)
    }
  )
  
  # Download SD Condition 
  output$Condition_Opening_Habitate_SD_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition SD Opening.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_SD_reactive(), fname)
    }
  )
  
  
  
  
  # habituate Condition data Closing_Year
  
  Closing_Year<-eventReactive(input$RefreshPlotHabitateClosingYear,{
    inFile3<-input$layer4
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    masked <- mask(x = data1, mask = adminBoundaries)    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    
    
    file2 <- input$csvFile3
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    return(cropped)
  })
  
  
  
  output$Closing_stock_Habitent_condition <- renderLeaflet({
    
    pal <- colorNumeric(input$color_ramp2, values(Closing_Year()[[input$Closing_stock]]),
                        na.color = "transparent")
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addScaleBar() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values(Closing_Year()[[input$Closing_stock]]),
        title = "Legend"
      ) %>%
      addRasterImage(
        Closing_Year()[[input$Closing_stock]], 
        colors = pal, 
        opacity = 0.8, 
        group = "Condition Indicator"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Condition Indicator"),
        options = layersControlOptions(collapsed = TRUE))
  }) %>% 
    bindEvent(Closing_Year(), input$Closing_stock)
  # habituate Condition data Closing_Year
  
  
  
  closing_Condition_statis<-eventReactive(input$RefreshPlotHabitateClosingYear,{
    as.data.frame(Closing_Year())%>%na.omit()->stats_closing
    data.frame(lapply(stats_closing, min))->minCondition
    
    data.frame(lapply(stats_closing, mean))->mannCondition
    data.frame(lapply(stats_closing, max))->maxCondition
    data.frame(lapply(stats_closing, max))->SDCondition
    
    rbind(minCondition,mannCondition,maxCondition,SDCondition)->Result_Closing_Condition
    
    
    row.names(Result_Closing_Condition)<-c("Min","Mean","Max","SD")
    
    
    
   
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    as.data.frame(data) -> df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    data
    Re<-round(as.data.frame(Result_Closing_Condition),3)
    
    names(Re)<-data$habitents
    Re
    
  })
  
  
  output$HabitateChangeClosing<-renderDataTable({

    as.data.frame(closing_Condition_statis())
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))


  
  
  
  output$Condition_Closing_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Closing.csv"}, 
    content = function(fname){
      
      
      write.csv(closing_Condition_statis(), fname)
    }
  )
  
  
  
  condtion_habitents_Closing_stackes<-eventReactive(input$RefreshPlotHabitateClosingYear,{
    as.data.frame(habitent_crop2(),xy=TRUE)->habitent_closing
    as.data.frame(habitent_crop2(),xy=TRUE)[1:2]->long_lat
    extract(Closing_Year(),long_lat)->CM_RESss
    names(Closing_Year())->data_names_opne
    as.data.frame(data_names_opne)->reselet   
    names(CM_RESss) <-c(data_names_opne)
    data.frame(habitent_closing,CM_RESss)->resulT_Dt
    na.omit(resulT_Dt)->final
    final[-1:-2]->Result
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(Result[1])<-"Habiten"
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    left_join(data,Result, by = c("ID"="X0"))->R1
    R1
    
  })
  
  
  
  
  Habitent_condetion_data_min_reactiveClosing<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2],df[3])->dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_Closing_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_Closing_stackes(),
                    by = list( condtion_habitents_Closing_stackes()$Habitat),
                    FUN = min)
    agg_min<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_min)<-new_dat$habitents
    as.data.frame( agg_min)
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(agg_min,3)
  })
  
  
  Habitent_condetion_data_mean_reactiveClosing<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2],df[3])->dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_Closing_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_Closing_stackes(),
                    by = list( condtion_habitents_Closing_stackes()$Habitat),
                    FUN = mean)
    
    
    agg_min<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_min)<-new_dat$habitents
    as.data.frame( agg_min)
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(agg_min,3)
    
  })
  
  Habitent_condetion_data_max_reactiveClosing<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2],df[3])->dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_Closing_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_Closing_stackes(),
                    by = list( condtion_habitents_Closing_stackes()$Habitat),
                    FUN = max)
    agg_min<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_min)<-new_dat$habitents
    as.data.frame( agg_min)
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(agg_min,3)
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(agg_min,3)
    
  })
  
  
  Habitent_condetion_data_sd_reactiveClosing<-reactive({
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    names(data)->dt
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2],df[3])->dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    new_dat <- rbind(c( dt[2]), data)
    as.data.frame(condtion_habitents_Closing_stackes())%>%dplyr::select(-1,-3)
    agg = aggregate(condtion_habitents_Closing_stackes(),
                    by = list( condtion_habitents_Closing_stackes()$Habitat),
                    FUN = sd)
    agg_min<- as.data.frame(agg)%>%dplyr::select(-2,-3,-4)
    names(agg_min)<-new_dat$habitents
    as.data.frame( agg_min)
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(agg_min,3)
    
  })
  
  output$Habitent_Condition_Closing_Stockes_Min_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_min_reactiveClosing())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  output$Habitent_Condition_Closing_Stockes_Mean_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_mean_reactiveClosing())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  output$Habitent_Condition_Closing_Stockes_Max_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_max_reactiveClosing())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  output$Habitent_Condition_Closing_Stockes_SD_Data<-renderDataTable({
    as.data.frame(Habitent_condetion_data_sd_reactiveClosing())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  # Condition Acounting min 
  output$Condition_Closing_Habitate_Min_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Min Closing.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_min_reactiveClosing(), fname)
    })
  
  
  
  # Condition Accounting mean 
  
  output$Condition_Closing_Habitate_Mean_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Mean Closing.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_mean_reactiveClosing(), fname)
    }
  )
  
  
  
  
  # Condition Accounting max 
  
  output$Condition_Closing_Habitate_Max_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition Max Closing.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_max_reactiveClosing(), fname)
    }
  )
  
  # Condition Accounting SD 
  
  output$Condition_Closing_Habitate_SD_Total_Step_3 <- downloadHandler(
    filename = function(){"Condition SD Closing.csv"}, 
    content = function(fname){
      write.csv(Habitent_condetion_data_sd_reactiveClosing(), fname)
    }
  )
  
  
  
  habi_Change_Condition<-eventReactive(input$RefreshPlotHabitateClosingYear,{
    
    inFile3<-input$layer3
    
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    as.data.frame(data)->reselt_data
    
    names(data1)<-reselt_data[[1]]
    
    as.data.frame(data1)%>%na.omit()->stats_Opening
    data.frame(lapply(stats_Opening, min))->minCondition
    
    data.frame(lapply(stats_Opening, mean))->mannCondition
    data.frame(lapply(stats_Opening, max))->maxCondition
    data.frame(lapply(stats_Opening, max))->SDCondition
    
    rbind(minCondition,mannCondition,maxCondition,SDCondition)->Result_Opening_Condition
    
    Result_Opening_Condition$Statistics<-c("Min","Mean","Max","SD")
    
    
    melt(Result_Opening_Condition)->Result
    
    
    
    # Closing 
    
    
    
    inFile3<-input$layer4
    
    
    if(is.null(inFile3))
      
      return(NULL)
    
    
    
    
    data2 <-stack( lapply(inFile3$datapath,raster))
    
    data = read.csv(file=file2$datapath)
    as.data.frame(data)->reselt_data
    names(data2)<-reselt_data[[1]]
    as.data.frame(data2)%>%na.omit()->stats_Closing
    data.frame(lapply(stats_Closing, min))->minConditionClosing
    data.frame(lapply(stats_Closing, mean))->mannConditionClosing
    data.frame(lapply(stats_Closing, max))->maxConditionClosing
    data.frame(lapply(stats_Closing, max))->SDConditionClosing
    rbind(minConditionClosing,mannConditionClosing,maxConditionClosing,SDConditionClosing)->Result_Closing_Condition
    melt(Result_Closing_Condition)->Resultclosing
    cbind(Result,Resultclosing)->Resultcombine
    Resultcombine[-4]->ResultcombineHabitates
    data.frame(ResultcombineHabitates)
    ResultcombineHabitates$Netchange<-ResultcombineHabitates[4]-ResultcombineHabitates[3]
    ResultcombineHabitates$ConditionStatistics<-paste(ResultcombineHabitates[[1]],ResultcombineHabitates[[2]])
    result<-data.frame(ResultcombineHabitates)
    
    file2 <- input$csvFile5
    if (is.null(file2)) { 
      return() 
    } 
    dataReferences = read.csv(file=file2$datapath)
    Rl<-data.frame(dataReferences)
    Rl$ConditionStatistics<-paste(Rl[[1]],Rl[[2]])
    left_join(result,Rl,by=c("ConditionStatistics"))->Final_result
    data.frame(Final_result)->Final_result_Data
    Final_result_Data%>%dplyr::select(variable,Statistics.x,value,value.1,Netchange,Measurement.Unit,Reference.Level.Target)->Final_result_Datasets
    data.frame(Final_result_Datasets)->habitate_result
    names(habitate_result)<-c("Condition.Indicator","Statistics","Opening","Closing","Netchange","Measurement.Unit","Reference Level/Target")
    habitate_result_data<-habitate_result%>%dplyr::select("Opening","Closing")
    habitate_result_data_nectchange_data<-habitate_result%>%dplyr::select("Netchange")
    
    # Replace non-numeric values with NA
    
    data.frame(habitate_result%>%dplyr::select("Condition.Indicator",'Statistics'),round(habitate_result_data,2),round( habitate_result_data_nectchange_data$Netchange,2),
               habitate_result%>%dplyr::select("Measurement.Unit","Reference Level/Target") )->habitates_change
    
    
    names(habitates_change)<-c("Condition.Indicator","Statistics",Years_Uploded()$Years[1],Years_Uploded()$Years[2],"Netchange","Measurement.Unit","Reference Level/Target")
    
    
    data.frame(habitates_change)
    
  })


  Opening_Stock_Condition<-reactive({
    
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2])->dd
    data$habitents <- paste(dd$Condition.Indicator,"_Units_", data$Measurement.Unit)
    new_dat <- rbind(c( "Statistics"), data)
    as.data.frame(Opening_Year_Condition_Crop(),xy=TRUE)%>%na.omit()->result
    result%>%dplyr::select(-x,-y)->Finel_result
    min_dt<-data.frame(c(lapply(Finel_result, min)))
    average_dt<-data.frame(c(lapply(Finel_result, mean)))
    maximum_dt<-data.frame(c(lapply(Finel_result, max)))
    sd_dt<-data.frame(c(lapply(Finel_result, sd)))
    data.frame(c("Min","Mean","Max","SD"))->Statistics
    names(Statistics)<-NULL
    Res<-data.frame(   Statistics, rbind(min_dt,average_dt,maximum_dt,sd_dt))
    names(Res)<-new_dat$habitents
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(Res,3)
  })
  
  
  
  Closing_Stock_Condition<-reactive({
    file1 <- input$csvFile3
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    as.data.frame(data)->df
    data.frame(df[1],df[2])->dd
    data$habitents <- paste(dd$Condition.Indicator,"_Units_",data$Measurement.Unit)
    new_dat <- rbind(c( "Statistics"), data)
    as.data.frame(Closing_Year_Condition_Crop(),xy=TRUE)%>%na.omit()->result
    result%>%dplyr::select(-x,-y)->Finel_result
    min_dt<-data.frame(c(lapply(Finel_result, min)))
    average_dt<-data.frame(c(lapply(Finel_result, mean)))
    maximum_dt<-data.frame(c(lapply(Finel_result, max)))
    sd_dt<-data.frame(c(lapply(Finel_result, sd)))
    data.frame(c("Min","Mean","Max","SD"))->Statistics
    names(Statistics)<-NULL
    Res<-data.frame(   Statistics, rbind(min_dt,average_dt,maximum_dt,sd_dt))
    names(Res)<-new_dat$habitents
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(Res,3)
    
    
  })
  
  
  
  habitentes<-reactive({
    data.frame(Opening_Stock_Condition())->re
    data.frame(Closing_Stock_Condition())->red
    melt_mind1<-melt(re, id="Statistics")
    melt_mind2<-melt(red, id="Statistics")
    data.frame(melt_mind1,melt_mind2)->habitent_chagne_DT
    habitent_chagne_DT[] <- lapply(habitent_chagne_DT, as.integer)
    file1 <- input$csvFile5
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    #left_join(habitent_chagne_DT, data,by=c("variable"="ID")  )->dt_join
    habitent_chagne_DT%>%dplyr::select(variable)->HB_ID
    data.frame(melt_mind1,melt_mind2)->habitent_chagne
    data.frame(habitent_chagne ,HB_ID)->rty
    rty%>%dplyr::select(
      "variable.2",
      "Statistics",
      "value",
      "value.1")->habi_RF
    names(habi_RF)<-c("ID","Statistics",
                      "Opening Value", "Closing Value")
    data.frame(habi_RF)
    left_join(habi_RF,data,by=c("ID"="ID"))->hbID
    data.frame(hbID)%>%dplyr::select(-ID,Statistics
                                     
    )->hb
    hbd <- hb%>%dplyr::select(Condition.Indicator,Statistics,Measurement.Unit,Opening.Value,Closing.Value,Reference.Level.Target)
    colnames(hbd)[4]<-Years_Uploded()$Years[1]
    colnames(hbd)[5]<-Years_Uploded()$Years[2]
    names(hbd)<-c('Condition.Indicator','Statistics','Measurement.Unit',Years_Uploded()$Years[1],Years_Uploded()$Years[2],'Reference.Level.Target')
    hbd
  })
  
  
  

  
  output$Condition_Change<-renderDataTable({
    
 
as.data.frame(    habi_Change_Condition())

    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ) )%>%bindEvent(habi_Change_Condition())
  

  
  
  # Step 3 Condition Opening Stasticies  
  
  output$Condition_Change_stats <- downloadHandler(
    filename = function(){"Condition Change Stasticies.csv"}, 
    content = function(fname){
      
      
      write.csv(habi_Change_Condition(), fname)
    }
  )
  
  

  
  habitates_Percentage<-eventReactive(input$RefreshPlotHabitate,{
    
    habitates_data()%>%dplyr::select(Habitat.Classes,Percentage)->result
    
    
    names(result)<-c("label","value")
    
    
    amBarplot(main = "Habitat Percentage",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE)
    
    
  })
  habitates_dt<-reactive({
    data.frame(plot4()[1],plot4()[6],plot4()[7])->habitatePercentages
    first_percentage<-paste0(Years_Uploded()$Years[1])
    secondt_percentage<-paste0(Years_Uploded()$Years[2])
    names(habitatePercentages)<-c(	"Classes",first_percentage,secondt_percentage)
    amBarplot(x = "Classes", y = c(first_percentage, secondt_percentage), data = habitatePercentages)%>% 
      amOptions(legend = TRUE)%>%setChartCursor()
  })
  
  
  Habitentent_condition_reactive<-reactive({
    condtion_habitents_opening_stackes()->opening
    condtion_habitents_Closing_stackes()->closing
    data.frame(opening,closing)->hbitates
    opening$Tume_period<-Years_Uploded()$Years[1]
    closing$Tume_period<-Years_Uploded()$Years[2]
    rbind(opening,closing)->Combine_Habitates
    Combine_Habitates_filter<- as.data.frame(Combine_Habitates)%>%dplyr::select(-1,-3)
    agg = aggregate(Combine_Habitates_filter,
                    by = list( Combine_Habitates_filter$Habitat,Combine_Habitates_filter$Tume_period),
                    FUN = min)
    
    Filter_aggrigation<-agg%>%dplyr::select(-1,-2)
    Filter_agg_Min<-Filter_aggrigation %>% arrange(Habitat)
    split(Filter_agg_Min,Filter_agg_Min$Tume_period)->Habitat_split
    
    data.frame(    Habitat_split[1])->Opening   
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data_n1 = read.csv(file=file2$datapath)
    Condition_unites<-paste0(data_n1$Condition.Indicator,"/ ", "  Min /",data_n1$Measurement.Unit)
    names(Opening)<-c("Habitat",Condition_unites,"Tume_period")
    melt(Opening)->Opening_table
    t(Opening_table)->Opening_table_transfermation
    Opening_table_transfermation[-2,]->Opening_table_transfermation_habitates
    data.frame(     Opening_table_transfermation_habitates[1,]) ->Opening_table_transfermation_habitates_names
    names(Opening_table_transfermation_habitates_names)<-"Habitat"
    Opening_table_transfermation_habitates_names
    data.frame(Opening_table_transfermation_habitates)->Opening_table_transfermation_habitates_dataFrames
    habitates_names<-paste0(Opening_table_transfermation_habitates_dataFrames$Habitates)
    names(Opening_table_transfermation_habitates_dataFrames)<-c(habitates_names)
    #closing
    data.frame(    Habitat_split[2])->closing   
    names(closing)<-c("Habitat",Condition_unites,"Tume_period")
    melt(closing)->closing_table
    t(closing_table)->closing_table_transfermation
    closing_table_transfermation[-2,]->Closing_table_transfermation_habitates
    data.frame(Closing_table_transfermation_habitates)->Closing_table_transfermation_habitates_dataFrames
    names(Closing_table_transfermation_habitates_dataFrames)<-c(habitates_names)
    bind_rows(Opening_table_transfermation_habitates_dataFrames,Closing_table_transfermation_habitates_dataFrames)->Openin_Closing_Combine
    Openin_Closing_Combine[-4:-5,]->Final_Result
    row.names(Final_Result)<-c("Habitant","Condition Indicator / Units",Years_Uploded()$Years[1],Years_Uploded()$Years[2])
    opnening<-as.numeric(Final_Result[3,])
    Closing<-as.numeric(Final_Result[4,])
    data.frame(opnening,Closing)->condition_Change
    names(condition_Change)<-c("opening","Closing")
    habitate_change_Deff<-condition_Change%>%mutate(Netchange=Closing-opening)
    habitate_change_Deff%>%  dplyr::select(-opening,-Closing)->Netchange
    t(Final_Result)->Final_Result_HA
    rbind(t(Final_Result_HA)  ,t(Netchange))->Habitent_condition
    hab_nmaes<-Habitent_condition[1,]
    names(Habitent_condition) <- hab_nmaes
    t(Habitent_condition)->v
    data.frame(v)->Q
    rownames(Q)<-NULL
    Q
    
  })
  
  habi_Change_Conditiondatasets<-reactive({
    
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    as.data.frame(data)->reselt_data
    reselt_data$Opening<-paste0(reselt_data[[1]], unique(reselt_data[[2]]))
    names(data1)<-reselt_data$Opening
    
    # Closing 
   
    
    inFile3<-input$layer4
    if(is.null(inFile3))
      return(NULL)
    data2 <-stack( lapply(inFile3$datapath,raster))
    data = read.csv(file=file2$datapath)
    as.data.frame(data)->reselt_data
    reselt_data$Closing<-paste0(reselt_data[[1]], unique(reselt_data[[2]]))
    names(data2)<-reselt_data$Closing
    stack(data1,data2)->result
    
  })
  
  Merain_habitens_stats_stat_table<-eventReactive(input$RefresHabitatechange,{
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    
    D1
    
    
    
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening_Extent","Closing_Extent")
    
    
    data.frame(table(Result[1]))->v1
    
    names(v1)<-c("classes","pixcelcount")
    data.frame(table(Result[2]))->v2
    
    names(v2)<-c("classes2","pixcelcount2")
    
    
    
    
    left_join(data, v1, by = c("Habitat"="classes"))->R1
    left_join(data, v2, by = c("Habitat"="classes2"))->R2
    
    
    data.frame(R1,R2)%>%dplyr::select(Habitat,pixcelcount,pixcelcount2)->Percentage_chagne_table
    
    
    names(Percentage_chagne_table)<-c("Classes","Opening_Extent","Closing_Extent")
    
    
    
    # Extract the latitude values
    min_lat <- habitent_crop2()@extent@ymin
    max_lat <- habitent_crop2()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(habitent_crop2())[2]
    resalution <- resolution_degrees * conversion_factor
    
    
    
    
    Percentage_chagne_table$Opening_Extent<-round((Percentage_chagne_table$Opening_Extent)  *resalution*resalution*1/10000,0)
    
    Percentage_chagne_table$Closing_Extent<-round((Percentage_chagne_table$Closing_Extent)  *resalution*resalution*1/10000,0)
    
    Percentage_chagne_table%>%mutate(Difference_Area=Percentage_chagne_table[[3]]-(Percentage_chagne_table[[2]]))->change
    
    
    change$Difference_Percentage<-round((change$Difference_Area/ Percentage_chagne_table$Opening_Extent)*100,0)
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent)%>%na.omit()->D1
    
    names(D1)<-c("Opening_Extent","Closing_Extent")
    
    
    left_join(D1, data, by = c("Opening_Extent"="ID"))->R1
    left_join(D1, data, by = c("Closing_Extent"="ID"))->R2
    
    names(R1[3])<-"Opening Extent"
    
    names(R2[3])<-"Closing Extent"
    
    data.frame(c(R1[3],R2[3]))->Result
    
    names(Result)<-c("Opening Extent","Closing Extent")
    Result%>%dplyr::select("Opening Extent")->RE1
    
    Result%>%dplyr::select("Closing Extent")->RE2
    
    
    v <-data.frame(RE1,RE2)
    
    LandtransetionMatrix_Table<-table(v)
    
    re1 <-as.data.frame.matrix(addmargins(LandtransetionMatrix_Table*resalution*resalution*1/10000),digits = 1)
    as.data.frame(re1%>%last()  )->dfg
    
    data.frame(    dfg%>%last()%>%dplyr::select(Sum) )->Reselet
    
    
    
    change$Opening_Extent_Percentag<-round((change$Opening_Extent/sum(Reselet$Sum))*100,0)
    
    change$Closing_Extent_Percentag<-round((change$Closing_Extent/sum(Reselet$Sum))*100,0)
    
    first_percentage<-paste0(Years_Uploded()$Years[1],"Percentag")
    secondt_percentage<-paste0(Years_Uploded()$Years[2],"Percentag")
    
    names(change)<-c("Classes",	Years_Uploded()$Years[1],	Years_Uploded()$Years[2],	"Difference_Area",	"Difference_Percentage",	first_percentage,	secondt_percentage)
    
    as.data.frame(change)->fddd
    
    split(fddd,fddd$Classes)
    
    
  })
  
  
  
  
output$vbox1 <- renderInfoBox({
  dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
  d <- as.data.frame(dagds) %>% dplyr::select(Years_Uploded()$Years[1])
  q <- paste0(round(d)," " ,"ha")
  year <- as.character(Years_Uploded()$Years[1])

  infoBox(
    title = HTML(paste("<h1 style='font-size:17px; font-weight:bold;color:#555555;margin-top:0;'>", year, "<br></h1>", sep = "")),
    value = HTML(paste("<p >", q,"<br></p>", sep = "")),
    icon = icon("list", lib = "glyphicon"),
    color = "navy",
    fill = FALSE
    
  )
})
  

output$vbox2 <- renderInfoBox({
  dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
  d <- as.data.frame(dagds) %>% dplyr::select(Years_Uploded()$Years[2])
  q <- paste0(round(d)," " ,"ha")
  year <- as.character(Years_Uploded()$Years[2])
  
  infoBox(
    title = HTML(paste("<h1 style='font-size:17px; font-weight:bold;color:#555555;margin-top:0;'>", year, "<br></h1>", sep = "")),
    value = HTML(paste("<p >", q,"<br></p>", sep = "")),
    icon = icon("list", lib = "glyphicon"),
    color = "navy",
    fill = FALSE
    
  )
})
output$vbox3 <- renderInfoBox({
  dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
  d <- as.data.frame(dagds) %>% dplyr::select('Difference_Area')
  q <- paste0(round(d)," " ,"ha")
  
  infoBox(
    title = HTML(paste("<h1 style='font-size:17px; font-weight:bold;color:#555555;margin-top:0;'>", 'Change in Area' , "<br></h1>", sep = "")),
    value = HTML(paste("<p >", q,"<br></p>", sep = "")),
    icon = icon("list", lib = "glyphicon"),
    color = "navy",
    fill = FALSE
    
  )
})

output$vbox4 <- renderInfoBox({
  dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
  d <- as.data.frame(dagds) %>% dplyr::select('Difference_Percentage')
  q <- paste0(round(d)," " ,"ha")
  
  infoBox(
    title = HTML(paste("<h1 style='font-size:17px; font-weight:bold;color:#555555;margin-top:0;'>", 'Change in %' , "<br></h1>", sep = "")),
    value = HTML(paste("<p >", q,"<br></p>", sep = "")),
    icon = icon("list", lib = "glyphicon"),
    color = "navy",
    fill = FALSE
    
  )
})


output$gauge <- renderAmCharts({
  
  dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
  first_percentage <- paste0(Years_Uploded()$Years[1], "Percentag")
  
  as.data.frame(dagds) %>% dplyr::select(first_percentage) -> d
  
  as.numeric(d) -> qd
  
  round(qd, 3) -> qe
  
  Opening <- paste0(Years_Uploded()$Years[1])
 # Opening <- paste0("<span style='font-family: Arial; font-size: 16px; color: #FF0000;'>", Years_Uploded()$Years[1], "</span>")
  
  bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), color = c("#071952", "#071952", "#071952"), stringsAsFactors = FALSE)
  
  
  amAngularGauge(x = qe,   text = "%",main = Opening,
                 mainColor = "#68838B", mainSize = 20, creditsPosition = "bottom-right",bands = bands)
  
  
})
  output$gauges <- renderAmCharts({
    
    dagds <- Merain_habitens_stats_stat_table()[[input$variable_3]]
    first_percentage <- paste0(Years_Uploded()$Years[2], "Percentag")
    
    as.data.frame(dagds) %>% dplyr::select(first_percentage) -> d
    
    as.numeric(d) -> qd
    
    round(qd, 3) -> qe
    
    closing <- paste0(Years_Uploded()$Years[2])
    
    
    bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100),color = c("#071952", "#071952", "#071952"),stringsAsFactors = FALSE)
    
    #flexdashboard::gauge(qe, min = 0, max = 100, symbol = '%', label = Opening,
    ##                  flexdashboard::gaugeSectors(success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#071952")
    #               ))
    
    amAngularGauge(x = qe,  text = "%",main = closing,
                   mainColor = "#68838B", mainSize = 20, creditsPosition = "bottom-right",bands = bands)
    
    
  })
  
  
  
  Habitentdata<-reactive({
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    stack(habitent_crop1(),habitent_crop2())->Exo_Habitent
    as.data.frame(Exo_Habitent,xy=TRUE)%>%na.omit()->D1
    
    D1$NewTemp<-ifelse(D1$X0.1==D1$X0.2,0, 1)
    
    D1[D1$NewTemp==1,]->Res
    
    Res
    
    names(Res)<-c("lon","lat","Opening_Extent","Closing_Extent")
    Res%>%dplyr::select("lon","lat","Opening_Extent","Closing_Extent")->Reselt_data
    
    
    
    left_join(Reselt_data, data, by = c("Opening_Extent"="ID"))->R1
    
    left_join(R1, data, by = c("Closing_Extent"="ID"))->R2
    
    R2%>%dplyr::select("lon","lat","Habitat.x","Habitat.y")->R3
    
    R3$Habitents_Change<-paste0(R3$Habitat.x," to ",R3$Habitat.y)
    
    
    R3_result<-R3%>%dplyr::select(lon,lat,Habitents_Change)
    
    extract( habi_Change_Conditiondatasets(),R3_result[1:2])->CM_RESss
    
    cbind(CM_RESss,R3_result)->result
    na.omit(result)->res_fl
    as.data.frame(res_fl)->hab_split
    
    split(hab_split,hab_split$Habitents_Change)
 
    
    
    
  })%>% bindEvent(habitent_crop1(),habitent_crop2())
  
  
  
  Condetion_Habitent_Tranformaion_Markers_map<-reactive({
    data.frame(names(Habitentdata()))->df
    names(df) <- NULL
    
    split(df,df)->r
    
    res<- paste0('Habitate Transformation From',r[[input$Heat_habitent]])
    
    leaflet(data =Habitentdata()[[input$Heat_habitent]])%>%    # add other map elements here
      addControl(htmltools::div(
        style = "text-align: center; font-size: 14px; font-weight: bold;", 
        res), 
        position = "topright")%>%
      addTiles(group = "OSM (default)")%>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addAwesomeMarkers(lng = ~lon   , lat = ~lat, icon = awesome,popup = popupTable(Habitentdata()[[input$Heat_habitent]]), clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)                              
                                                                                                                           
                                                                                                                           
                                                                                                                           
      ) %>%addLayersControl(
        baseGroups = c( "Esri","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
    
  })%>% bindCache(Habitentdata(),input$Merain_Habitentes_Markers_Transetion)
  
  
  
  
  output$Condetion_Habitent_Tranformaion_Markers<-renderLeaflet({
    
    Condetion_Habitent_Tranformaion_Markers_map()
    
  })
  
  output$Total_Habitate_Extent_condition_change_table <- downloadHandler(
    
    filename = function() {
      paste0("SEEAChange", "_Table", ".xlsx")
    },
    content = function(file){
      tbla<-data.frame(Change_matriex())
      tblb<-data.frame(Asset_Account())
      tblc<-data.frame(habitate_extent_Change())
      tbld<-data.frame(habitate_Tranformation_Table())
      tble<-data.frame(habi_Change_Condition())
      tblf<-data.frame(habitentes_DT())
      
      
      
      
      
      sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
      names(sheets) <- c("SEEA change matrix",
                         "Ecosystem Extent Asset Account",
                         "Habitat percentage change table",
                         "Habitent Transfermation",
                         "Overall condition change",
                         "Habitat condition change"
                         
                         
                         
      )
      
      
      writexl::write_xlsx( sheets, path = file) # saving the file
    }
  ) 
output$HabitentChange<- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "HabitentChange.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "HabitentChange.Rmd")
      file.copy("HabitentChange.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(o=habchange(),
                     m=Habitae_Change_Transetion_Matrix(),
                     n= habitate_Change(),
                     q=habitate_extent_Change(),
                     w=habitates_Change_Percentage_Plot(),
                     z=habitates_Change_Plot(),
                     e=habitates_dt_Percentage(),

                     s=metardata()
                     )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  # Hot spot maps  Report 
  output$Hot_Spot_Reports <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Habitate_Change_HotSpot_Markers_Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Habitate_Change_HotSpot_Markers_Report.Rmd")
      file.copy("Habitate_Change_HotSpot_Markers_Report.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list( n=Habitate_Hotspot(),d=metardata()
                      
      ) 
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  # Hot spot maps  Report 
  output$Hot_Spot_Markers_Reports <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Habitate_Change_Markers_Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Habitate_Change_Markers_Report.Rmd")
      file.copy("Habitate_Change_Markers_Report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list( n=Habitate_Change_markers_data(),d=metardata()
                      
      ) 
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  # Condition opening extent report 
  output$OpeningStastics_Condition <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "OpeningStastics.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "OpeningStastics.Rmd")
      file.copy("OpeningStastics.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(m=Opening_Year(),n=round(as.data.frame(Opening_Condition_statis()),2),s=metardata())
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
  

  
  
  
  
  
  
  
  output$Condition_Habitate_Opening_Total_Step_3 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "OpeningConditionAccountingReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "OpeningConditionAccountingReport.Rmd")
      file.copy("OpeningConditionAccountingReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(m=Opening_Year(),
                     z=habitates_plote(),
                     n=round(as.data.frame(Opening_Condition_statis()),2),
                     q= as.data.frame(Habitent_condetion_data_min_reactive()),
                     w= as.data.frame(Habitent_condetion_data_mean_reactive()),
                     e=as.data.frame(Habitent_condetion_data_mex_reactive()),
                     r=as.data.frame(Habitent_condetion_data_SD_reactive()),
                     s=metardata())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  # Condition opening extent report 
  output$Closing_Report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ClosingStastics.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "ClosingStastics.Rmd")
      file.copy("ClosingStastics.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(m=Closing_Year(),n=round(as.data.frame(closing_Condition_statis()),2),s=metardata())
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
  
  output$Condition_Habitate_Closing_AccountingReport_Step_3 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ClosingConditionAccountingReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "ClosingConditionAccountingReport.Rmd")
      file.copy("ClosingConditionAccountingReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(m=Closing_Year(),
                     z=habitates_Closing(),
                     n=round(as.data.frame(closing_Condition_statis()),2),
                     q=  as.data.frame(Habitent_condetion_data_min_reactiveClosing()),
                     w= as.data.frame(Habitent_condetion_data_mean_reactiveClosing()),
                     e=as.data.frame(Habitent_condetion_data_max_reactiveClosing()),
                     r=as.data.frame(Habitent_condetion_data_sd_reactiveClosing()),
                     s=metardata())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  output$Condition_Change_Stasticeis <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ConditionChangeStastics.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "ConditionChangeStastics.Rmd")
      file.copy("ConditionChangeStastics.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(m=habi_Change_Conditiondatasets(),
                     n= habi_Change_Condition(),
                     s=metardata())
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  output$Condition_Habitate_Finel_Result<- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "HabitatExtentConditionChange.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "HabitatExtentConditionChange.Rmd")
      file.copy("HabitatExtentConditionChange.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(o=habchange(),
                     m=Habitae_Change_Transetion_Matrix(),
                     n= habitate_Change(),
                     q=habitate_extent_Change(),
                     w=habitates_Change_Percentage_Plot(),
                     z=habitates_Change_Plot(),
                     e=habitates_dt_Percentage(),
                     p=habi_Change_Conditiondatasets(),
                     R=habi_Change_Condition(),
                     s=metardata(),
                     A=habitentes_DT()

      )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  # Habitate markers maps  Report 
  output$Markers_Reports_Condition_Integrated <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Markers_Condition_Integrated.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Markers_Condition_Integrated.Rmd")
      file.copy("Markers_Condition_Integrated.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list( n=Condetion_Habitent_Tranformaion_Markers_map(),d=metardata()
                      
      ) 
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  #  Step-1  Conditions 
  # Render Political Boundaries Attribute Header Selector
  output$Politecal_boundaries <- renderUI({
    req(map()) # Ensure that the map is uploaded
    selectInput("variable_mapAdminshp", "Attribute Heading", choices = names(map()))
  })
  # Render Attribute Name Selector
  output$Politecal_boundaries_Selection <- renderUI({
    req(Admin_boundaries_Data()) # Ensure that the data is extracted
    selectInput("variable_PoliticalBoundaries_Data", "Attribute Name", choices = names(Admin_boundaries_Data()), multiple = TRUE)
  })
  # Hot spot mapping 
  output$Habitent_Change_Hotspots<-renderUI({
    selectInput("Heat_habitent", "Select habitat  transformation hot spot maps and markers maps",choices=names(projection4_Hotspotmaps()))
  })
  # Condition opining habitats 
  output$Opening_stock_Condition<-renderUI({
    selectInput("Opening_stock", "",choices=names(Opening_Year()))
  })
  # Condition closing habitats 
  output$Closing_stock_Condition<-renderUI({
    selectInput("Closing_stock", "",choices=names(Closing_Year()))
  })
  
  
  output$Merain_Habitent_projection4_stat<-renderUI({
    selectInput("variable_3", "Habitat Change in Area Hectors and Percentage Change ",choices=names(Merain_habitens_stats_stat_table()))
  })
  
  output$Merain_Habitentes_Markers<-renderUI({
    selectInput("Merain_Habitentes_Markers_Transetion","Habitent Transformation Markers",  choices= names(Habitentdata()),multiple = FALSE)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

