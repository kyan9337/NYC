library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(htmltools)
library(tidyverse)
library(plotly)
library(latticeExtra)
library(readxl)
library(stringr)
library(tidyr)
library(DT)
library(leaflet)
library(httr)
library(rgdal)
library(broom)
library(plotly)
library(readr)
library(rgeos)
library(leaflet)
library(ggmap)
library(varhandle)
library(miceadds)
library(tigris)
map_data <- read.csv("F:/MSSP/MA676/JSM-New/NYC/map_data.csv")

nycounties <- geojsonio::geojson_read("Community Districts.geojson",
                                      what = "sp")
# Or use the rgdal equivalent:
# nycounties <- rgdal::readOGR("json/nycounties.geojson", "OGRGeoJSON")

map_data <- geo_join(nycounties, map_data, "boro_cd", "borocd",how="inner")
pal <- colorNumeric("viridis", NULL)

chos <- c("Population"="pop_2010", "acres"="acres","Crime Rate"="crime_per_1000","Park Number"="count_parks",
            "Hospital Number"="count_hosp_clinic","Library Number"="count_libraries","Public School Number"="count_public_schools",
            "Building Density"="build_dens","Rent Burden"="pct_hh_rent_burd")

ui <- dashboardPage(
    
    skin = "purple",
    dashboardHeader(title = "NYC Profile "
                    ),
    dashboardSidebar(
        
        
        sidebarMenu(
            
            id = "sidebarmenu",
            menuItem("Welcome", tabName = "welcome", icon = icon("certificate",lib = "glyphicon")),
            menuItem("City Overview",
                     tabName = "Overview", icon = icon("venus"),
                     menuItem("Mapping View",
                              tabName = "map_overall",
                              icon = icon("bed")
                              
                     ),
                     menuItem("Data Visualization",
                              tabName = "Vis",
                              icon = icon("bar-chart-o")
                     ),
                     menuItem("Data",
                              tabName = "DT",
                              icon = icon("refresh")
                     )
            ),
            menuItem("Distric Detail",
                     tabName = "Compare", icon = icon("venus"),
                     menuItem("Simple comparsion",
                              tabName = "Difference",
                              icon = icon("bar-chart-o"),
                             
                                  radioButtons("ag", "Compare Variable", 
                                               c("A",
                                                 "B",
                                                 "C"),
                                               select = "B")
                              
                     ),
                     menuItem("????????",
                              tabName = "asdfasdf",
                              icon = icon("bar-chart-o"),
                              
                              radioButtons("adf", "Compare Variable", 
                                           c("A",
                                             "B",
                                             "C"),
                                           select = "B")
                              
                     )
                     
            ),
            
            menuItem("About", tabName = "about", icon = icon("question-circle"))
        )
    ),
    
    dashboardBody(
        tags$head(tags$style(HTML('
                              .same-row {
                              max-width: 200px;
                              display: table-cell;
                              padding-right: 10px;
                              }
                              '))),
        
        tags$style(HTML("
                    
                    
                    .box.box-solid.box-primary>.box-header {
                    
                    
                    background:#666666;
                    
                    }
                    .box.box-solid.box-primary{
                    
                    border-bottom-color:transparent;
                    border-left-color:transparent;
                    border-right-color:transparent;
                    border-top-color:transparent;
                    
                    }
                    ")),
        tags$style(HTML("
                    
                    
                    .box.box-solid.box-warning>.box-header {
                    color:#fff;
                    background:#FF9900
                    }
                    .box.box-solid.box-warning{
                    border-bottom-color:transparent;
                    border-left-color:transparent;
                    border-right-color:transparent;
                    border-top-color:transparent;
                    }")),
        tags$head(tags$style(
            type="text/css",
            "#header img {max-width: 180%; width:150%; height: 120%}"
        )),
        
        tags$head(tags$style(
            type="text/css",
            "#row2 img {max-width: 150%; width:auto; height: auto}"
        )),
        
        tags$head(tags$style(
            type="text/css",
            "#row3 img {max-width: 150%; width:140%; height: 100%}"
        )),
        
        
        
        
        
        tabItems(
            tabItem(
                tabName = "welcome"
                ,
                fluidRow(
                    mainPanel(imageOutput("header",width = "auto"))


                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                fluidRow((
                    column(width = 12,
                           box(width=11,solidHeader = TRUE, status = "primary",
                               title="Introduction",
                               h4("This app includes data from two rowing teams at Boston University:
                             Women Rowing Team and Light Weight Women Rowing Team.
                             The primary purpose of this app is to help coaches to visualize
                             athletes' performance and provide coaches more information for
                             decision making.",size = 10,style = "font-family: 'Arial'," )
                           ) )
                )
                )
                
            )
            ,
            tabItem(
                tabName = "map_overall",
                
                fluidRow(
                    
                    column(width = 12,
                           box(title = "NYC Overview",width = NULL, solidHeader = TRUE,
                               leafletOutput("NYC_MAP",height = 500)
                           ),
                           box(
                               title = "Controls", status = "primary", solidHeader = TRUE,
                               
                               
                               radioButtons("Variable", "Compare Variable",
                                           
                                            chos,
                                           
                                            selected = chos[1])
                               
                           )
                    )
                    
                )
            ),
            
            
            tabItem(
                tabName = "ZEW",
                fluidRow(
                    column(width = 12,
                           box( title = "Watts", status = "warning", solidHeader = TRUE,
                                plotlyOutput("chart3"))
                           
                           ,box(title = "Heart Rate",status = "primary", solidHeader = TRUE,
                                plotlyOutput("team")
                                
                           ),
                           column(width = 12,
                                  
                                  box( 
                                      title = "Controls", status = "warning", solidHeader = TRUE,
                                      dateRangeInput("dateRange233",
                                                     label = "Select Date",
                                                     start = 2018-09-05, end = Sys.Date()
                                      ),
                                      selectInput("athlete3","Select Athlete", 
                                                  choices =  c("A","B"))
                                  )
                           )
                           
                           
                    )
                )
            ),
            tabItem(
                tabName = "DT",
                fluidRow(
                    tabBox(
                         tabPanel("Cleaned Data" , DT::dataTableOutput("tableNYC"), width = 12, height = 550),width = 24,height ="500px"
                    )
                    
                    )
                
            ),
            tabItem(
                tabName = "about",
                imageOutput("row2"),
                br(),
                valueBoxOutput("userguide"),
                br(),
                br(),
                br(),
                br(),
                br(),
                h4("Contact information"),
                h4("Instructor: Eric Kolaczyk"),
                h4("This shiny app developed by Mark Yan, Samuel Luo, Si Chen, Sharry Zhang"),
                h4("Contact us at mssp@bu.edu")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  
    
    output$NYC_MAP <- renderLeaflet({
    
        A <- as.character(input$Variable)
        
        pal <- colorNumeric(c( "#ffddf4", "#d7837f","#893843"),map_data[[A]])
        leaflet(map_data) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.7,
                        fillColor = ~pal(map_data[[A]]),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        label = ~paste0(CD.Name,": ",
                                        formatC(map_data[[A]], big.mark = ","))) %>%
            addLegend(pal = pal,title =  input$Variable, values = ~map_data[[A]], opacity = 1.0)
            })
    
    output$header<- renderImage({
        Leg<-"www/NYC_header.jpg"
        list(src=Leg)
    },deleteFile = FALSE)  

    output$tableNYC <- DT::renderDataTable({
      
        DT::datatable(map_data_nyc, options = list(searching = TRUE,pageLength = 8,lengthMenu = c(8, 2, 4, 10), scrollX = T,scrollY = "300px"),rownames= FALSE
        )})
    
    
    }


# Run the application
shinyApp(ui = ui, server = server)