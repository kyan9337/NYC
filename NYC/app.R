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
                             
                                  radioButtons("Variable", "Compare Variable", 
                                               c("A",
                                                 "B",
                                                 "C"),
                                               select = "B")
                              
                     ),
                     menuItem("????????",
                              tabName = "asdfasdf",
                              icon = icon("bar-chart-o"),
                              
                              radioButtons("Variable", "Compare Variable", 
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
                tabName = "welcome",
                fluidRow(
                    mainPanel(imageOutput("header",width = "auto"))
                )
                
            )
            ,
            tabItem(
                tabName = "map_overall",
                
                fluidRow(
                    
                    column(width = 12,
                           box(title = "NYC Overview",width = NULL, solidHeader = TRUE,
                               leafletOutput("NYC_MAP",height = 500)
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
                tabName = "HRD",
                fluidRow(
                    tabBox(
                        tabPanel("Individual Heart Rate Drop" , plotlyOutput("plot1"), width = 6, height = 550)
                        
                        ,tabPanel("Team Stats" , DT::dataTableOutput("table111"), width = 6, height = 550)
                    )
                    ,
                    box(
                        title = "Controls", status = "primary", solidHeader = TRUE,
                        
                        dateRangeInput("dateRange333",
                                       label = "Select Date",
                                       start = "2018-09-05", end = Sys.Date())
                        ,
                        selectInput("Athlete3", "Select Athlete:", choices = c("A","B"), selected = "A")
                        
                    ))
                
            ),
            tabItem(
                tabName = "Test30",
                fluidRow(
                    box(title = "Team Stats", status = "warning", collapsible = TRUE, 
                        solidHeader = TRUE, DT::dataTableOutput("table30"), width = 6, height = 600),
                    
                    box(title = "Individual Split Time", status = "primary", collapsible = TRUE,
                        solidHeader = TRUE, plotlyOutput("plot30"), width = 6, height = 600)
                    ,
                    box( 
                        title = "Select Date", status = "warning", solidHeader = TRUE,
                        selectInput("selectdat30", "30min Test Date", choices = c("A","B"))
                    ),
                    box(
                        title = "Select Athlete", status = "primary", solidHeader = TRUE,
                        
                        selectInput("i30", "30min Test: Athletes' List", choices = c("A","B"))
                    )
                )
            ),
            tabItem(
                tabName = "Test2k",
                fluidRow(
                    box(title = "Team Stats", status = "warning", collapsible = TRUE, 
                        solidHeader = TRUE, DT::dataTableOutput("table2k"), width = 6, height = 600),
                    
                    box(title = "Individual Split Time", status = "primary", collapsible = TRUE,
                        solidHeader = TRUE, plotlyOutput("plot2k"), width = 6, height = 600)
                    ,
                    box( 
                        title = "Select Date", status = "warning", solidHeader = TRUE,
                        selectInput("selectdat2k", "2k Test Date", choices = c("A","B"))
                    ),
                    box(
                        title = "Select Athlete", status = "primary", solidHeader = TRUE,
                        
                        selectInput("i2k", "2k Test: Athletes' List", choices = c("A","B"))
                    )
                )
            ),
            tabItem(
                tabName = "Test5k",
                fluidRow(
                    box(title = "Team Stats", status = "warning", collapsible = TRUE, 
                        solidHeader = TRUE, DT::dataTableOutput("table5k"), width = 6, height = 600),
                    
                    box(title = "Individual Split Time", status = "primary", collapsible = TRUE,
                        solidHeader = TRUE, plotlyOutput("plot5k"), width = 6, height = 600)
                    ,
                    box( 
                        title = "Select Date", status = "warning", solidHeader = TRUE,
                        selectInput("selectdat5k", "5k Test Date", choices = c("A","B"))
                    ),
                    box(
                        title = "Select Athlete", status = "primary", solidHeader = TRUE,
                        
                        selectInput("i5k", "5k Test: Athletes' List", choices = c("A","B"))
                    )
                )
            ),
            tabItem(
                tabName = "PP",
                fluidRow(
                    box(title = "PP Test Stats", status = "warning", collapsible = TRUE, 
                        solidHeader = TRUE, DT::dataTableOutput("tablepp1"), width = 6, height = 600),
                    
                    
                    
                    box( 
                        title = "Select Date", status = "primary", solidHeader = TRUE,
                        selectInput("selectdatpp","PP Test Date", choices = c("A","B") )
                    )
                    
                )
            ),
            tabItem(
                tabName = "one",
                fluidRow(
                    
                    
                    box(title = "1 Minute Test Stats", status = "primary", collapsible = TRUE,
                        solidHeader = TRUE, DT::dataTableOutput("tablepp2"), width = 6, height = 600)
                    ,
                    
                    box(
                        title = "Select Date", status = "primary", solidHeader = TRUE,
                        selectInput("selectdat11","1min Test Date", choices = c("A","B") )
                    )
                )
            ),
            tabItem(
                tabName = "TP",
                fluidRow(
                    tabBox(
                        tabPanel("Team Stats", DT::dataTableOutput('tabletp'), width = 6, height = 550),
                        tabPanel("Individual Performance", plotlyOutput("plottppt"),width =6, height = 550)
                    )
                    ,
                    box( 
                        title = "Select Date", status = "primary", solidHeader = TRUE,
                        selectInput("selectdattp","  Training Pace Test Date", choices = c("A","B") )
                    ),
                    box(
                        title = "Select Athlete", status = "primary", solidHeader = TRUE,
                        selectInput("itppt", "Training Paces: Athletes' List", choice = c("A","B"))
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
        
       
        m <- leaflet(map_data) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.9,
                        fillColor = ~pal(log10(acres)),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        label = ~paste0(borocd, ": ", formatC(acres, big.mark = ","))) %>%
            addLegend(pal = pal, values = ~log10(acres), opacity = 1.0,
                      labFormat = labelFormat(transform = function(x) round(10^x)))
        m
    })
    
    output$header<- renderImage({
        Leg<-"www/NYC_header.jpg"
        list(src=Leg)
    },deleteFile = FALSE)  
}

# Run the application
shinyApp(ui = ui, server = server)