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

##########################
map_data <- read.csv("map_data.csv")
map_data_nyc <- map_data
nycounties <- geojsonio::geojson_read("Community Districts.geojson",
                                      what = "sp")
map_data <- geo_join(nycounties, map_data, "boro_cd", "borocd",how="inner")
pal <- colorNumeric("viridis", NULL)

chos <- c("Population"="pop_2010", "acres"="acres","Crime Rate"="crime_per_1000","Park Number"="count_parks",
            "Hospital Number"="count_hosp_clinic","Library Number"="count_libraries","Public School Number"="count_public_schools",
            "Building Density"="build_dens","Rent Burden"="pct_hh_rent_burd")

cmd <- c(  "Williamsburg, Greenpoint"="301"  ,          "Woodhaven, Richmond Hill"="409"    ,        "Queens Village, Rosedale"="413" ,          
           "Riverdale, Kingsbridge, Marble Hill"="208" ,"Bedford Stuyvesant"="303"           ,       "Bushwick"="304"  ,                         
           "East New York, Starrett City"="305"    ,    "Park Slope, Carroll Gardens"="306"   ,      "Brooklyn Heights, Fort Greene"="302"  ,    
           "Bedford Park, Norwood, Fordham"="207"  ,    "Highbridge, Concourse Village"="204"  ,     "University Hts., Fordham, Mt. Hope"="205" ,
           "East Tremont, Belmont"="206"           ,    "Bensonhurst, Bath Beach"="311"       ,      "Borough Park, Ocean Parkway"="312"        ,
           "Coney Island, Brighton Beach"="313"    ,    "Flatbush, Midwood"="314"              ,     "Sheepshead Bay, Gerritsen Beach"="315"    ,
           "Chelsea, Clinton"="104"               ,     "Wakefield, Williamsbridge"="212"       ,    "Brownsville, Ocean Hill"="316"            ,
           "East Flatbush, Rugby, Farragut"="317"   ,   "Canarsie, Flatlands"="318"              ,   "Astoria, Long Island City"="401"          ,
           "Sunnyside, Woodside"="402"             ,    "Jackson Heights, North Corona"="403"     ,  "Elmhurst, South Corona"="404"             ,
           "Flushing, Bay Terrace"="407"           ,    "Sunset Park, Windsor Terrace"="307"       , "Crown Heights North"="308"                ,
           "Melrose, Mott Haven, Port Morris"="201" ,   "Hunts Point, Longwood"="202"               ,"Fresh Meadows, Briarwood"="408"           ,
           "The Rockaways, Broad Channel"="414"    ,    "Ozone Park, Howard Beach"="410"    ,        "Bayside, Douglaston, Little Neck"="411"   ,
           "West Side, Upper West Side"="107"     ,     "Upper East Side"="108"              ,       "Central Harlem"="110"                     ,
           "East Harlem"="111"                   ,      "Washington Heights, Inwood"="112"    ,      "Stapleton, Port Richmond"="501"           ,
           "Stuyvesant Town, Turtle Bay"="106"     ,    "Jamaica, St. Albans, Hollis"="412"    ,     "Pelham Pkwy, Morris Park, Laconia"="211"  ,
           "New Springville, South Beach"="502"    ,    "Tottenville, Woodrow, Great Kills"="503",   "Midtown Business District"="105"          ,
           "Soundview, Parkchester"="209"         ,     "Battery Park City, Tribeca"="101"        ,  "Greenwich Village, Soho"="102"            ,
           "Manhattanville, Hamilton Heights"="109" ,   "Ridgewood, Glendale, Maspeth"="405"       , "Lower East Side, Chinatown"="103"         ,
           "Morrisania, Crotona Park East"="203"   ,    "Crown Heights South, Wingate"="309"        ,"Bay Ridge, Dyker Heights"="310"           ,
           "Throgs Nk., Co-op City, Pelham Bay"="210" , "Forest Hills, Rego Park"="406")
All_facility <- read.csv("All_facilities.csv")
factype <- as.character(unique(All_facility$facdomain))
factype <- factype[-8]
r <- GET("http://data.beta.nyc//dataset/472dda10-79b3-4bfb-9c75-e7bd5332ec0b/resource/d826bbc6-a376-4642-8d8b-3a700d701557/download/88472a1f6fd94fef97b8c06335db60f7nyccommunitydistricts.geojson")

nyc_boroughs <- readOGR(content(r, "text"), "OGRGeoJSON", verbose = F)
# nyc_boroughs_df <- tidy(nyc_boroughs)
nyc_boroughs@data$borough <- ""
manhattan_code <- c(101:112,164)
bronx_code <- c(201:212,226:228)
brooklyn_code <- c(301:318,355,356)
queens_code <- c(401:414,480:484)
staten_code <- c(501:503,595)
nyc_boroughs@data$borough <- ifelse(nyc_boroughs@data$communityDistrict %in% manhattan_code, "Manhattan",nyc_boroughs@data$borough)
nyc_boroughs@data$borough <- ifelse(nyc_boroughs@data$communityDistrict %in% bronx_code, "Bronx",nyc_boroughs@data$borough)
nyc_boroughs@data$borough <- ifelse(nyc_boroughs@data$communityDistrict %in% brooklyn_code, "Brooklyn",nyc_boroughs@data$borough)
nyc_boroughs@data$borough <- ifelse(nyc_boroughs@data$communityDistrict %in% queens_code, "Queens",nyc_boroughs@data$borough)
nyc_boroughs@data$borough <- ifelse(nyc_boroughs@data$communityDistrict %in% staten_code, "Staten Island",nyc_boroughs@data$borough)


# leaflet
pal0 <- colorFactor(palette = "Pastel1",
                    domain = nyc_boroughs@data$borough)
pal1<-colorFactor(palette = "Dark2", levels = unique(All_facility$facdomain))

centers0 <- data.frame(gCentroid(nyc_boroughs,byid=TRUE))
centers0$region <- as.character(nyc_boroughs@data$communityDistrict)
centers0_avg <- centers0 %>%
  group_by(region) %>%
  summarise(x=mean(x),y=mean(y))

################################
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
                     menuItem("Facilities",
                              tabName = "facility",
                              icon = icon("bar-chart-o")
                              
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
                tabName = "facility",
                fluidRow(
                    column(width = 12,
                           box(  status = "primary", solidHeader = FALSE,
                                leafletOutput("facility_1",height = 500))
                           
                           ,box(status = "primary", solidHeader = FALSE,
                                leafletOutput("facility_2",height = 500)
                                
                           ),
                           column(width = 12,
                                  
                                  box( 
                                      title = "Controls", status = "warning", solidHeader = TRUE,
                                      selectInput("boro1","Select community to view on left side panel:",cmd,selected = cmd[1]),
                                      selectInput("boro2","Select community to view right side panel:"
                                                  ,cmd,selected = cmd[3])
                                  ),
                                  box( 
                                    title = "Controls", status = "warning", solidHeader = TRUE,
                                    checkboxGroupInput("boro11","Select facility type to view:",choices = factype ,selected = factype)
                                  
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
                fluidRow(
                  column(width = 12,
                         box(  status = "primary", solidHeader = FALSE
                               ))),
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
    
    output$facility_1 <- renderLeaflet({
      
      F1 <- filter(All_facility,borocd == input$boro1)
      F1 <- filter(F1,facdomain %in% input$boro11)
      cen1 <- filter(centers0_avg,region == input$boro1)
      leaflet(nyc_boroughs) %>%
        addProviderTiles("CartoDB.Positron")%>% 
        setView(cen1$x,cen1$y , zoom = 14) %>% 
        addPolygons(stroke=TRUE,weight=1,fillOpacity = 0.5, smoothFactor = 0.5,color = "black",fillColor = ~pal0(borough),highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                                                              bringToFront = F)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircles(data = F1, radius=30 ,popup = ~as.character(F1$facname),weight = 1, fill=TRUE, opacity = 0,color = "black",fillColor = ~pal1(facdomain),fillOpacity = 1,stroke = TRUE) %>%
        addLegend(pal = pal1, values = F1$facdomain, opacity = 2, title = "Factors of Parks",position = "bottomright")
      
    })
    
    output$facility_2 <- renderLeaflet({
      
      F2 <- filter(All_facility,borocd == input$boro2)
      F2 <- filter(F2,facdomain %in% input$boro11)
      cen2 <- filter(centers0_avg,region == input$boro2)
      leaflet(nyc_boroughs) %>%
        addProviderTiles("CartoDB.Positron")%>% 
        setView(cen2$x,cen2$y , zoom = 14) %>% 
        addPolygons(stroke=T,weight=1,fillOpacity = 0.5, smoothFactor = 0.5,color = "black",fillColor = ~pal0(borough),highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                                                              bringToFront = FALSE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircles(data = F2, radius=30 ,weight = 1, popup = ~as.character(F2$facname),fill=TRUE, opacity = 0,color = "black",fillColor = ~pal1(facdomain),fillOpacity = 1,stroke = TRUE) %>%
        addLegend(pal = pal1, values = F2$facdomain, opacity = 2, title = "Factors of Parks",position = "bottomright")
      
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