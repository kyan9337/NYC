 
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
library(slickR)
library(devtools)
library(sunburstR)
library(TraMineR)
library(pipeR)
library(reshape)
install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(htmltools)
library(reshape)
library(d3r)


##########################
map_data <- read.csv("map_data.csv")
map_data_nyc <- map_data
dfi <- read.csv("all_borough_v2.csv")
nycounties <- geojsonio::geojson_read("Community Districts.geojson",
                                   what = "sp")
map_data <- geo_join(nycounties, map_data, "boro_cd", "borocd",how="inner")
pal <- colorNumeric("viridis", NULL)
borough <- c("Manhattan","Queens","Brooklyn","Bronx","Staten Island")
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
All_facility <- read.csv("All_facilities.csv") %>% filter(facdomain != "	Public Safety, Emergency Services, and Administration of Justice")
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

##### Shannon #####
#Shooting data 
shooting<-read.csv("NYPD_Shooting_Incident_Data__Year_To_Date_.csv")
shooting$OCCUR_DATE<-as.Date(shooting$OCCUR_DATE,format="%m/%d/%Y")
shooting<-shooting[,c(2,4,7,17,18)]
#Arrest data
arrest_data<-read_csv('arrest_data.csv')

##### Jianhao ##########
burdern<-function(a,b){
  q<-dfi%>%
    filter(borough == a | borough == b)%>%
    ggplot(aes(x = borough, y = pct_hh_rent_burd, fill = subborough))+
    geom_bar(stat="identity", position=position_dodge())
  ggplotly(q)
}

commute<- function(a,b){
  q<-dfi%>%
    filter(borough == a | borough == b)%>%
    ggplot(aes(x = borough, y = mean_commute, fill = subborough))+
    geom_bar(stat="identity", position=position_dodge())
  ggplotly(q)
}

crime<-function(a,b){
  q<-dfi%>%
    group_by(subborough)%>%
    filter(borough == a | borough == b)%>%
    plot_ly(labels = ~subborough, values = ~crime_per_1000)%>%
    add_pie(hole = 0.6)%>%
    layout(title = "Crime rate",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  q
  
}

education<-function(a,b){
  q<-dfi%>%
    group_by(subborough)%>%
    filter(borough == a | borough == b)%>%
    plot_ly(labels = ~subborough, values = ~pct_bach_deg)%>%
    add_pie(hole = 0.6)%>%
    layout(title = "Crime rate",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  q
}

poverty<-function(a,b){
  q<-dfi%>%
    filter(borough == a | borough == b)%>%
    ggplot(aes(x = borough, y = poverty_rate, fill = subborough))+
    geom_bar(stat="identity", position=position_dodge())
  ggplotly(q)
}


sun_1<-dfi[,c(42,43,44,45,198,199)]
sun_1$def<-rep("facility", length(sun_1$count_public_schools))
sun_2<-dfi[,c(33,198,199)]
sun_2$def<-rep("lots",length(sun_2$lots_public_facility_institution))

mydata1<-melt(sun_1,id.vars=c("borough","subborough","def"),
              variable.name="Year",value.name="Sale")
mydata2<-melt(sun_2,id.vars=c("borough","subborough","def"),
              variable.name="Year",value.name="Sale")
mydata<-rbind(mydata1,mydata2)

dat <- data.frame(
  level1 = mydata$borough,
  level2 = mydata$subborough,
  level3 = mydata$def,
  level4 = mydata$variable,
  size = mydata$value,
  stringsAsFactors = FALSE
)

tree <- d3_nest(dat, value_cols = "size")
count <- All_facility%>%
  group_by(borocd,facdomain)%>%
  summarise(n = n())
colnames(count) <- c("borocd","community_name", "count")
################################
logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "NYC Profile"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = "MSSP"
  ,badgeTextColor = "rgb(105,105,105)"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)

theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(
  # skin = "purple",
  dashboardHeader(
    ### changing logo
    title = logo_blue_gradient
    
    #title = "NYC Profile "
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
                              icon = icon("bar-chart-o")
                              
                     ),
                     menuItem("flood risk",
                              tabName = "wifi",
                              icon = icon("bar-chart-o")
                              
                     ),
                     menuItem("Safety",
                              tabName = "safety",
                              icon = icon("bar-chart-o")
                              
                     ),
                    
                     
                     menuItem("Facilities",
                              tabName = "facility",
                              icon = icon("bar-chart-o")
                              
                     )
                     
            ),
            menuItem("Education", tabName = "education", icon = icon("question-circle")),
            menuItem("About", tabName = "about", icon = icon("question-circle"))
        )
    ),
    
    dashboardBody(
      theme_blue_gradient,
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
                  mainPanel(
                    slickROutput("slickr", width="150%"))


                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                fluidRow((
                    column(width = 6,
                           box(width=12,solidHeader = FALSE, background= "olive",
                               title="Introduction",
                               h4("Welcome to our Shiny App!"
                                  ,size = 10,style = "font-family: 'Arial'," ),
                               h4("Location!",size = 10,style = "font-family: 'Arial'," ),
                               h4("Location!",size = 10,style = "font-family: 'Arial'," ),
                               h4("Location! Location is what we care about!
Before you go to New York, please check our Shiny to truly get to know New York!",size = 10,style = "font-family: 'Arial'," )
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
              tabName = "Vis", 
              fluidRow(
                box(h4("This sunburst plot indicates the land use condition in New York City. 
                       The innermost circle is the five main boroughs (Manhattan, Bronx, Staten Island, Queens, Brooklyn).
                       The second inner circle is the 59 community districts. The third inner circle represents the occupied 
                       area of each land use type, including parking lots, facilities (hospitals and clinics, 
                       schools and institutions, libraries, parks). For example, 
                       when you clic in Greenwich Village/Financial District in Manhattan, you will see that 77.6% of the land area
                       is occupied as parking lots and 22.4% is facilities. Among the facilities’ land, 49% is public schools’ land, 
                       28% is parks’ land, 19% is health facilities’ land and 4% is libraries’ land.",size = 10,style = "font-family: 'Arial',")),
                box(title= "Land use sunburst plot", status = "warning", width= 12, solidHeader = TRUE, sund2bOutput("sunburstPlot", height = "750", width = "100%"))
              )
            ),
            
            
            tabItem(
                tabName = "facility",
                 fluidRow(
                   column(width = 3,
                          box( title = "Left Map Control", width = NULL, status = "primary",
                               selectInput("boro1","Select community to view on left side panel:",cmd,selected = cmd[1])),
                          box(title = "Right Map Control", width = NULL, status = "primary",
                              selectInput("boro2","Select community to view right side panel:" ,cmd,selected = cmd[3])),
                          box(title = "Controls", width = NULL, status = "primary",
                              checkboxGroupInput("boro11","Select facility type to view:",choices = factype ,selected = factype)
                          )
                          )     
                    ,
                  column(width = 9,
                           box( 
                                
                                leafletOutput("facility_1",height = 500),
                                br(),
                                br(),
                                tableOutput("table1")
                                )     
                                      
                           
                           ,box(
                                leafletOutput("facility_2",height = 500),
                                br(),
                                br(),
                                tableOutput("table2")
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
              tabName = "Difference",
              fluidRow(
                box(title = "Controls", status = "warning", solidHeader = TRUE,
                    selectInput("borough1","Select borough 1:",borough,selected = borough[1]),
                    br(),
                    br(),
                    br(),
                    selectInput("borough2","Select borough 2:" ,borough,selected = borough[3])
                ),
                box(title = "Rent Burden", solidHeader = TRUE,
                    plotlyOutput("rentburden"))
              )
            ),
            
            tabItem(
              tabName = "Security",
              fluidRow(
                column(width = 6,
                       box(title = "Shooting points", solidHeader = TRUE,
                           leafletOutput("shooting_map",height = 500))),
                column(width = 6,
                       box(title="Arresting points", solidHeader = TRUE,
                           leafletOutput("arrest_map",height = 500)))
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
                h4("abcd"),
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
    
    output$rentburden <- renderPlotly({
      
      burdern(input$borough1,input$borough2)
    })
    
    output$table1 <- renderTable({
      count %>%
        filter(borocd ==input$boro1) %>%
        select(community_name, count) %>%
        arrange(desc(count))
    })
    
    output$table2 <- renderTable({
      count %>%
        filter(borocd ==input$boro2) %>%
        select(community_name, count) %>%
        arrange(desc(count))
    })
    
    output$progressBox55 <- renderValueBox({
      fac55 <- filter(count, borocd ==input$boro2 & facdomain =="Libraries and Cultural Programs" )
      
      valueBox(
        
        paste0(fac55$n), "Libraries and Cultural Programs", icon = icon("archive"),
        color = "yellow"
      )
    })
    
    output$progressBox66 <- renderValueBox({  
      fac66 <- filter(count, borocd ==input$boro2 & facdomain =="Parks, Gardens, and Historical Sites" )
      valueBox(
        
        
        paste0(fac66$n), "Parks, Gardens, and Historical Sites", icon = icon("baseball-ball"),
        color = "maroon"
      )
    })
    
    output$facility_1 <- renderLeaflet({
      
      F1 <- filter(All_facility,borocd == input$boro1)
      F1 <- filter(F1,facdomain %in% input$boro11)
      cen1 <- filter(centers0_avg,region == input$boro1)
      leaflet(nyc_boroughs) %>%
        addProviderTiles("CartoDB.Positron")%>% 
        setView(cen1$x,cen1$y , zoom = 12.5) %>% 
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
        setView(cen2$x,cen2$y , zoom = 12.5) %>% 
        addPolygons(stroke=T,weight=1,fillOpacity = 0.5, smoothFactor = 0.5,color = "black",fillColor = ~pal0(borough),highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                                                              bringToFront = FALSE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircles(data = F2, radius=30 ,weight = 1, popup = ~as.character(F2$facname),fill=TRUE, opacity = 0,color = "black",fillColor = ~pal1(facdomain),fillOpacity = 1,stroke = TRUE) %>%
        addLegend(pal = pal1, values = F2$facdomain, opacity = 2, title = "Factors of Parks",position = "bottomright")
      
    })
    
    output$shooting_map <- renderLeaflet({
      
      leaflet(data = shooting) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(lat =~Latitude,lng=~Longitude,popup = ~as.character(BORO),
                   clusterOptions = markerClusterOptions()) %>% 
        addCircleMarkers(radius = 2.5, color="red", stroke = FALSE, fillOpacity = 0.5)
   
    })
    
    output$arrest_map <- renderLeaflet({
      
      leaflet(data = arrest_data) %>% addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(lat =~Latitude,lng=~Longitude,
                   popup = cat(paste0(
                     "Offense Description: ", ~OFNS_DESC,"\n",
                     "Perpetrator’s sex: ",~PERP_SEX,"\n",
                     "Perpetrator’s age: ",~AGE_GROUP,"\n",
                     "Perpetrator’s race:
                     ",~PERP_RACE)),
                   clusterOptions =
                     markerClusterOptions()) %>%
        addCircleMarkers(radius = 1.5, color="red", stroke = FALSE, fillOpacity = 0.5)
    })
    
    output$slickr <- renderSlickR({
      imgs <- list.files("F:/MSSP/MA676/NYC/NYC/www", pattern=".jpg", full.names = TRUE)
      slickR(imgs)
    })

    output$tableNYC <- DT::renderDataTable({
      
        DT::datatable(map_data_nyc, options = list(searching = TRUE,pageLength = 8,lengthMenu = c(8, 2, 4, 10), scrollX = T,scrollY = "300px"),rownames= FALSE
        )})
    
    output$sunburstPlot <-  renderSund2b({ 
      
      sb3 <- sund2b(tree, width="100%")
      sb3
      
    })
    
    }


# Run the application

shinyApp(ui = ui, server = server)