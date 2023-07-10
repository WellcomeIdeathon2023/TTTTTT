# How do we make sure these are minimum packages needed?
library(dplyr)
library(epiR)
library(ggplot2)
library(leaflet)
library(leaflet.minicharts)
library(magrittr)
library(raster)
library(sf)
library(scales)
library(shiny)
library(stringr)
library(shinyjs)
library(terra)
library(tidyverse)



# Set-up server -----------------------------------------------------------

# DATA PATHS! Explicitly put what data your component needs to run
# will make checking it is reproducible easier later
# data paths must be relative to shiny app
COPERNICUS_DATA <- "../../data/Processed/copernicus_long.Rdata"
METHANE_DATA <- "../../data/Carbonmapper/carbonmapper_ch4_plumelist_2020_2021.xls"
HOME_CSV <- "../../data/my_home/my_home.csv"
RESPIRATORY_DATA <- "../../data/CDC/respiratory.txt"
STRESS_DATA <- "../../data/Processed/traumacomplete_state.csv"
ANXIETY_DATA <- "../../data/Processed/anxietycomplete_state.csv"
STATES_SHAPEFILE <- "../../data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"
POPULATION_DATA <- "../../data/CDC/population_state_year.txt"
STOVE_SHAPEFILE <- "../../data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"
CDC_1 <- "../../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-4.txt"
CDC_2 <- "../../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-5.txt"
CDC_3 <- "../../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-6.txt"

# Copernicus set-up --------------------------------------------------------------

# load processed data
load(file = COPERNICUS_DATA) # create out_long df
# get state names
state_names <- sort(unique(out_long$NAME))
# get minimum and maximum dates

# Plume finder set-up --------------------------------------------------------------

df <- readxl::read_excel(METHANE_DATA,sheet=2)
df %<>% rename(lat = plume_lat, lng = plume_lon)
df$qplume <-  (df$qplume/max(df$qplume))*10

#compute distance to plumes
df_sf <- df %>% st_as_sf(coords = c("lng","lat"),crs=4326)

# default csv
pt0 <- readr::read_csv(HOME_CSV) %>% st_as_sf(coords = c("lon","lat"),crs=4326)


#generate simulated twitter data
lat <- c(35.409483, 35.32, 35.337660)
lon <- c(-119.032986, -118.9, -119.17580)
hashtag <- c("<b>@hmiller</b> at 2023-07-01 12:28:05: <br/>#methaneleak #Bakersfield", 
             "<b>@minnie123</b> at 2023-07-02 24:10:10: <br/>#stopleaks #gaskills #methaneleak", 
             "<b>@jpterson</b> at 2023-07-01 13:09:11: <br/>#methaneleak #StopFossilFuels")
text <- str_wrap(c("<i>Smells like gas is leaking from the <br/>old pipe behind the train tracks", 
                   "<i>My neighbor's having a leak on their <br/>property. We called the company <br/>but they <br/>haven't gotten back to us", 
                   "<i>How many more leaks does it take for <br/>people to understand gas is dangerous?"),10)

tweets <- bind_cols(lat,lon,hashtag,text)
names(tweets) <- c("lat","lon","hashtag","text")

# Stove set-up ------------------------------------------------------------

stove_shp <- st_read(STOVE_SHAPEFILE)
stove_shp <- st_transform(stove_shp,"+proj=longlat +datum=WGS84")
stove_shp$gas <- rnorm(nrow(stove_shp), mean=.38, sd=.1) #simulate prevalence of gas cook stoves in the US
stove_shp %<>% mutate(gas_cat = case_when(gas <.3 ~1,
                                    gas >= .3 & gas <.6 ~2,
                                    gas >.6 ~3
)) %>% filter(NAME!="Hawaii" & NAME!="Alaska" )
stove_shp$gas_cat <- as.factor(stove_shp$gas_cat)
levels(stove_shp$gas_cat) <- c("<30%","30-60%","60+%")


#cdc data
cdc <- read.delim(CDC_1)[-1] %>% na.omit() %>% 
  mutate(Crude.Rate = as.character(Crude.Rate)) %>% filter(Gender.Code != "")
cdc2 <- read.delim(CDC_2)[-1] %>% na.omit() 
cdc3 <- read.delim(CDC_3)[-1] %>% na.omit() 
cdc$group <- "gender"; cdc$val <- paste0("Gender - ",cdc$Gender)
cdc2$group <- "race"; cdc2$val <- paste0("Race - ",cdc2$Single.Race.6)
cdc3$group <- "children (1-14)"; cdc3$val <- paste0("Age - children (1-14)")
cdc <- bind_rows(cdc,cdc2,cdc3)

cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(stove_shp %>% filter(NAME != "Hawaii"),cdc,by.x="GEOID",by.y="State.Code") %>% st_centroid()
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()
cdc <- cdc %>% mutate(long = unlist(map(cdc$geometry,1)), #get coords
                      lat = unlist(map(cdc$geometry,2)))

cdc$geometry <- NULL

col <- colorQuantile("YlOrRd",cdc$gas)

# Respiratory set-up ------------------------------------------------------

respiratory_by_state <- read.delim(RESPIRATORY_DATA) # assumes you are in root of the project
respiratory_by_state <- respiratory_by_state[, 2:8]
respiratory_by_state$Year <- substr(respiratory_by_state$Month.Code, 1, 4)


population_state_year <- read.delim(POPULATION_DATA)
population_state_year <- population_state_year[,c("Population", "State", "Year")]
population_state_year <- na.omit(population_state_year)

respiratory_by_state <- merge(respiratory_by_state, population_state_year, by = c("State", "Year"))

respiratory_by_state$crude_rate <- respiratory_by_state$Deaths/respiratory_by_state$Population * 100000

CI <- apply(respiratory_by_state[, c("Deaths", "Population")], 1, function(x) {
  poisson.test(x[1], T = x[2] / 100000)$conf.int
})

respiratory_by_state$ci_lower <- CI[1,]
respiratory_by_state$ci_upper <- CI[2,]


names(respiratory_by_state) <- c("state", "year", "state_code", "month_verbal", "month",
                                 "death_cause", "icd_10", "deaths", "population",
                                 "rate", "ci_lower", "ci_upper")


respiratory_by_state$month <- paste(respiratory_by_state$month, "/28", sep = "")

respiratory_by_state$death_cause <- gsub("#", "", respiratory_by_state$death_cause)

# Remove specified rows
respiratory_by_state <- subset(respiratory_by_state, 
                               !(death_cause %in% c("Other acute lower respiratory infections (J20-J22,U04)", 
                                                    "Pneumoconioses and chemical effects (J60-J66,J68,U07.0)")))

respiratory_by_state$month <- as.Date(respiratory_by_state$month, format = "%Y/%m/%d")

# UI ----------------------------------------------------------------------

# Plume finder ui ---------------------------------------------------------


plume_ui <- div(
  class="outer",
  tags$head(
    # Include our custom CSS
    includeCSS("../styles.css"),
    includeScript("../gomap.js")
   ),
   leafletOutput("map",height="100%"),
   textOutput("test"),
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
                 width = 330, height = "auto",
                 sliderInput("nbuffer","Search for leaks. Set radius in km:", min = 1, max = 100, value = 12, step = 1),
                 numericInput("lat", "Lat", NA),
                 numericInput("lon", "Lon", NA)
   )
)

# Buttons ui -----------------------------------------------------------
#buttons_ui <- fluidRow( align = 'center',
#    column(3, align='center', actionButton("hide_methane_button", "Hide/show methane trends")),
#    column(3, align='center', actionButton("hide_map_button", "Hide/show map")),
#    column(3, align='center', actionButton("hide_health_button", "Hide/show health trends"))
#
#  )

# Copernicus + respiratory ui -----------------------------------------------------------
copernicus_ui <- fluidRow(

  #conditionalPanel(
    #condition = ("input.hide_methane_button%2 == 0"),
    sidebarLayout(
      sidebarPanel(
      helpText("Explore methane and mortality trends by cause, state and time."),
      
      # have input here for state name 
      selectInput("resp_cause", "Choose a cause:",
                  choices = unique(respiratory_by_state$death_cause)),
      
      selectInput("coper_state_input", 
                  label = "Choose a state:",
                  choices = state_names,
                  selected = state_names[1]),
      
      dateRangeInput("coper_date_range", 
                  label = "Choose a time range:",
                  start = min(out_long$date),
                  end = max(out_long$date))
    ),
      
      mainPanel(
        column(6, plotOutput("state_methane_trend")),
        column(6, plotOutput("deathPlot")),
      )
    )
  #)
)


# Mental health UI ----------------------------------------------------------


mh_ui <- fluidRow(
  #conditionalPanel(
    #condition = ("input.hide_map_button%2 == 0"),
    sidebarLayout(
      sidebarPanel(
        helpText("See stress and anxiety trends across the country."),

        selectInput("stress_anxiety", "Stress or anxiety?",
                    choices = c("Stress", "Anxiety")),

        selectInput("mh_years", "Select a time period:",
                    choices = c("3-year trend", "5-year trend")),
      ),

      mainPanel(
        leafletOutput("mh_map")
      )
    )
  #)
)



# Stove UI ----------------------------------------------------------------

stove_ui <- div(
  class="outer",
    
  tags$head(
    # Include our custom CSS
    includeCSS("../styles.css"),
    includeScript("../gomap.js")
  ),
  
  leafletOutput("stove_map",height="100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                selectInput(inputId="filter",label="Select group:" ,choices = c("gender","race","children (1-14)"),selected ="race"),
                plotOutput("stove_plot2",height=300*0.8 ,width = 300 ),
                plotOutput("stove_plot3",height=200 ,width = 300 )
  )

)

# Main ui -----------------------------------------------------------------
ui <- navbarPage(
  "ME-thane Dashboard",
  useShinyjs(), # allows show/hide
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), # allows custom styles
  tabPanel("Overview", 
    column(1),
    column(
     10,
     h1("Methane emissions and health outcomes"),
     p("This dashboard combines data from a number of sources (see Data) to empower
       people to examine how methane relates to their health."),
     h3("Leaks near you"),
     p("In this panel, you can explore reported methane leaks in your area. 
        Lobby your local government and see what local people are saying about pollution in your area."),
     h3("Healthier homes"),
     p("This panel shows the distribution of households using different types of stoves.
      See how this relates to respiratory outcomes in your area and how this might differentially affect different groups in society."),
     h3("Healthcare needs planner"),
     p("In this panel, see methane emissions and health data side-by-side. 
        See trends across time to determine how this may affect burden on the healthcare system in your area and surrounding areas."),
     h3("Data"),

     p("We bring together data from:"),
     a("The Copernicus Climate data store,", href="https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-methane?tab=overview"), br(),
     a("The Centers for Disease Control and Prevention (CDC) WONDER,", href="https://wonder.cdc.gov/ucd-icd10-expanded.html"), br(),
     a("The Substance Abuse and Mental Health Services Adminstration (SAMHSA) data store", href="https://www.samhsa.gov/data/data-we-collect/mh-cld-mental-health-client-level-data"), br(),
     p("There are many limitations to these data-sets. They have very different spatial and temporal coverage, as well as granularities. Both data from the CDC and SAMHSA focus on the United States of America."),
     p("However, despite these differences, they also share similar structures, capturing information along spatial and temporal axes.The purpose of this platform is to explore how we can begin to bring together these different sources of data to facilitate research into the relationship between methane and health."),
    ),
    column(1),
  ),
  tabPanel("Leaks near you", plume_ui),
  tabPanel("Healthier homes", stove_ui),
  tabPanel(
    "Healthcare needs planner", 
    fluidRow(
      column(1),
      column(
        10,
        h1("Explore methane and health data-sets"),

        #h3("Choose which data views to show:"),
        #buttons_ui,
        hr(),
        copernicus_ui, 
        mh_ui,
      ),
      column(1),
    )
  ),
)


# Define server logic ----
server <- function(input, output) {

  # Plume functionality -------------------
  
  # Reactive variables
  
  #if file uploaded, replace pts
  pts <- reactive({
    if(!is.na(input$lon)){
      pts <- matrix(data=c(input$lon,input$lat),1,2) %>% as.data.frame()
      names(pts) <- c("lon","lat")
      pts %<>% st_as_sf(coords = c("lon","lat"),crs=4326)
    }else{
      pts <- pt0
    }
  })
  
  nbuffer <- reactive({input$nbuffer*1000}) #covert to meter (neded for st_buffer)
  buffer <- reactive({st_buffer(pts(), dist = nbuffer())})
  sel <- reactive({st_within(df_sf,buffer(),sparse = F)})
  num <- reactive({length(sel()[sel()==TRUE])})
  sf_out <-  reactive({df_sf[sel(),]})
  
  lab <- reactive({
    if(num() > 0){
      lab <-paste0("There are ",num()," pipeline leaks within ", nbuffer()/1000, " km of your location. <br><br>You can lobby your \nlocal politician <b>Ms. Shelly Maine</b> to fix this issue. She can be reached at <b>1-515-616-777</b> or at <a href='shelly.main@local.gov.org'>shelly.main@local.gov.org</a>")
    }else{
      lab <- paste0("There are no documented pipeline leaks within ", nbuffer()/1000, " km of your location <b>but this does not mean there are no methane leaks near you.</b> <br><br>Methane leak data are only available for parts of: ",
                    paste(sort(c("California", "Arizona", "Colorado", "Utah", "New Mexico", "Texas", "Lousiana", "Pennsylvania", "West Virginia", "Ohio")),collapse=", "),
                    ". <br><br>Change the map layer to see where documented leaks are across the US. <br><br>Lobby your state representative to collect data on leaks in your state. Find your state representate: <a href='https://www.house.gov/representatives/find-your-representative'>here</a> (external website)."
      )
    }
  })
  
  
  # Get map from hover ------------------------------------------------------
  
  
  observeEvent(input$lon>0, {
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearGroup(group="Markers") # Clear existing markers
    proxy %>% setView(lat=unlist (map (pts()$geometry,2)),lng=unlist (map (pts()$geometry,1)),zoom = 9) %>%
      addTiles() %>% addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
      addMarkers(data=pts(),popup =  lab(),layerId = 10) %>% addPolygons(data=buffer(),opacity = .01)
  })
  
  # Render map --------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% hideGroup ("Markers") %>%
      addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
      addMarkers(data=pts(),popup =  lab(),layerId = 10) %>% addPolygons(data=buffer(),opacity = .01) %>%
      addPopups(data=tweets,lat=~lat,lng=~lon, 
      paste0(tweets$hashtag, "<br/><br/>", tweets$text) ,
      options = popupOptions(closeButton = T), group = "Markers") %>% 
      addLayersControl(overlayGroups = "Markers")
  })
  
  
  # Copernicus functionality ---------------
  # Reactive variables
  coperInput <- reactive({
    state_data <- out_long[out_long$NAME==input$coper_state_input,] # select rows relating to the state
    time_data <- state_data[ (state_data$date >= input$coper_date_range[1] ) & (state_data$date <= input$coper_date_range[2] ),  ] # select rows within the time range
    return(time_data)
  })
  
  # Update UI
  output$state_methane_trend <- renderPlot(
    ggplot( coperInput() ) + geom_line(aes(x=date,y=val)) +
      xlim(input$coper_date_range[1],input$coper_date_range[2]) + 
      labs(x = "Date", y = "Methane concentration", 
           title = paste("Average methane concentration") 
      ) +
      theme_minimal()
  )
  
  # Respiratory functionality --------------------- 
  # Reactive variables
  respInput <- reactive({
    filtered_data <- subset(
      respiratory_by_state, 
      (state == input$coper_state_input) & (death_cause == input$resp_cause) & (month >= input$coper_date_range[1] ) & (month <= input$coper_date_range[2] )
      )
    return(filtered_data)
  })
  
  output$deathPlot <- renderPlot({ 
    ggplot(respInput(), aes(x = month, y = rate)) +
      geom_line() +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
      xlim(input$coper_date_range[1],input$coper_date_range[2]) +
      labs(x = "Date", y = "Death Rate", 
           title = paste("Death rate (95% C.I.) from\n", input$resp_cause),
           fill = "95% Confidence Interval") +
      theme_minimal()
  })

  # Mental health functionality --------------------- 

  shp <- st_read(STATES_SHAPEFILE)
  shp <- st_transform(shp,"+proj=longlat +datum=WGS84")

  # Reactive variables
  mh_trendtime <- reactive({
    if(input$mh_years == "3-year trend"){
      mh_trendtime <- 3
    }else{
      mh_trendtime <- 5
    }
    return(mh_trendtime)
  })

  mh_processed <- reactive({
    if(input$stress_anxiety == "Stress"){
      mh_data <- read.csv(STRESS_DATA)
    }else{
      mh_data <- read.csv(ANXIETY_DATA)
    }
    mh_data$GEOID <- str_pad(mh_data$GEOID, 2, pad = "0")
    shp_merged <- merge(shp, mh_data, by.x="GEOID", by.y="GEOID", all=TRUE)
    shp_merged <- shp_merged[order(shp_merged$GEOID),]

    shp_merged <- shp_merged[-c(53), ]
    col_name_2020<-colnames(mh_data)[grepl(toString(2020),colnames(mh_data))][1]
    col_name_past<-colnames(mh_data)[grepl(toString(2021-mh_trendtime()[1]),colnames(mh_data))][1]

    shp_merged$normalised_change <- (shp_merged[[col_name_2020]] - shp_merged[[col_name_past]]) / shp_merged[[col_name_past]] * 100

    return(shp_merged)
  })

  normalised_change <- reactive({mh_processed()$normalised_change})

  colscale <- reactive({colorNumeric('PRGn', normalised_change(), reverse = TRUE)})

  output$mh_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolygons(data=mh_processed(),fillColor= ~colscale()(normalised_change()),col="white",weight=1) %>%
    addLegend(pal =colscale(), values =normalised_change(), group = "circles", position = "bottomleft",title= paste("Percentage change in ", input$stress_anxiety, " diagnoses", sep=''), labFormat = labelFormat(suffix = "%"), na.label = "Insufficient data")
  })
  

# Stove functionality -----------------------------------------------------

  cdc2 <- reactive({
    cdc <- cdc %>% filter(group==input$filter)
    cdc2 <- cdc %>% dplyr::select(NAME,long,lat,val,Crude.Rate) %>% pivot_wider(names_from = val,  values_from = Crude.Rate) %>% dplyr::select(-1)  
  })
  
  
  content <- paste(sep = "<br/>",
                   "<b>Is there a relationship between cooking with gas and respiratory diseases (such as asthma)?</b></b></b>",
                   "Some evidence suggests that gas stoves emit toxic chemicals that can be inhaled and increase disease risk, especically for vulnerable populations such as children.</b></b>",
                   "You can explore if there is a relationship between the proportion of households using gas stoves and the rate of respiratory disases (2018-2021) at the US-state level."
  )
  
  
  # Render Leaflet map
  output$stove_map <- renderLeaflet({
    leaflet() %>% addTiles() %>% 
      setView(-88,47,  zoom = 4.4) %>%
      addPolygons(data=stove_shp,fillColor= ~col(stove_shp$gas),col="white",weight=1,label =  paste0(round(stove_shp$gas*100,0),"% of households use gas stoves in ",stove_shp$NAME)) %>%
      addLegend(pal = col, values = stove_shp$gas, group = "circles", position = "bottomleft",title="Households with gas stove") %>%
      addPopups(-104,50.5,content,options=popupOptions(closeButton = T)) %>%
      addMinicharts(lng=cdc2()$long,lat=cdc2()$lat,chartdata = cdc2() %>% dplyr::select(-c(1:2)),type="pie",
                    height=10,maxValues=1,
                    width = 15 * (rowSums(cdc2() %>% dplyr::select(-c(1:2)),na.rm = T)/(max(cdc2() %>% dplyr::select(-c(1:2)),na.rm=TRUE))),
                    legendPosition = "bottomright"
      )
  })
  
  # Render plots
  pdf <- reactive({ cdc %>% filter(group ==input$filter)})
  
  #plot 2
  output$stove_plot2 <- renderPlot({
    p <- ggplot(data=pdf(),
                aes(y=Crude.Rate,x=gas,group=val,col=val)) + 
      geom_point() + geom_smooth(method="lm",se =F) + 
      xlab("Prop. of households with gas \nstove per US State") + ylab("Rate per 100k")+
      scale_colour_discrete(type=d3.schemeCategory10)+
      ggtitle("Rate and gas stove use",
              subtitle = paste0("By ",input$filter, ", relationship at US-state level"))+
      theme_linedraw()
    p + theme(text=element_text(size=15),legend.position = "none")
  })
  
  #plot 3
  output$stove_plot3 <- renderPlot({
    p3 <- ggplot(data=pdf(),
                 aes(y=Crude.Rate,x=val,fill=val)) + 
      geom_col(position = position_dodge(width = 0.9)) +theme_linedraw()+
      xlab("Group") + ylab("Disease rate per 100k")+
      scale_fill_discrete(type=d3.schemeCategory10)+
      scale_x_discrete(labels = label_wrap(8))+ #wrap labels
      ggtitle(paste0("Rate per state by ",input$filter))+
      theme_linedraw()
    p3 + theme(text=element_text(size=15),legend.position = "none")
    
    
    
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)

