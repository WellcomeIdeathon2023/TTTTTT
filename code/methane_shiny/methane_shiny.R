# How do we make sure these are minimum packages needed?
library(dplyr)
library(epiR)
library(ggplot2)
library(leaflet)
library(magrittr)
library(raster)
library(sf)
library(shiny)
library(terra)


# Set-up server -----------------------------------------------------------

# DATA PATHS! Explicitly put what data your component needs to run
# will make checking it is reproducible easier later
# data paths must be relative to shiny app
COPERNICUS_DATA <- "../../data/Processed/copernicus_long.Rdata"
METHANE_DATA <- "../../data/Carbonmapper/carbonmapper_ch4_plumelist_2020_2021.xls"
HOME_CSV <- "../../data/my_home/my_home.csv"
RESPIRATORY_DATA <- "../../data/CDC/respiratory.txt"
POPULATION_DATA <- "../../data/CDC/population_state_year.txt"

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

# set up graphing variables

resp_breaks <- as.Date(c("2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31",
                         "2019-03-31", "2019-06-30", "2019-09-30", "2019-12-31", 
                         "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
                         "2021-03-31", "2021-06-30", "2021-09-30", "2021-12-31"), 
                       format = "%Y-%m-%d")

resp_labels <- c("3/18", "6/18", "9/18", "12/18",
                 "3/19", "6/19", "9/19", "12/19",
                 "3/20", "6/20", "9/20", "12/20",
                 "3/21", "6/21", "9/21", "12/21")


# UI ----------------------------------------------------------------------

# Plume finder ui ---------------------------------------------------------


plume_ui <- fluidPage(
  titlePanel("Reported methane leaks by location"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("nbuffer","Search for leaks. Set radius in km:", min = 1, max = 100, value = 12, step = 1),
      fileInput("upload", NULL, buttonLabel = "Upload CSV", multiple = FALSE,accept=".csv")
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Copernicus ui -----------------------------------------------------------
copernicus_ui <- fluidRow(
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore methane trends by state using data from copernicus."),
      
      # have input here for state name 
      selectInput("coper_state_input", 
                  label = "Choose a state to display trends for.",
                  choices = state_names,
                  selected = state_names[1]),
      
      dateRangeInput("coper_date_range", 
                  label = "Time range of interest:",
                  start = min(out_long$date),
                  end = max(out_long$date))
    ),
    
    mainPanel(
      plotOutput("state_methane_trend")
    )
  )
)


# Respiratory ui ----------------------------------------------------------


respiratory_ui <- fluidRow(
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore death rate by cause and state over time."),

      selectInput("resp_state", "Choose a state:",
                  choices = unique(respiratory_by_state$state)),

      selectInput("resp_cause", "Choose a cause:",
                  choices = unique(respiratory_by_state$death_cause)),

    ),

    mainPanel(
      plotOutput("deathPlot")
    )
  )
)


# Main ui -----------------------------------------------------------------
ui <- navbarPage(
  "ME-thane Dashboard",
  tabPanel("Methane leaks", plume_ui),
  tabPanel(
    "Methane and respiratory trends", 
    fluidPage(
      copernicus_ui, 
      respiratory_ui
    )
  )
)




# Define server logic ----
server <- function(input, output) {

  # Plume functionality -------------------
  
  # Reactive variables
  #if file uploaded, replace pts
  pts <- reactive({
    if(!is.null(input$upload)){
      pts <- vroom::vroom(input$upload$datapath, delim = ",") %>% as.data.frame() %>%  st_as_sf(coords = c("lon","lat"),crs=4326)
    }else{
      pts <- pt0 # loaded in setup
    }
  })
  
  nbuffer <- reactive({input$nbuffer*1000}) #covert to meter (needed for st_buffer)
  buffer  <- reactive({st_buffer(pts(), dist = nbuffer())})
  sel     <- reactive({st_within(df_sf,buffer(),sparse = F)})
  num     <- reactive({length(sel()[sel()==TRUE])})
  sf_out  <- reactive({df_sf[sel(),]})
  
  lab <- reactive({
    if(num() > 0){
      lab <-paste0("There are ",num()," pipeline leaks within ", nbuffer()/1000, " km of your location. <br><br>You can lobby your \nlocal politician <b>Ms. Shelly Maine</b> to fix this issue. She can be reached at <b>1-515-616-777</b> or at <a href='shelly.main@local.gov.org'>shelly.main@local.gov.org</a>")
    }else{
      lab <- paste0(
        "There are no documented pipeline leaks within ", nbuffer()/1000, " km of your location <b>but this does not mean there are no methane leaks near you.</b> <br><br>Methane leak data are only available for parts of: ",
                    paste(sort(c("California", "Arizona", "Colorado", "Utah", "New Mexico", "Texas", "Lousiana", "Pennsylvania", "West Virginia", "Ohio")),collapse=", "),
                    ". <br><br>Change the map layer to see where documented leaks are across the US. <br><br>Lobby your state representative to collect data on leaks in your state. Find your state representate: <a href='https://www.house.gov/representatives/find-your-representative'>here</a> (external website)."
      )
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
      addMarkers(data=pts(),popup =  lab()) %>% addPolygons(data=buffer(),opacity = .01,label="Click on the marker to find out what you can do!")
    
  })
  
  
  # Copernicus functionality ---------------
  # Reactive variables
  stateInput <- reactive({
    state_data <-out_long[out_long$NAME==input$coper_state_input,] # select rows
    return(state_data)
  })
  
  # Update UI
  output$state_methane_trend <- renderPlot(
    ggplot( stateInput() ) + geom_line(aes(x=date,y=val)) + theme_linedraw() +
      ggtitle( paste("Average methane concentration in", input$coper_state_input, "(2003-2016)" ) )
  )
  
  # Respiratory functionality --------------------- 
  
  
  
  output$deathPlot <- renderPlot({ # TODO: change some of this to reactive variables
    
    filtered_data <- subset(respiratory_by_state, 
                            state == input$resp_state & death_cause == input$resp_cause)
    
    
    ggplot(filtered_data, aes(x = month, y = rate)) +
      geom_line() +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
      geom_smooth(method = "loess", se = TRUE, color = "red3", fill = "pink", alpha = 0.6) +
      scale_x_date(breaks = resp_breaks, labels = resp_labels, date_minor_breaks = "1 month") +
      labs(x = "Month", y = "Death Rate", 
           title = paste("Death rate from", input$resp_cause, 
                         "and 95% confidence interval over time in", input$resp_state),
           fill = "95% Confidence Interval") +
      theme_minimal()
  })

  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)