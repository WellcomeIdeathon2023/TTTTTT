
library(leaflet)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(shiny)


#pre processing plumes 

df <- readxl::read_excel("../data/Carbonmapper/carbonmapper_ch4_plumelist_2020_2021.xls",sheet=2)
df %<>% rename(lat = plume_lat, lng = plume_lon)
df$qplume <-  (df$qplume/max(df$qplume))*10

#compute distance to plumes
df_sf <- df %>% st_as_sf(coords = c("lng","lat"),crs=4326)

### -------


# Define UI

ui <- navbarPage(
  "ME-thane Dashboard",
  tabPanel("Methane leaks",
  fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput("nbuffer","Search for leaks. Set radius in km:", min = 1, max = 100, value = 12, step = 1),
      fileInput("upload", NULL, buttonLabel = "Upload CSV", multiple = FALSE,accept=".csv")
    ),
    mainPanel(
      leafletOutput("map")#
        )
      )
  )
)
)

# Define server
server <- function(input, output, session) {
  # Read raster
  
  #if file uploaded, replace pts
  
  pt0 <- readr::read_csv("../data/my_home/my_home.csv") %>% st_as_sf(coords = c("lon","lat"),crs=4326)
  #pts <- reactive({ pt0})
  
  #if file uploaded, replace pts
  pts <- reactive({
    if(!is.null(input$upload)){
    pts <- vroom::vroom(input$upload$datapath, delim = ",") %>% as.data.frame() %>%  st_as_sf(coords = c("lon","lat"),crs=4326)
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
  
  
  #output$test <- renderTable(vroom::vroom(input$upload$datapath, delim = ",") %>% as.data.frame() %>%  
                             #st_as_sf(coords = c("lon","lat"),crs=4326))
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
    addMarkers(data=pts(),popup =  lab()) %>% addPolygons(data=buffer(),opacity = .01,label="Click on the marker to find out what you can do!")
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
