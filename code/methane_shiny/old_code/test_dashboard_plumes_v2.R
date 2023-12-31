
library(leaflet)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(shiny)
library(mapview)
library(htmlwidgets)

#pre processing plumes 

df <- readxl::read_excel("../data/Carbonmapper/carbonmapper_ch4_plumelist_2020_2021.xls",sheet=2)
df %<>% rename(lat = plume_lat, lng = plume_lon)
df$qplume <-  (df$qplume/max(df$qplume))*10

#compute distance to plumes
df_sf <- df %>% st_as_sf(coords = c("lng","lat"),crs=4326)

### ShinyDashboard -------



# UI  ---------------------------------------------------------------------


ui <- navbarPage(
  "ME-thane Dashboard",
  tabPanel("Methane leaks",
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               leafletOutput("map",height="100%"),
               textOutput("test"),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             sliderInput("nbuffer","Search for leaks. Set radius in km:", min = 1, max = 100, value = 12, step = 1),
                             numericInput("lat", "Lat", NA),
                             numericInput("lon", "Lon", NA)
               #print("Cook stove use and respiratory disease Rates per 100k across US States (2018-2020)"),
           )
          )
    )
  )





# Server ------------------------------------------------------------------


# Define server
server <- function(input, output, session) {
  
  
  # Read raster
  
  #if file uploaded, replace pts
  
  pt0 <- readr::read_csv("../data/my_home/my_home.csv") %>% st_as_sf(coords = c("lon","lat"),crs=4326)
  

  
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
    proxy %>% clearMarkers() # Clear existing markers
   proxy %>%  addTiles() %>% addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
      addMarkers(data=pts(),popup =  lab(),layerId = 10) %>% addPolygons(data=buffer(),opacity = .01)
    })
  
# Render map --------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addCircleMarkers(data=sf_out(),radius= sf_out()$qplume) %>% addScaleBar() %>% addMeasure(primaryLengthUnit ="meters") %>%
    addMarkers(data=pts(),popup =  lab(),layerId = 10) %>% addPolygons(data=buffer(),opacity = .01)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
