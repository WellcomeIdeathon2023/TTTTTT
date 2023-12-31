
library(leaflet)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(shiny)
library(tidyverse)

#pre processing plumes 

shp <- st_read("../data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
shp <- st_transform(shp,"+proj=longlat +datum=WGS84")
shp$gas <- rnorm(nrow(shp), mean=.38, sd=.1) #simulate prevalence of gas cook stoves in the US
shp %<>% mutate(gas_cat = case_when(gas <.3 ~1,
                                    gas >= .3 & gas <.6 ~2,
                                    gas >.6 ~3
)) %>% filter(NAME!="Hawaii" & NAME!="Alaska" )
shp$gas_cat <- as.factor(shp$gas_cat)
levels(shp$gas_cat) <- c("<30%","30-60%","60+%")


col <- colorQuantile("YlOrRd",cdc$gas)


#cdc data
cdc <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-4.txt")[-1] %>% na.omit() %>% 
  mutate(Crude.Rate = as.character(Crude.Rate)) %>% filter(Gender.Code != "")
cdc2 <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-5.txt")[-1] %>% na.omit() 
cdc3 <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-6.txt")[-1] %>% na.omit() 
cdc$group <- "gender"; cdc$val <- paste0("Gender - ",cdc$Gender)
cdc2$group <- "race"; cdc2$val <- paste0("Race - ",cdc2$Single.Race.6)
cdc3$group <- "children (1-14)"; cdc3$val <- paste0("Age - children (1-14)")
cdc <- bind_rows(cdc,cdc2,cdc3)

cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp %>% filter(NAME != "Hawaii"),cdc,by.x="GEOID",by.y="State.Code") %>% st_centroid()
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()
cdc <- cdc %>% mutate(long = unlist(map(cdc$geometry,1)), #get coords
                      lat = unlist(map(cdc$geometry,2)))
cdc$geometry <- NULL
### -------


# Define UI

ui <- navbarPage(
  "ME-thane Dashboard",
  tabPanel("Gas stoves",
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               leafletOutput("map",heigh="100%"),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         selectInput(inputId="filter",label="Select group:" ,choices = c("gender","race","children (1-14)"),selected ="race")
               )
                 #print("Cook stove use and respiratory disease Rates per 100k across US States (2018-2020)"),
             )
           )
)

# Define server
server <- function(input, output, session) {

cdc2 <- reactive({
  cdc <- cdc %>% filter(group==input$filter)
  cdc2 <- cdc %>% select(NAME,long,lat,val,Crude.Rate) %>% pivot_wider(names_from = val,  values_from = Crude.Rate) %>% select(-1)  
})
  

  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolygons(data=shp,fillColor= ~col(shp$gas),col="white",weight=1,label =  paste0(round(shp$gas*100,0),"% of households use gas stoves in ",shp$NAME)) %>%
      addLegend(pal = col, values = shp$gas, group = "circles", position = "bottomleft",title="Households with gas stove") %>%
      addMinicharts(lng=cdc2()$long,lat=cdc2()$lat,chartdata = cdc2() %>% select(-c(1:2)),type="pie",
                    height=10,maxValues=1,
                    width = 15 * (rowSums(cdc2() %>% select(-c(1:2)),na.rm = T)/(max(cdc2() %>% select(-c(1:2)),na.rm=TRUE))),
                    legendPosition = "bottomright"
                    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
