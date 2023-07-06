
library(shiny)
library(leaflet)
library(raster)

# Define UI
ui <- fluidPage(
  titlePanel("Raster Map Viewer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("layer", "Choose layer:", min = 1, max = 210, value = 1, step = 1)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Read raster
  r <- stack("../data/Copernicus/200301_202006-C3S-L3_GHG-GHG_PRODUCTS-MERGED-MERGED-OBS4MIPS-MERGED-v4.3.nc",
              varname ="xch4")
  noms <- names(r)
  r <- projectRaster(nc, crs=4326)
  
  # Render Leaflet map
  output$map <- renderLeaflet({
      leaflet() %>% addTiles() %>%
      addRasterImage(x=r[[input$layer]])
  })
}

# Run the app
shinyApp(ui = ui, server = server)
