

library(leaflet)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)


#setwd
#setwd("~/Downloads/Ideathon/TTTTTT")

# Carbon Mapper dataset ---------------------------------------------------

#1. plume csv lat/lon location data
df <- readxl::read_excel("data/Carbonmapper/carbonmapper_ch4_plumelist_2020_2021.xls",sheet=2)
df %<>% rename(lat = plume_lat, lng = plume_lon)
df$qplume <-  (df$qplume/max(df$qplume))*10

leaflet() %>% addTiles() %>% addCircleMarkers(data=df,radius= ~qplume)


leaflet() %>% addTiles() %>% addMarkerClusters(data=df,radius= ~qplume) 

#2. plume image data
ncpath <- "data/Carbonmapper/carbonmapper_ch4_rgb_geotiffs_2020_2021/ang20200708t192518-3_r1823_c108_ctr.tif"  
r <- rast(x=ncpath)
r <- rectify(r) %>% raster()
r[r==0] <- NA

leaflet() %>% addTiles() %>% addRasterImage(x=r)


#compute distance to plumes
df_sf <- df %>% st_as_sf(coords = c("lng","lat"),crs=4326)


pts <- readr::read_csv("data/my_home/my_home.csv") %>% st_as_sf(coords = c("lon","lat"),crs=4326)

nbuffer <- 20000

buffer <- st_buffer(pts, dist = nbuffer)
sel <- st_within(df_sf,buffer,sparse = F)
num <- length(sel[sel==TRUE])
sf_out <-  df_sf[sel,]

#gen label for leaks visualisation
if(num > 0){
  lab <- paste0("There are ",num," pipeline leaks within ", nbuffer/1000, " km of your location. <br><br>You can lobby your \nlocal politician <b>Ms. Shelly Maine</b> to fix this issue. She can be reached at <b>1-515-616-777</b> or at <a href='shelly.main@local.gov.org'>shelly.main@local.gov.org</a>")
}else{
  lab <- paste0("There are no documented pipeline leaks within ", nbuffer/1000, " km of your location <b>but this does not mean there are no methane leaks near you.</b> <br><br>Methane leak data are only available for parts of: ",
         paste(sort(c("California", "Arizona", "Colorado", "Utah", "New Mexico", "Texas", "Lousiana", "Pennsylvania", "West Virginia", "Ohio")),collapse=", "),
         ". <br><br>Change the map layer to see where documented leaks are across the US. <br><br>Lobby your state representative to collect data on leaks in your state. Find your state representate: <a href='https://www.house.gov/representatives/find-your-representative'>here</a> (external website)."
         )
}

leaflet() %>% addTiles() %>% addCircleMarkers(data=sf_out,radius= ~qplume) %>% 
  addMarkers(data=pts,popup = lab) %>% addPolygons(data=buffer,opacity = .01,label="Click on the marker to find out what you can do!")

