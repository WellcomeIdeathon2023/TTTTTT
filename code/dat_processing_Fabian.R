

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


pts <- readr::read_csv("../../my_home.csv") %>% st_as_sf(coords = c("lon","lat"),crs=4326)

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

# Copernicus dataset ------------------------------------------------------

#loading currently not working! 


library(ncdf4)

ncpath <- "~/Downloads/"
nc<-nc_open(ncname)
nc$var[[1]]


names(nc[['var']])
var1 <- ncvar_get(nc, "xch4", collapse_degen=FALSE)
var2 <- ncvar_get(nc, "lat_bnds", collapse_degen=FALSE)
var2 <- ncvar_get(nc, "lat_bnds", collapse_degen=FALSE)

dim_lon <- ncvar_get(nc, "lat_bnds")
dim_lat <- ncvar_get(nc, "lon_bnds")
dim_time <- ncvar_get(nc, "time_bnds")
coords <- as.matrix(expand.grid(dim_lon, dim_lat, dim_time))
nc_df <- data.frame(cbind(coords, var1))
names(nc_df) <- c("lon", "lat","time","var1")
r <- rasterFromXYZ(nc_df %>% filter(time==4748) )

ncname <- "~/Downloads/200301_202006-C3S-L3_GHG-GHG_PRODUCTS-MERGED-MERGED-OBS4MIPS-MERGED-v4.3.nc"  
r <- raster::stack(ncname,varname="xch4")
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
#T_array <- ncvar_get(r,r$var[[7]])
#T <- "xch4"


nc <- nc_open(ncname )
names(nc$var)

#variable's attributes
ncatt_get(r, T, "long_name")   #long name
ncatt_get(r, T, "units")       #measure unit
fillvalue <- ncatt_get(r, T, "_FillValue")  #(optional)  



leaflet() %>% addTiles()  %>% addRasterImage(x=r[[1]])





# CDC Wonder --------------------------------------------------------------

cdc <- read.delim("data/CDC/cdc_extract.txt")[-1] %>% mutate(County.Code = as.character(County.Code))
cou <- read_sf("data/Boundaries/tl_2020_us_county/tl_2020_us_county.shp") #county data
cou <- st_centroid(cou)
cdc <- merge(cou,cdc,by.x="GEOID",by.y="County.Code")
cdc <- st_transform(cdc, crs = "+proj=longlat +datum=WGS84")

leaflet() %>% addTiles() %>% addCircleMarkers(data=cdc$geometry,radius = as.numeric(cdc$Crude.Rate)/mean(as.numeric(cdc$Crude.Rate),na.rm=TRUE))
leaflet() %>% addTiles() %>% addCircleMarkers(data=cdc$geometry)

