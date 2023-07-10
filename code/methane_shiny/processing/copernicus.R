# Processing code for select a state and plot the methane trends
rm(list=ls()) # clear environment

library(lubridate)
library(raster)

# netcdf file
coper_netcdf <- stack("data/Copernicus/200301_202006-C3S-L3_GHG-GHG_PRODUCTS-MERGED-MERGED-OBS4MIPS-MERGED-v4.3.nc",
            varname ="xch4")

# county data
coper_shape <- st_read("data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
coper_shape <- st_transform(coper_shape,4326)
sp <- as_Spatial(coper_shape)

# This takes a while to run - we are saving this to speed up!
out <- raster::extract(coper_netcdf,sp,fun=mean,cellnumbers=TRUE,sp=T,na.rm=TRUE) %>% as.data.frame()

out_long <- out %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "val")
out_long$date <- ymd(substring(out_long$date, 2))

# create 3 and 5 year summaries
coper_shape$meth3 <- out_long %>% 
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2020-12-31")) %>%
  group_by(NAME) %>% summarise(val = mean(val,na.rm=T)) %>% dplyr::pull(val)

coper_shape$meth5 <- out_long %>% 
  filter(date >= as.Date("2016-01-01") & date <= as.Date("2020-12-31")) %>%
  group_by(NAME) %>% summarise(val = mean(val,na.rm=T)) %>% dplyr::pull(val)


col <- colorQuantile("YlOrRd",coper_shape$meth)

coper_shape2 <- filter(coper_shape,NAME!="Hawaii" & NAME != "Alaska")

#
leaflet() %>% addTiles() %>% addPolygons(data=coper_shape2,fillColor= ~col(coper_shape2$meth3 ),col="white",weight=1) %>%
  addLegend(pal = col, values = coper_shape2$meth3 , group = "circles", position = "bottomleft",title="Methan emissions")

leaflet() %>% addTiles() %>% addPolygons(data=coper_shape2,fillColor= ~col(coper_shape2$meth5 ),col="white",weight=1) %>%
  addLegend(pal = col, values = coper_shape2$meth5 , group = "circles", position = "bottomleft",title="Methan emissions") %>% addRasterImage(x=yrm5)


# save out_long for quick reading in
save(out_long, coper_shape2, file = "data/Processed/copernicus_long.Rdata")
