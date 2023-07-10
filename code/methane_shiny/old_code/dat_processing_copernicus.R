
library(leaflet)
library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(ncdf4)

#read in as raster stack
nc <- stack("data/Copernicus/200301_202006-C3S-L3_GHG-GHG_PRODUCTS-MERGED-MERGED-OBS4MIPS-MERGED-v4.3.nc",
             varname ="xch4")
noms <- names(nc)
nc <- projectRaster(nc, crs=4326)
#writeRaster(nc,"input/copernicus.tif", format="GTiff",overwrite=TRUE)
#no need to but can write as seperate raster layers
#for(i in 1:2){
  #r <- nc[[i]] #subset to single raster layer
  #r <- projectRaster(r, crs=4326) #project to WGS84
  #writeRaster(r, filename = paste0("/Users/fabian/Downloads/Copernicus/",  noms[i], ".tif"), format = "GTiff") #write
#}

#viz one layer
#leaflet() %>% addTiles()  %>% addRasterImage(x=r)

#extract means over geographies
st <- st_read("data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp") #county data
st <- st_transform(st,4326)
sp <- as_Spatial(st)
crs(sp)



out <- raster::extract(nc,sp,fun=mean,cellnumbers=TRUE,sp=T,na.rm=TRUE) %>% as.data.frame()
st$meth <- out[,14]


col <- colorQuantile("YlOrRd",st$meth)

st2 <- filter(st,NAME!="Hawaii" & NAME != "Alaska")

leaflet() %>% addTiles() %>% addPolygons(data=st2,fillColor= ~col(st2$meth ),col="white",weight=1) %>%
  addLegend(pal = col, values = st2$meth , group = "circles", position = "bottomleft",title="Methan emissions")

#pivot to longer to map methane across time
library(tidyverse)
library(lubridate)
out_long <- out %>% 
  pivot_longer(cols = starts_with("X"), 
             names_to = "date", 
             values_to = "val")
out_long$date <- ymd(substring(out_long$date, 2))

#plot
ggplot(out_long) + geom_line(aes(x=date,y=val,group=STUSPS,col=STUSPS)) + theme_linedraw() +
  ggtitle("Average methane concentration per US State (2003-2016)")

#using gganimate to animate change over time
library(gganimate)
p <- ggplot(out_long %>% group_by(date) %>% summarise(val = mean(val,na.rm=TRUE)),aes(x=date,y=val)) + geom_line()+ theme_linedraw()+
  ggtitle("Average methane concentration, US average (2003-2016)")
p + transition_reveal(date)


#Mapping of emissions by continent
con <- st_read("data/Boundaries/World_Continents/World_Continents.shp") %>% st_simplify(dTolerance = .5)
st_crs(con)
con <- filter(con,CONTINENT!="Oceania" & CONTINENT != "Antarctica")

out2 <- raster::extract(nc,con,fun=mean,cellnumbers=TRUE,sp=T,na.rm=TRUE) %>% as.data.frame()
con$meth <- out2[,14]


#col <- colorNumeric("YlOrRd",out2$meth)

#leaflet() %>% addTiles() %>% addPolygons(data=con,fillColor= ~col(con$meth ),col="white",weight=1) %>%
  #addLegend(pal = col, values = con$meth , group = "circles", position = "bottomleft",title="Methan emissions")

out2_long <- out2 %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "val")
out2_long$date <- ymd(substring(out2_long$date, 2))

#plot: by continent
ggplot(out2_long) + geom_line(aes(x=date,y=val,group=CONTINENT,col=CONTINENT)) + theme_linedraw() +
  ggtitle("Average methane concentration by continent (2003-2016)") #+ transition_reveal(date)

#plo: overall
ggplot(out2_long %>% group_by(date) %>% summarise(val = mean(val,na.rm=TRUE)),aes(x=date,y=val)) + geom_line() + theme_linedraw() + 
  geom_smooth()+
  ggtitle("Average methane concentration over land area globally (2003-2016)")




