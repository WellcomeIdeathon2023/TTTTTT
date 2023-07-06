

library(leaflet)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(ggplot2)

shp <- st_read("/Users/fabian/Downloads/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
shp <- st_transform(shp,"+proj=longlat +datum=WGS84")
shp$gas <- rnorm(nrow(shp), mean=.38, sd=.2) #simulate prevalence of gas cook stoves in the US
shp %<>% mutate(gas_cat = case_when(gas <.3 ~1,
                                    gas >= .3 & gas <.6 ~2,
                                    gas >.6 ~3
                                    ))
shp$gas_cat <- as.factor(shp$gas_cat)
levels(shp$gas_cat) <- c("<30%","30-60%","60+%")

col <- colorQuantile("YlOrRd",shp$gas)

leaflet() %>% addTiles() %>% addPolygons(data=shp,fillColor= ~col(shp$gas),col="white",weight=1) %>%
  addLegend(pal = col, values = shp$gas, group = "circles", position = "bottomleft",title="Households with gas stove")

#cdc dat
cdc <- read.delim("/Users/fabian/Downloads/Underlying Cause of Death, 2018-2021, Single Race-2.txt")[-1]
cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp,cdc,by.x="GEOID",by.y="State.Code")
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA


#by race
cdc <- read.delim("/Users/fabian/Downloads/Underlying Cause of Death, 2018-2021, Single Race-3.txt")[-1]
cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp,cdc,by.x="GEOID",by.y="State.Code")
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()


cdc %<>% arrange(desc(Single.Race.6))

ggplot(cdc, aes(x = gas_cat, y = Crude.Rate, group=gas_cat,
  fill = Single.Race.6)) +
  geom_bar(stat = "identity") +
  labs(x = "US state-level use of gas stoves", y = "Child deaths (<15 years)")+
  ggtitle("Gas stove use and child deaths \nfrom respiratory disease")+
  theme_linedraw()

