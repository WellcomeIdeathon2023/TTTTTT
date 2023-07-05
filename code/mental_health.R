install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("leaflet")
library(leaflet)
install.packages("stringr")
library(stringr)
install.packages("magrittr")
library(magrittr)

# Read in mental health data
mh2020 <- read.csv("./data/mental_health/mhcld-puf-2020-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2019 <- read.csv("./data/mental_health/mhcld-puf-2019-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2018 <- read.csv("./data/mental_health/mhcld-puf-2018-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2017 <- read.csv("./data/mental_health/mhcld-puf-2017-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2016 <- read.csv("./data/mental_health/mhcld-puf-2016-csv.csv", header=TRUE, stringsAsFactors = FALSE)

# Create dataframe of counts by state [state is STATEFIP, number of mental health diagnoses is NUMMHS]
# Other interesting variables to look at could be TRAUSTREFLG (trauma and stress related disorders) or ANXIETYFLG (anxiety disorders)
mh2020_state <- mh2020 %>% group_by(STATEFIP) %>% summarise(NUMMHS = sum(NUMMHS))
mh2019_state <- mh2019 %>% group_by(STATEFIP) %>% summarise(NUMMHS = sum(NUMMHS))
mh2018_state <- mh2018 %>% group_by(STATEFIP) %>% summarise(NUMMHS = sum(NUMMHS))
mh2017_state <- mh2017 %>% group_by(STATEFIP) %>% summarise(NUMMHS = sum(NUMMHS))
mh2016_state <- mh2016 %>% group_by(STATEFIP) %>% summarise(NUMMHS = sum(NUMMHS))

trauma2020_state <- mh2020 %>% group_by(STATEFIP) %>% summarise(TRAUMA = sum(TRAUSTREFLG))
trauma2019_state <- mh2019 %>% group_by(STATEFIP) %>% summarise(TRAUMA = sum(TRAUSTREFLG))
trauma2018_state <- mh2018 %>% group_by(STATEFIP) %>% summarise(TRAUMA = sum(TRAUSTREFLG))
trauma2017_state <- mh2017 %>% group_by(STATEFIP) %>% summarise(TRAUMA = sum(TRAUSTREFLG))
trauma2016_state <- mh2016 %>% group_by(STATEFIP) %>% summarise(TRAUMA = sum(TRAUSTREFLG))

anxiety2020_state <- mh2020 %>% group_by(STATEFIP) %>% summarise(anxiety = sum(ANXIETYFLG))
anxiety2019_state <- mh2019 %>% group_by(STATEFIP) %>% summarise(anxiety = sum(ANXIETYFLG))
anxiety2018_state <- mh2018 %>% group_by(STATEFIP) %>% summarise(anxiety = sum(ANXIETYFLG))
anxiety2017_state <- mh2017 %>% group_by(STATEFIP) %>% summarise(anxiety = sum(ANXIETYFLG))
anxiety2016_state <- mh2016 %>% group_by(STATEFIP) %>% summarise(anxiety = sum(ANXIETYFLG))

# Read in the shapefile
shp <- st_read("./data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
shp <- st_transform(shp,"+proj=longlat +datum=WGS84")

# Load in state population data
state_pops <- read.csv("./data/state_populations.csv", header=TRUE, stringsAsFactors = FALSE)
state_pops <- state_pops[order(state_pops$state_code),]

# Put the mental health data into the shapefile for mapping
mh2020_state$STATEFIP <- str_pad(mh2020_state$STATEFIP, 2, pad = "0")
shp_mhcasenumbers2020 <- merge(shp, mh2020_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
shp_mhcasenumbers2020 <- shp_mhcasenumbers2020[order(shp_mhcasenumbers2020$GEOID),]
shp_mhcasenumbers2020 <- shp_mhcasenumbers2020[-c(52, 53), ]
shp_mhcasenumbers2020$normalised <- as.numeric(shp_mhcasenumbers2020$NUMMHS) / as.numeric(state_pops$population) * 100000

shp_mhcasenumbers2020 %<>% mutate(mh_cat = case_when(normalised <0 ~1,
                                    normalised >= 0 & normalised <1000 ~2,
                                    normalised >= 1000 & normalised <2000 ~3,
                                    normalised >= 2000 & normalised <3000 ~4,
                                    normalised >= 3000 & normalised <4000 ~5,
                                    normalised >= 4000 & normalised <5000 ~6,
                                    normalised >= 5000 & normalised <6000 ~7,
                                    normalised >= 6000 & normalised <7000 ~8,
                                    normalised >= 7000 & normalised <8000 ~9,
                                    normalised >= 8000 & normalised <9000 ~10,
                                    normalised > 9000 ~11
                                    ))
shp_mhcasenumbers2020$mh_cat <- as.factor(shp_mhcasenumbers2020$mh_cat)
levels(shp_mhcasenumbers2020$mh_cat) <- c("<1000","1000-2000","2000-3000", "3000-4000", "4000-5000", "5000-6000", "6000-7000", "7000-8000", "8000-9000","9000+")

col <- colorNumeric("YlOrRd",shp_mhcasenumbers2020$normalised)

leaflet() %>% addTiles() %>% addPolygons(data=shp_mhcasenumbers2020,fillColor= ~col(shp_mhcasenumbers2020$normalised),col="white",weight=1) %>%
  addLegend(pal = col, values = shp_mhcasenumbers2020$normalised, group = "circles", position = "bottomleft",title="Mental health diagnoses per 100,000 people")

#cdc dat
cdc <- read.delim("/Users/fabian/Downloads/Underlying Cause of Death, 2018-2021, Single Race-2.txt")[-1]
cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp,cdc,by.x="GEOID",by.y="State.Code")
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA

library(ggplot2)

ggplot(data=cdc,aes(x=gas,y=Crude.Rate)) + geom_point() + geom_smooth(method="lm") +
  geom_text(aes(x=gas,y=Crude.Rate, label = STUSPS),nudge_x = 0.02) +
xlab("Proportion of households with gas stove") + ylab("Child deaths (<15 years)\n from respiratory disease")+theme_linedraw()

#by race
cdc <- read.delim("/Users/fabian/Downloads/Underlying Cause of Death, 2018-2021, Single Race-3.txt")[-1]
cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp,cdc,by.x="GEOID",by.y="State.Code")
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()

ggplot(data=cdc,aes(x=gas,y=Crude.Rate,group=Single.Race.6,col=Single.Race.6)) + geom_point() + geom_smooth(method="lm") +
  geom_text(aes(x=gas,y=Crude.Rate, label = STUSPS),nudge_x = 0.02) +
  xlab("Proportion of households with gas stove") + ylab("Child deaths (<15 years)\n from respiratory disease")+
  theme_linedraw()
cdc %<>% arrange(desc(Single.Race.6))

ggplot(cdc, aes(x = gas_cat, y = Crude.Rate, group=gas_cat,
                fill = Single.Race.6)) +
  geom_bar(stat = "identity") +
  labs(x = "US state-level use of gas stoves", y = "Child deaths (<15 years)")+
    ggtitle("Gas stove use and child deaths \nfrom respiratory disease")+
  theme_linedraw()