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

"""
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
"""
# Read in summaries from saved files
mh2020_state <- read.csv("./data/mental_health_summaries/mh2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2019_state <- read.csv("./data/mental_health_summaries/mh2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2018_state <- read.csv("./data/mental_health_summaries/mh2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2017_state <- read.csv("./data/mental_health_summaries/mh2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2016_state <- read.csv("./data/mental_health_summaries/mh2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

trauma2020_state <- read.csv("./data/mental_health_summaries/trauma2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2019_state <- read.csv("./data/mental_health_summaries/trauma2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2018_state <- read.csv("./data/mental_health_summaries/trauma2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2017_state <- read.csv("./data/mental_health_summaries/trauma2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2016_state <- read.csv("./data/mental_health_summaries/trauma2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

anxiety2020_state <- read.csv("./data/mental_health_summaries/anxiety2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2019_state <- read.csv("./data/mental_health_summaries/anxiety2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2018_state <- read.csv("./data/mental_health_summaries/anxiety2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2017_state <- read.csv("./data/mental_health_summaries/anxiety2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2016_state <- read.csv("./data/mental_health_summaries/anxiety2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

# Read in the shapefile
shp <- st_read("./data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
shp <- st_transform(shp,"+proj=longlat +datum=WGS84")

# Load in state population data
state_pops <- read.csv("./data/state_populations.csv", header=TRUE, stringsAsFactors = FALSE)
state_pops <- state_pops[order(state_pops$state_code),]

make_map <- function(dataset, title, variable,shapefile = shp){
    dataset$STATEFIP <- str_pad(dataset$STATEFIP, 2, pad = "0")
    shp_merged <- merge(shp, dataset, by.x="GEOID", by.y="STATEFIP", all=TRUE)
    shp_merged <- shp_merged[order(shp_merged$GEOID),]
    #EDIT to delete appropriate rows
    shp_merged <- shp_merged[-c(52, 53), ]
    print(shp_merged$"NUMMHS")
    shp_merged$normalised_counts <- shp_merged$NUMMHS / as.numeric(state_pops$population) * 100000
    shp_merged %<>% mutate(mh_cat = case_when(normalised_counts <0 ~1,
                                    normalised_counts >= 0 & normalised_counts <1000 ~2,
                                    normalised_counts >= 1000 & normalised_counts <2000 ~3,
                                    normalised_counts >= 2000 & normalised_counts <3000 ~4,
                                    normalised_counts >= 3000 & normalised_counts <4000 ~5,
                                    normalised_counts >= 4000 & normalised_counts <5000 ~6,
                                    normalised_counts >= 5000 & normalised_counts <6000 ~7,
                                    normalised_counts >= 6000 & normalised_counts <7000 ~8,
                                    normalised_counts >= 7000 & normalised_counts <8000 ~9,
                                    normalised_counts >= 8000 & normalised_counts <9000 ~10,
                                    normalised_counts > 9000 ~11
                                    ))
    shp_merged$mh_cat <- as.factor(shp_merged$mh_cat)
    levels(shp_merged$mh_cat) <- c("<1000","1000-2000","2000-3000", "3000-4000", "4000-5000", "5000-6000", "6000-7000", "7000-8000", "8000-9000","9000+")

    col <- colorNumeric("YlOrRd",shp_merged$normalised_counts)

    leaflet() %>% addTiles() %>% addPolygons(data=shp_merged,fillColor= ~col(shp_merged$normalised_counts),col="white",weight=1) %>%
    addLegend(pal = col, values = shp_merged$normalised_counts, group = "circles", position = "bottomleft",title=title)
}

make_map(mh2020_state, "Mental health diagnoses per 100,000 people in 2020", "NUMMHS")
make_map(mh2019_state, "Mental health diagnoses per 100,000 people in 2019", "NUMMHS")

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

