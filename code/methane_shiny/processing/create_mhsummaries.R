library(dplyr)
library(stringr)

# Read in mental health data
mh2020 <- read.csv("../../../data/mental_health/mhcld-puf-2020-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2019 <- read.csv("../../../data/mental_health/mhcld-puf-2019-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2018 <- read.csv("../../../data/mental_health/mhcld-puf-2018-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2017 <- read.csv("../../../data/mental_health/mhcld-puf-2017-csv.csv", header=TRUE, stringsAsFactors = FALSE)
mh2016 <- read.csv("../../../data/mental_health/mhcld-puf-2016-csv.csv", header=TRUE, stringsAsFactors = FALSE)

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

# Read in summaries from saved files
mh2020_state <- read.csv("../../../data/mental_health_summaries/mh2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2019_state <- read.csv("../../../data/mental_health_summaries/mh2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2018_state <- read.csv("../../../data/mental_health_summaries/mh2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2017_state <- read.csv("../../../data/mental_health_summaries/mh2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
mh2016_state <- read.csv("../../../data/mental_health_summaries/mh2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

trauma2020_state <- read.csv("../../../data/mental_health_summaries/trauma2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2019_state <- read.csv("../../../data/mental_health_summaries/trauma2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2018_state <- read.csv("../../../data/mental_health_summaries/trauma2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2017_state <- read.csv("../../../data/mental_health_summaries/trauma2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
trauma2016_state <- read.csv("../../../data/mental_health_summaries/trauma2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

anxiety2020_state <- read.csv("../../../data/mental_health_summaries/anxiety2020_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2019_state <- read.csv("../../../data/mental_health_summaries/anxiety2019_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2018_state <- read.csv("../../../data/mental_health_summaries/anxiety2018_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2017_state <- read.csv("../../../data/mental_health_summaries/anxiety2017_state.csv", header=TRUE, stringsAsFactors = FALSE)
anxiety2016_state <- read.csv("../../../data/mental_health_summaries/anxiety2016_state.csv", header=TRUE, stringsAsFactors = FALSE)

shp <- st_read("../../data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
shp <- st_transform(shp,"+proj=longlat +datum=WGS84")

# Merge states for consistency among datasets
# Pad statefips with 0s to match shapefile
mh2020_state$STATEFIP <- str_pad(mh2020_state$STATEFIP, 2, pad = "0")
mh2019_state$STATEFIP <- str_pad(mh2019_state$STATEFIP, 2, pad = "0")
mh2018_state$STATEFIP <- str_pad(mh2018_state$STATEFIP, 2, pad = "0")
mh2017_state$STATEFIP <- str_pad(mh2017_state$STATEFIP, 2, pad = "0")
mh2016_state$STATEFIP <- str_pad(mh2016_state$STATEFIP, 2, pad = "0")

trauma2020_state$STATEFIP <- str_pad(trauma2020_state$STATEFIP, 2, pad = "0")
trauma2019_state$STATEFIP <- str_pad(trauma2019_state$STATEFIP, 2, pad = "0")
trauma2018_state$STATEFIP <- str_pad(trauma2018_state$STATEFIP, 2, pad = "0")
trauma2017_state$STATEFIP <- str_pad(trauma2017_state$STATEFIP, 2, pad = "0")
trauma2016_state$STATEFIP <- str_pad(trauma2016_state$STATEFIP, 2, pad = "0")

anxiety2020_state$STATEFIP <- str_pad(anxiety2020_state$STATEFIP, 2, pad = "0")
anxiety2019_state$STATEFIP <- str_pad(anxiety2019_state$STATEFIP, 2, pad = "0")
anxiety2018_state$STATEFIP <- str_pad(anxiety2018_state$STATEFIP, 2, pad = "0")
anxiety2017_state$STATEFIP <- str_pad(anxiety2017_state$STATEFIP, 2, pad = "0")
anxiety2016_state$STATEFIP <- str_pad(anxiety2016_state$STATEFIP, 2, pad = "0")

# Merge with shapefile states
shp_states <- data.frame(shp$GEOID)
names(shp_states)[1] <- "GEOID"
mh2020_state <- merge(shp_states, mh2020_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
mh2019_state <- merge(shp_states, mh2019_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
mh2018_state <- merge(shp_states, mh2018_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
mh2017_state <- merge(shp_states, mh2017_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
mh2016_state <- merge(shp_states, mh2016_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)

trauma2020_state <- merge(shp_states, trauma2020_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
trauma2019_state <- merge(shp_states, trauma2019_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
trauma2018_state <- merge(shp_states, trauma2018_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
trauma2017_state <- merge(shp_states, trauma2017_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
trauma2016_state <- merge(shp_states, trauma2016_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)

anxiety2020_state <- merge(shp_states, anxiety2020_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
anxiety2019_state <- merge(shp_states, anxiety2019_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
anxiety2018_state <- merge(shp_states, anxiety2018_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
anxiety2017_state <- merge(shp_states, anxiety2017_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)
anxiety2016_state <- merge(shp_states, anxiety2016_state, by.x="GEOID", by.y="STATEFIP", all=TRUE)

mh2020_state <- subset(mh2020_state, select = -c(X))
mh2019_state <- subset(mh2019_state, select = -c(X))
mh2018_state <- subset(mh2018_state, select = -c(X))
mh2017_state <- subset(mh2017_state, select = -c(X))
mh2016_state <- subset(mh2016_state, select = -c(X))

trauma2020_state <- subset(trauma2020_state, select = -c(X))
trauma2019_state <- subset(trauma2019_state, select = -c(X))
trauma2018_state <- subset(trauma2018_state, select = -c(X))
trauma2017_state <- subset(trauma2017_state, select = -c(X))
trauma2016_state <- subset(trauma2016_state, select = -c(X))

anxiety2020_state <- subset(anxiety2020_state, select = -c(X))
anxiety2019_state <- subset(anxiety2019_state, select = -c(X))
anxiety2018_state <- subset(anxiety2018_state, select = -c(X))
anxiety2017_state <- subset(anxiety2017_state, select = -c(X))
anxiety2016_state <- subset(anxiety2016_state, select = -c(X))

mhcomplete_state <- merge(mh2020_state, mh2019_state, by="GEOID", all=TRUE)
names(mhcomplete_state)[2] <- "NUMMHS_2020"
names(mhcomplete_state)[3] <- "NUMMHS_2019"
mhcomplete_state <- merge(mhcomplete_state, mh2018_state, by="GEOID", all=TRUE)
names(mhcomplete_state)[4] <- "NUMMHS_2018"
mhcomplete_state <- merge(mhcomplete_state, mh2017_state, by="GEOID", all=TRUE)
names(mhcomplete_state)[5] <- "NUMMHS_2017"
mhcomplete_state <- merge(mhcomplete_state, mh2016_state, by="GEOID", all=TRUE)
names(mhcomplete_state)[6] <- "NUMMHS_2016"

traumacomplete_state <- merge(trauma2020_state, trauma2019_state, by="GEOID", all=TRUE)
names(traumacomplete_state)[2] <- "TRAUMA_2020"
names(traumacomplete_state)[3] <- "TRAUMA_2019"
traumacomplete_state <- merge(traumacomplete_state, trauma2018_state, by="GEOID", all=TRUE)
names(traumacomplete_state)[4] <- "TRAUMA_2018"
traumacomplete_state <- merge(traumacomplete_state, trauma2017_state, by="GEOID", all=TRUE)
names(traumacomplete_state)[5] <- "TRAUMA_2017"
traumacomplete_state <- merge(traumacomplete_state, trauma2016_state, by="GEOID", all=TRUE)
names(traumacomplete_state)[6] <- "TRAUMA_2016"

anxietycomplete_state <- merge(anxiety2020_state, anxiety2019_state, by="GEOID", all=TRUE)
names(anxietycomplete_state)[2] <- "ANXIETY_2020"
names(anxietycomplete_state)[3] <- "ANXIETY_2019"
anxietycomplete_state <- merge(anxietycomplete_state, anxiety2018_state, by="GEOID", all=TRUE)
names(anxietycomplete_state)[4] <- "ANXIETY_2018"
anxietycomplete_state <- merge(anxietycomplete_state, anxiety2017_state, by="GEOID", all=TRUE)
names(anxietycomplete_state)[5] <- "ANXIETY_2017"
anxietycomplete_state <- merge(anxietycomplete_state, anxiety2016_state, by="GEOID", all=TRUE)
names(anxietycomplete_state)[6] <- "ANXIETY_2016"

# From summaries, create a summary of how these have changed over time
mhcomplete_state$mh_change <- mh2020_state$NUMMHS - mh2018_state$NUMMHS
traumacomplete_state$trauma_change <- trauma2020_state$TRAUMA - trauma2018_state$TRAUMA
anxietycomplete_state$anxiety_change <- anxiety2020_state$anxiety - anxiety2018_state$anxiety

write.csv(mhcomplete_state, "../../../data/Processed/mental_health_summaries/mhcomplete_state.csv")
write.csv(traumacomplete_state, "../../../data/Processed/mental_health_summaries/traumacomplete_state.csv")
write.csv(anxietycomplete_state, "../../../data/Processed/mental_health_summaries/anxietycomplete_state.csv")
