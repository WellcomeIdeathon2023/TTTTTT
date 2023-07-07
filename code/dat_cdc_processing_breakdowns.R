

cdc <- read.delim("../../../Downloads/Underlying Cause of Death, 2018-2021, Single Race-4.txt")[-1] %>% na.omit() %>% 
  mutate(Crude.Rate = as.character("Crude.Rate")) %>% filter(Gender.Code != "")
cdc2 <- read.delim("../../../Downloads/Underlying Cause of Death, 2018-2021, Single Race-5.txt")[-1] %>% na.omit() 
cdc3 <- read.delim("../../../Downloads/Underlying Cause of Death, 2018-2021, Single Race-6.txt")[-1] %>% na.omit() 
cdc$group <- "race"; cdc$val <- paste0("Gender - ",cdc$Gender)
cdc2$group <- "race"; cdc2$val <- paste0("Race - ",cdc2$Single.Race.6)
cdc3$group <- "children (1-14)"; cdc3$val <- paste0("Age - children (1-14)")
cdc <- bind_rows(cdc,cdc2,cdc3)

cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp %>% filter(NAME != "Hawaii"),cdc,by.x="GEOID",by.y="State.Code") %>% st_centroid()
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()
