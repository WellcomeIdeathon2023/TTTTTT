

cdc <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-4.txt")[-1] %>% na.omit() %>% 
  mutate(Crude.Rate = as.character(Crude.Rate)) %>% filter(Gender.Code != "")
cdc2 <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-5.txt")[-1] %>% na.omit() 
cdc3 <- read.delim("../data/CDC/Underlying Cause of Death, 2018-2021, Single Race-6.txt")[-1] %>% na.omit() 
cdc$group <- "gender"; cdc$val <- paste0("Gender - ",cdc$Gender)
cdc2$group <- "race"; cdc2$val <- paste0("Race - ",cdc2$Single.Race.6)
cdc3$group <- "children (1-14)"; cdc3$val <- paste0("Age - children (1-14)")
cdc <- bind_rows(cdc,cdc2,cdc3)

cdc$State.Code <- ifelse(nchar(as.character(cdc$State.Code))==1,paste0("0",cdc$State.Code),cdc$State.Code)
cdc <- merge(shp %>% filter(NAME != "Hawaii"),cdc,by.x="GEOID",by.y="State.Code") %>% st_centroid()
cdc$Crude.Rate[cdc$Crude.Rate=="Unreliable"] <- NA
cdc$Crude.Rate <- cdc$Crude.Rate %>% as.numeric()


p3 <- ggplot(data=cdc %>% filter(group =="gender"),
       aes(y=Crude.Rate,x=val,fill=val)) + 
  geom_col(position = position_dodge(width = 0.9)) +theme_linedraw()+
  xlab("Group") + ylab("Rate per 100k")+
  scale_fill_discrete(type=d3.schemeCategory10)+
  ggtitle("Respiratory disease by XXX")+
  theme_linedraw()
p3 + theme(text=element_text(size=15),legend.position = "none")





pdf <- cdc %>% filter(group =="gender") %>% mutate(Gender == group)
ggplot(data=pdf,
       aes(y=Crude.Rate,x=gas,group=Gender,col=Gender)) + 
  geom_point() + geom_smooth(method="lm",se =F) + 
  xlab("Prop. of households with gas stove per US State") + ylab("Rate per 100k")+
  scale_colour_discrete(type=d3.schemeCategory10)
  theme_dark()
  
  
  ggplot(data=pdf %>% filter(group =="race"),
         aes(y=Crude.Rate,x=gas_cat,group=Single.Race.6,fill=Single.Race.6)) + 
    geom_col(position = position_dodge(width = 0.9)) +theme_linedraw()
  


