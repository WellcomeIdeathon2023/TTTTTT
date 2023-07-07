# Processing code for select a state and plot the methane trends
rm(list=ls()) # clear environment

library(lubridate)
library(raster)

# netcdf file
nc <- stack("data/Copernicus/200301_202006-C3S-L3_GHG-GHG_PRODUCTS-MERGED-MERGED-OBS4MIPS-MERGED-v4.3.nc",
            varname ="xch4")

# county data
st <- st_read("data/Boundaries/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
st <- st_transform(st,4326)
sp <- as_Spatial(st)

# This takes a while to run - we are saving this to speed up!
out <- raster::extract(nc,sp,fun=mean,cellnumbers=TRUE,sp=T,na.rm=TRUE) %>% as.data.frame()

out_long <- out %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "val")
out_long$date <- ymd(substring(out_long$date, 2))

# save out_long for quick reading in
save(out_long, file = "data/Processed/copernicus_long.Rdata")
