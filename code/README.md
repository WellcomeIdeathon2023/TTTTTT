# Dashboard for methane and health data
## TTTTTT

This dashboard brings together health and climate data to explore health effects of methane with a geographic focus on the US.

Our dashboard is built using R Shiny. 

More information on the data is provided in the `data/READMDE.md`. 

# Installation and requirements
A recent verison of R: version >= 4.2.1 
It requires the following packages to run:
- dplyr
- epiR
- ggplot2
- leaflet
- leaflet.minicharts
- magrittr
- raster
- sf
- scales
- shiny
- stringr
- shinyjs
- terra
- tidyverse

Once these have been installed, the file `methane_shiny.R` in the `code/methane_shiny` folder is the entry point for the application. A convenient way to run this is through `RStudio`, which includes a `run app` button. 


## Running from the command line
Navigate to the methane_shiny folder (assuming you are in the root folder)
```shell
>>> cd code/methane_shiny
```
Finally, run the `methane_shiny.R` scirpt:
```shell
>>> Rscript methane_shiny.R 
```

Feel free to reach out to us if you have any issues running the application!