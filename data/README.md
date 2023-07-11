# README

We have added all processed and used data to this folder, thus it is self-contained and requires NO additional downloads, unless you want to re-process the mental health data from scratch. 

The three data-sets in the `Processed` folder were created by running the scripts in the `code/methane_shiny/processing` folder. 

# Processing mental health data
First, download 2016-2020 data-sets from the bottom of the SAHMSA website: https://www.samhsa.gov/data/data-we-collect/mh-cld-mental-health-client-level-data

Put these `.csv` files into `mental_health` folder under `data`. Finally, navigate to `processing/create_mhsummaries.R` and run this script in `R` to create the processed mental health data. 


