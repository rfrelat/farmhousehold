# Empty script to transform household dataset in farmhousehold format
# Last update: 27/05/2024

# Make sure to use the last update of the farmhousehold package
# devtools::install_github("rfrelat/farmhousehold")
library(farmhousehold)

library(terra) # for spatial information

# Load the parameters
source("Param_XX.R")


# 1. Load data ----------------------------------
# load the files containing the data (csv is the preferred format) 

# 2. Create crop table --------------------------
# a data.frame 'crop' is created with the columns:
# hhid
# name
# land_area_ha 
# harvest_kg
# consumed_kg 
# sold_kg
# income_lcu


# 3. Create livestock table ---------------------
# a data.frame 'lstk' is created with the columns:
# hhid
# name
# n

# 4. Create livestock production table ----------
# a data.frame 'lstk_prod' is created with the columns:
# hhid
# name
# n
# prodwhole
# harvest_kg
# consumed_kg
# sold_kg
# income_lcu

# 5. Create household table ---------------------
# a data.frame 'lstk_prod' is created with the columns:
# hhid
# id_form
# country
# year
# large_region
# gps_lat
# gps_lon
# hh_size_members
# hh_size_mae
# off_farm_lcu
# off_farm_div
# currency_conversion_lcu_to_ppp
# + food security indicators


# 6. Add GIS data -------------------------------
# create a vector with household GPS coordinates
# p <- vect(cbind(hhinfo$gps_lon, hhinfo$gps_lat))

# Farming systems from Dixon
# fs <- rast("../Data/GIS/cell5m_Agroecology_FarmingSystems_FS_2012_TX.tif")
# activeCat(fs) <- 2 #to get the full name of AGROECOLOG
# fsp <- terra::extract(fs, p)
# hhinfo$farming_system <- as.character(fsp$AGROECOLOG)

# Population
# pop <- rast("../Data/GIS/gpw_v4_population_density_rev11_2020_30s.tif")
# popp <- terra::extract(pop, p)$gpw_v4_population_density_rev11_2020_30_sec
# hhinfo$population_2020 <- popp

# Travel time to cities
# ttc <- rast("../Data/GIS/travel_time_to_cities_u6.tif")
# ttcp <- terra::extract(ttc, p)$travel_time_to_cities_1
# hhinfo$travel_time_cities <- ttcp



# 7. Post processing ----------------------------

# add summary of farm production

# check the name of the crop and livestock and find best matching one
crop$name <- bestname(crop$name, names(conv_energy), warn = TRUE)
lstk$name <- bestname(lstk$name, names(conv_tlu), warn = TRUE)
lstk_prod$name <- bestname(lstk_prod$name, names(conv_tlu), warn = TRUE)
lstk_prod$prod <- bestname(lstk_prod$prod, names(conv_energy), warn = TRUE)

# calculate crop and livestock summary characteristics
hhinfo <- calc_farm_prod(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy)

# this step could contain some data validity checks and deleting outliers

# 8. Save dataset -------------------------------
# create a farmhousehold object
hhdb_xx <- farmhousehold(
  "crop"=crop, 
  "lstk"=lstk, 
  "lstk_prod"=lstk_prod, 
  "hhinfo"=hhinfo,
  "conv_tlu"=conv_tlu, 
  "conv_energy"=conv_energy
)

# save it in a rds file
saveRDS(hhdb_xx,
     file="hhdb_xx.rds")
