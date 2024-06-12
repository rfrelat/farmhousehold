suppressPackageStartupMessages({
  require(shiny)
  require(shinycssloaders)
  require(plotly)
  require(sf)
  require(tmap)
  require(farmhousehold)
})

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

varY <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
          "crop_yield_kg_per_ha",
          "lstk_yield_kg_per_tlu",
          "farm_div",
          "income_per_person_per_day_usd",
          "farm_sold_perc_kg",  "off_farm_perc",
          "pop_pressure_mae_per_ha", "lstk_pressure_tlu_per_ha",
          "farm_income_div", "hdds_score", "fies_score",
          "population_2020", "travel_time_cities")

# all variables
varX <- c("large_region", "country", "region", "koeppen", "farming_system","segmentation",
          varY)

# explanatory variable
varZ <- c("none","region", "koeppen", "farming_system", "travel_time_cities")

scaleChoices <- c(
  "large_region",
  "country",
  "region",
  "koeppen",
  "farming_system",
  "project")

#Geography variable
varG <- c("country", "region", "koeppen", "farming_system")

# Segmentation method
segChoices <- c("User-defined", "Geography") #"Dorward", "Farm orientation"
