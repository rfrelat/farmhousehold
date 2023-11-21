suppressPackageStartupMessages({
  require(shiny)
  require(shinycssloaders)
  require(plotly)
  require(sf)
  require(tmap)
  require(farmhousehold)
})

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

logV <- c("crop_yield_kg_per_year_per_ha",
          "lstk_yield_kg_per_year_per_tlu",
          "farm_income_ppp_per_year",
          "off_farm_income_ppp_per_year")
varY <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
          "crop_yield_kg_per_year_per_ha",
          "lstk_yield_kg_per_year_per_tlu",
          "farm_div", "farm_income_ppp_per_year",
          "off_farm_income_ppp_per_year",
          "farm_sold_perc_kg",
          "off_farm_income_perc",
          "farm_income_div", "hdds")

# all variables
varX <- c("region", "koeppen", "farming_system",
          "population_2020", "travel_time_cities",
          varY, "hfias_status")

# explanatory variable
varZ <- c("none","region", "koeppen", "farming_system", "travel_time_cities")

scaleChoices <- list("Country"="country",
                     "Köppen climate"="koeppen",
                     "Farming system"="farming_system",
                     "Project"="project")
