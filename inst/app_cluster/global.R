suppressPackageStartupMessages({
  require(shiny)
  library(shinymeta)
  library(moments)
  library(corrplot)
  library(ade4)
  library(farmhousehold)
})

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

scaleChoices <- list("Large region"= "large_region",
                     "Country"="country",
                     "Region"='region',
                     "Köppen climate"="koeppen",
                     "Farming system"="farming_system",
                     "Project"="project")

varChoices <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
          "crop_yield_kg_per_ha","crop_sold_perc","crop_income_div",
          "lstk_yield_kg_per_tlu","lstk_sold_perc","lstk_income_div",
          "farm_div","crop_div","lstk_div",
          "income_per_person_per_day_usd",
          "farm_sold_perc_kg",  "off_farm_perc",
          "pop_pressure_mae_per_ha", "lstk_pressure_tlu_per_ha",
          "farm_income_div", "hdds_score", "fies_score","foodshortage_count",
          "population_2020", "travel_time_cities")

varDefault <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
                "farm_div","farm_sold_perc_kg", "hdds_score")
