suppressPackageStartupMessages({
  require(shiny)
  library(shinymeta)
  library(moments)
  library(corrplot)
  library(ade4)
  library(farmhousehold)
})

scaleChoices <- list("Country"="country",
                     "Project"="project",
                     "Köppen climate"="koeppen")

varChoices <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
                "foodshortage_count", "hdds",
                "crop_div","lstk_div","farm_div",
                "off_farm_income_perc",
                "crop_sold_perc","lstk_sold_perc","farm_sold_perc_kg",
                "crop_yield_kg_per_ha",
                "crop_income_div","lstk_income_div", "farm_income_div")

varDefault <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
                "farm_div","farm_sold_perc_kg","farm_income_div")
