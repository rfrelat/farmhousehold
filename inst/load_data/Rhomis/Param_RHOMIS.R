# Parameters for transforming RHOMIS dataset in farmhousehold format
# Last update: 27/05/2024

library(farmhousehold) # for cleanname
tlu <- read.csv("../Data/Parameters/lstk_param.csv", sep=";")
conv_tlu <- as.numeric(tlu$TLU_Conversion)
names(conv_tlu) <- cleanname(tlu$Animal)


# Missing crop energy conversion: hop, pyrethrum, rough_pea
crop <- read.csv("../Data/Parameters/crop_param.csv")
conv_energy <- as.numeric(crop$energy)
names(conv_energy) <- crop$crop

ten_groups <- list("HDDS_G1_Grains_Roots_Tubers"="grainsrootstubers",
                   "HDDS_G2_Legumes"="legumes",
                   "HDDS_G3_Nuts_Seeds"="nuts_seeds",
                   "HDDS_G4_Milk"="milk_dairy",
                   "HDDS_G5_Meat"="meat",
                   "HDDS_G6_Eggs"="eggs",
                   "HDDS_G7_Leafy_Vegetables"="veg_leafy",
                   "HDDS_G8_VitA_Fruits_Vegetables"="vita_veg_fruit",
                   "HDDS_G9_Other_Vegetables"="vegetables",
                   "HDDS_G10_Other_Fruits"="fruits"
)
fies_lab <- c("FIES_1_Worried",
               "FIES_2_UnableHealthy",
               "FIES_3_FewKinds",
               "FIES_4_SkipMeal",
               "FIES_5_AteLess",
               "FIES_6_RunOut",
               "FIES_7_Hungry",
               "FIES_8_SkipDay")

convtime <- list("daily"=1,
                 "fewperweek"=1,
                 "weekly"=1,
                 "fewpermonth"=0,
                 "monthly"=0,
                 "never"=0,
                 "0"=0,
                 "no_answer"=0)

conv_kg <- c(
  "1"="Af Tropical, rainforest",
  "2"="Am Tropical, monsoon",
  "3"="Aw Tropical, savannah",
  "4"= "BWh Arid, desert, hot",
  "5"= "BWk Arid, desert, cold",
  "6"= "BSh Arid, steppe, hot",
  "7"= "BSk Arid, steppe, cold",
  "8"= "Csa Temperate, dry summer, hot summer",
  "9"= "Csb Temperate, dry summer, warm summer",
  "10"= "Csc Temperate, dry summer, cold summer",
  "11"= "Cwa Temperate, dry winter, hot summer",
  "12"= "Cwb Temperate, dry winter, warm summer",
  "13"= "Cwc Temperate, dry winter, cold summer",
  "14"= "Cfa Temperate, no dry season, hot summer",
  "15"= "Cfb Temperate, no dry season, warm summer",
  "16"= "Cfc Temperate, no dry season, cold summer",
  "17"= "Dsa Cold, dry summer, hot summer",
  "18"= "Dsb Cold, dry summer, warm summer",
  "19"= "Dsc Cold, dry summer, cold summer",
  "20"= "Dsd Cold, dry summer, very cold winter",
  "21"= "Dwa Cold, dry winter, hot summer",
  "22"= "Dwb Cold, dry winter, warm summer",
  "23"= "Dwc Cold, dry winter, cold summer",
  "24"= "Dwd Cold, dry winter, very cold winter",
  "25"= "Dfa Cold, no dry season, hot summer",
  "26"= "Dfb Cold, no dry season, warm summer",
  "27"= "Dfc Cold, no dry season, cold summer",
  "28"= "Dfd Cold, no dry season, very cold winter",
  "29"= "ET Polar, tundra",
  "30"= "EF Polar, frost"
)

large <- c(
  "bolivia"="Latin America",
  "burkina_faso"="Western Africa",
  "burundi"="Eastern Africa",
  "cambodia"="South+East Asia",
  "comoros"="Eastern Africa",
  "costa_rica"="Latin America",
  "cote d'ivoire"="Western Africa",
  "drc"="Middle Africa",
  "ecuador"="Latin America",
  "el_salvador"="Latin America",
  "ethiopia"="Eastern Africa",
  "gambia"="Western Africa",
  "ghana"="Western Africa",
  "guatemala"="Latin America",
  "honduras"="Latin America",
  "india"="South+East Asia",
  "kenya"="Eastern Africa",
  "malawi"="Eastern Africa",
  "mali"="Western Africa",
  "morocco"="Northern Africa",
  "nepal"="South+East Asia",
  "nicaragua"="Latin America",
  "niger"="Western Africa",
  "nigeria"="Western Africa",
  "palestine"="Middle East",
  "peru"="Latin America",
  "philipines"="South+East Asia",
  "rwanda"="Eastern Africa",
  "senegal"="Western Africa",
  "sierra leone"="Western Africa",
  "south africa"="Southern Africa",
  "tanzania"="Eastern Africa",
  "uganda"="Eastern Africa",
  "vietnam"="South+East Asia",
  "zambia"="Eastern Africa"
)
