# Parameters for transforming SIMLESA dataset in farmhousehold format
# Last update: 27/05/2024

library(rhomis) # for currency conversion

prefix <- "SIMLESA_MWI_2013"

# set the name of the livestock for each product
conv_prod <- c(
  "butter"="cattle",
  "cheese"="cattle",
  "eggs"="poultry",
  "honey"="beehive",
  "milk"="cattle",
  "skinandhide"=NA
)

conv_egg <- 0.05 #numbers to kg

# conversion factors to tropical livestock unit
conv_tlu <- c("beehive"=0,
              "cattle"=0.7,
              "donkey_horse"=0.7,
              "pig"=0.3,
              "smalllivestock"=0.1,
              "poultry"=0.01
)

# category for household diet diversity score
cat_hdds <- c(
  "Cereals"= "HDDS_G1_Grains_Roots_Tubers",
  "Vitamin A rich vegetables and tubers"="HDDS_G8_VitA_Fruits_Vegetables",
  "White tubers and roots"="HDDS_G1_Grains_Roots_Tubers",
  "Dark green leafy vegetables"="HDDS_G7_Leafy_Vegetables",
  "Other vegetables"="HDDS_G9_Other_Vegetables",
  "Vitamin A rich fruits" ="HDDS_G8_VitA_Fruits_Vegetables",
  "Other fruits" ="HDDS_G10_Other_Fruits",
  "Organ meat (iron rich)"="HDDS_G5_Meat",
  "Flesh meats"="HDDS_G5_Meat",
  "Eggs"="HDDS_G6_Eggs",
  "Fish"="HDDS_G5_Meat",
  "Legumes, nuts and seeds"="HDDS_G3_Nuts_Seeds",
  "Milk and milk products"="HDDS_G4_Milk",
  "Oils and fats"="HDDS_G11_Fats Oil",
  "Red palm products"="HDDS_G1_Grains_Roots_Tubers",
  "Sweets" ="HDDS_G12_Sugar",
  "Spices, condimets, beverages"="HDDS_G13_Spices",
  "Meal outside home"=NA
)


# Adult Male equivalent conversion factors for age classes 0-4, 5-10, 10-24, 24-50, 50+
conv_F <- c(0.5, 0.75, 0.75, 0.86, 0.6) #female
conv_M <- c(0.5, 0.75, 0.925, 1, 0.73) #male

# Conversion local currency unit to usd power parity purchase
conv_wb <- rhomis::currency_conversion
conv_lcu <- conv_wb$value[conv_wb$`Country Name`=="Malawi" & conv_wb$year==2013]

# Parameters for crop
conv_crop <- read.csv("../Data/Parameters/crop_param.csv")
conv_energy <- as.numeric(conv_crop$energy)
names(conv_energy) <- conv_crop$crop
conv_e2 <- as.numeric(conv_crop$energy)
names(conv_e2) <- gsub("_", "", conv_crop$crop)
conv_energy <- c(conv_energy, conv_e2)
conv_energy <- conv_energy[!duplicated(names(conv_energy))]
