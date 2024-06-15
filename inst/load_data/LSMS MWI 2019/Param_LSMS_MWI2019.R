# Parameters for transforming LSMS Malawi 2019 dataset in farmhousehold format
# Last update: 27/05/2024

library(farmhousehold) #for cleanname()
library(rhomis) #for currency conversion

prefix <- "LSMS_MWI_2019"

# get the crop quantities conversion factors
conv1 <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ihs_seasonalcropconversion_factor_2020.csv")

# add other conversion factors
conv2 <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ihs_foodconversion_factor_2020.csv")
conv2$unit_name <- ifelse(conv2$unit_name=="OTHER (SPECIFY)",
                          conv2$Otherunit, 
                          conv2$unit_name)
conv3 <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ihs_treeconversion_factor_2020.csv")

conv <- data.frame(rbind(
  cbind("name"=conv1$unit_name, "factor"=conv1$conversion),
  cbind("name"=conv2$unit_name, "factor"=conv2$factor),
  cbind("name"=conv3$unit_name, "factor"=conv3$conversion)))

conv_kg <- tapply(as.numeric(conv$factor),cleanname(conv$name), median)

conv_kg[grep("gon", conv_kg)]

conv_kg <- c(conv_kg,
             "1litrebottle"=1,
             "30litresdish"=30,
             "30litresbasin"=30,
             "40litrepail"=40,
             "40litrebasin"=40,
             "40litrebucket"=40,
             "45kg"=45,
             "5litrebasin"=5,
             "5litrebucket"=5,
             "5litrechigoba"=5,
             "5literchigoba"=5,
             "5litreschigoba"=5,
             "50kgbag"=50,
             "500mlcup"=0.5,
             "50kg"=50,
             "70kg"=60,
             "70kgbag"=60,
             "75kgbag"=60,
             "80kgbag"=65,
             "90kgbag"=70,
             "90kg"=70,
             "120kgbag"=90,
             "150kgbag"=100,
             "bag"=60,
             "basinunspecifiedsize"=3.5,
             "basket"=10,
             "basketunspecified"=10,
             "basketdengue"=10,
             "basket-dengu"=10,
             "besketdengu"=10,
             "busket"=10,
             "bigdengu"=10,
             "bottle"=1,
             "bucket"=5,
             "chigoba"=5,
             "chogoba"=5,  
             "crates"=NA,
             "dengu"=10,          
             "dengugogoda"=10,    
             "dish"=30, 
             "dovewhole"=0.1, 
             "eggs"=0.05,
             "gondola"=NA, 
             "gram"=0.001,
             "gram'"=0.001,
             "gramm"=0.001,
             "kalulu"=NA,
             "half"=0.5,
             "hand"=0.05, 
             "handful"=0.05,
             "HALFKG"=0.5,
             "largebasket"=10,
             "largebasketdengu"=10,
             "largebundle"=19.25,  
             "largepail"=15,
             "largebasin"=8.5,
             "litre"=1,
             "mediumbundle"=14,
             "mediumpail"=7.6,
             "ndowa"=5,
             "nolos"=NA,
             "ntolo"=NA,
             "mtolo"=NA,
             "mikolo"=NA,
             "oxcart"=468,
             "halfoxcart"=234,
             "pail5l"=5,
             "phaziplate"=0.1,
             "plasticdish"=0,1,
             "smallpig"=50,
             "smallbasin"=3.8,
             "fullsmallpig"=50,
             "phazi"=0.1,
             "piecesoffreshmaize"=0.1,
             "pigwhole"=75,
             "small.basin"=3.8,
             "smallbundle"=0.15,
             "smallpail"=4.25,
             "tan"=1000,
             "ton"=1000,
             "tone"=1000,
             "tonne"=1000,
             "torne"=1000,
             "tray's"=1.80,
             "trays"=1.80,
             "vitete"=NA,
             "weavingbasket"=10,
             "bigweavedbasket"=10,
             "wheelbarrow"=90,
             "fullgoat"=25,
             "biggoat"=25)

names(conv_kg) <- cleanname(names(conv_kg))
conv_kg <- conv_kg[!duplicated(names(conv_kg))]
conv_kg <- conv_kg[order(names(conv_kg))]

conv_lstk_prod <- data.frame(
  "id"=c("CHICKEN EGGS", "COW MILK", "GUINEA FOWL EGGS", "MANURE", "MEAT","SKINS AND HIDES"),
  "name"=c("POULTRY", "CATTLE", "TURKEY/GUINEA FOWL", NA, NA, NA),
  "prod"=c("egg", "milk", "egg", "manure", "meat", "other")
)

conv_ha <- c(
  "ACRE"=0.404686,
  "HECTARE"=1, 
  "SQUARE METERS"=0.0001,
  "YARDS"=1/11960,
  "OTHER (SPECIFY)"=NA
)

conv_pland <- c(
  "Less than 1/4"=0.1,
  "1/4"=0.25,
  "1/2"=0.5,
  "3/4"=0.75,
  "More than 3/4"=0.9
)

conv_F <- c(0.5, 0.75, 0.75, 0.86, 0.6)
conv_M <- c(0.5, 0.75, 0.925, 1, 0.73)

conv_tlu <- c("BEEKEEPING"=0,
              "CATTLE"=0.7,
              "DONKEY/MULE/HORSE"=0.7,
              "DOVE/PIGEON"=0.01,
              "DUCK"=0.01,
              "GOAT"=0.1,
              "PIG"=0.3,
              "POULTRY"=0.01,
              "RABBIT"=0.01,
              "RATS"=0.01,
              "SHEEP"=0.1,
              "TURKEY/GUINEA FOWL"=0.05
)

conv_wb <- rhomis::currency_conversion
conv_lcu <- conv_wb$value[conv_wb$`Country Name`=="Malawi" & conv_wb$year==2019]


labdiet <- c("HDDS_G1_a_Cereals", "HDDS_G1_b_Roots Tubers", "HDDS_G23_Nuts Pulses", "HDDS_G79_Vegetables",
             "HDDS_G56_Meat Fish Eggs", "HDDS_G8_10_Fruits", "HDDS_G4_Milk", "HDDS_G11_Fats Oil", "HDDS_G12_Sugar", "HDDS_G13_Spices")


conv_paid <- c(
  "DAY"=1/5,
  "MONTH"=4, 
  "WEEK"=1
)
clean_paid <- c(
  "DAY"=31,
  "MONTH"=12, 
  "WEEK"=4
)
conv_profit <- c(
  "Almost all"=0.9,                                                              
  "About half"=0.5,                                                              
  "Almost none"=0.1,                                                             
  "About 25%"=0.25,
  "70%"=0.7,                                                                     
  "About 75%"=0.75,                                                          
  "None"=0,
  "15%"=0.15       
)

conv <- read.csv("../Data/Parameters/crop_param.csv")
conv_energy <- as.numeric(conv$energy)
names(conv_energy) <- conv$crop
