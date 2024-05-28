# Empty parameter file for transforming household dataset in farmhousehold format

# here you should load all the parameters for the household dataset
# at least, it should contain:
# conv_energy: the crop energy conversion factor (kg to kcal)
# conv_tlu: the tropical livestock unit conversion factor
# currency_conversion_lcu_to_ppp: the currency conversion factor (per country and year)

# you can set it in text (if the number of items is limited) or load it from a file.

# for instance the prefix for the household id
# prefix <- "LSMS_MWI_2019"

# or a vector to convert areas into ha
# conv_ha <- c(
#   "acre"=0.404686,
#   "hectare"=1, 
#   "square meters"=0.0001,
#   "yards"=1/11960
# )

# or a vector to convert proportions
# conv_percentage <- c(
#   "Less than 1/4"=0.1,
#   "1/4"=0.25,
#   "1/2"=0.5,
#   "3/4"=0.75,
#   "More than 3/4"=0.9
# )

# or a vector with the conversion factors in Male Adult Equivalent
# conv_F <- c(0.5, 0.75, 0.75, 0.86, 0.6)
# conv_M <- c(0.5, 0.75, 0.925, 1, 0.73)

# or a file to convert local weight units into kg
# conv_kg <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ihs_seasonalcropconversion_factor_2020.csv")

# or a file with the crop energy values
# conv <- read.csv("../Data/Parameters/crop_param.csv")
# conv_energy <- as.numeric(conv$energy)
# names(conv_energy) <- conv$crop