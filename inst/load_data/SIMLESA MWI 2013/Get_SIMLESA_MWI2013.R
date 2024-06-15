# Transform SIMLESA dataset in farmhousehold format
# Last update: 27/05/2024

# Make sure to use the last update of the farmhousehold package
# devtools::install_github("rfrelat/farmhousehold")
library(farmhousehold)

library(dplyr) # for fast row binding
library(terra) # for spatial information
library(haven) # for reading sav files

# Load the parameters
source("Param_SIMLESA_MWI2013.R")


# 1. Load data ----------------------------------
# geographic variable
geo <- read_sav("../Data/SIMLESA_2013/Module 1 Household and village clean_1.sav")

# Crop production and use
t4a <- read_sav("../Data/SIMLESA_2013/Module 4  Part A  Crop Production for all Crop Grown _ Merged.sav")
t4b <- read_sav("../Data/SIMLESA_2013/Module 4 Part B Utilization of Crops Harvested.sav")
t4c <- read_sav("../Data/SIMLESA_2013/Module 4 Part C Marketing_ Phase1 Channels.sav")

# Livestock
t10 <- read_sav("../Data/SIMLESA_2013/Module 10 Part E Livestock ownership.sav")

# HDDS
t8 <- read_sav("../Data/SIMLESA_2013/Module 8 Food dietary score.sav")

# household composition
t2 <- read_sav("../Data/SIMLESA_2013/Module 2 Part A Household comp_1.sav")

# off farm
t11 <- read_sav("../Data/SIMLESA_2013/Module 11 Other sources of household income.sav")


# 2. Create crop table --------------------------

#correct if sum area >100
perctot <- NAto0(t4a$Area_intercropA)+NAto0(t4a$Area_intercropB)+NAto0(t4a$Area_intercropC)
perctot[perctot<100] <- 100
t4a$percA <- NAto0(as.numeric(t4a$Area_intercropA))/perctot
t4a$percB <- NAto0(as.numeric(t4a$Area_intercropB))/perctot
t4a$percC <- NAto0(as.numeric(t4a$Area_intercropC))/perctot

# some with zero percentage but harvest and crop...
# replace with proportion of harvest
zero <- t4a$percA+t4a$percB+t4a$percC==0
t4a$harvestA <- NAto0(t4a$A58a)+NAto0(t4a$A59a)
t4a$harvestB <- NAto0(t4a$A58b)+NAto0(t4a$A59b)
t4a$harvestC <- NAto0(t4a$A58c)+NAto0(t4a$A59c)
t4a$totharvest <- t4a$harvestA+t4a$harvestB+t4a$harvestC
t4a$percA[zero] <- NAto0(t4a$harvestA/t4a$totharvest)[zero]
t4a$percB[zero] <- NAto0(t4a$harvestB/t4a$totharvest)[zero]
t4a$percC[zero] <- NAto0(t4a$harvestC/t4a$totharvest)[zero]

# guess areas with tree (if any area left)
t4a$percD <- round(1-t4a$percA-t4a$percB-t4a$percC,5)

# merge crop A, B, C and tree (D)
field <- data.frame(
  "hhid"=rep(t4a$HHID,4),
  "name"=c(as_factor(t4a$MaincropA),as_factor(t4a$MaincropB),as_factor(t4a$MaincropC), as_factor(t4a$m4_p1_a15_a)),
  "land_area_ha"=c(t4a$Plotarea_ha*t4a$percA,t4a$Plotarea_ha*t4a$percB,t4a$Plotarea_ha*t4a$percC,t4a$Plotarea_ha*t4a$percD),
  "harvest_kg"=c(t4a$harvestA, t4a$harvestB, t4a$harvestC, rep(0, nrow(t4a)))
)
# all area reported was kept
# sum(field$land_area_ha, na.rm = TRUE) == sum(t4a$Plotarea_ha, na.rm=TRUE)
field <- field[!is.na(field$name),]
field <- field[!field$name%in%c("Not applicable", "NA"),]
field$name <- as.character(field$name)
field$name[grep("^Other", field$name)] <- "Other"


# summarize information per crop and per household
field$cropid <- paste(field$hhid, field$name, sep="_")
ha <- tapply(field$land_area_ha, field$cropid, sum, na.rm=TRUE)
kg <- tapply(field$harvest_kg, field$cropid, sum, na.rm=TRUE)
cropid <- strsplit(names(ha), "_")
crop <- data.frame(
  hhid=sapply(cropid, function(x)x[[1]]),
  name=sapply(cropid, function(x) ifelse(length(x)>1, x[[2]], "")),
  land_area_ha=as.numeric(ha),
  harvest_1=as.numeric(kg)
)

# crop uses, Module 4 part B
# simplify per crop and per household (to simplify merging with crop table)
t4b$name <- as.character(as_factor(t4b$m3_p1_b1_c))
t4b$name[grep("^Other", t4b$name)] <- "Other"
t4b <- t4b[!is.na(t4b$name),]

t4b$cropid <- paste(t4b$hhid, t4b$name, sep="_")
kg <- tapply(NAto0(t4b$m3_p1_ba), t4b$cropid, sum)
cropid <- strsplit(names(kg), "_")
crop_use <- data.frame(
  hhid=sapply(cropid, function(x)x[[1]]),
  name=sapply(cropid, function(x) ifelse(length(x)>1, x[[2]], "")),
  harvest_2=as.numeric(kg),
  sold_2=as.numeric(tapply(NAto0(t4b$m3_p1_be), t4b$cropid, sum)),
  kind_kg=as.numeric(tapply(NAto0(t4b$m3_p1_bf), t4b$cropid, sum)),
  seed_kg=as.numeric(tapply(NAto0(t4b$m3_p1_bg), t4b$cropid, sum)),
  consumed_kg=as.numeric(tapply(NAto0(t4b$m3_p1_bh), t4b$cropid, sum))
)

#merge the crop production and the crop use table
crop <- full_join(crop, crop_use, by = join_by(hhid, name))

# table(NAto0(crop$harvest_1)>0, NAto0(crop$harvest_2)>0)
# use the value that are not zero, and trust more harvest_2 because better link with uses
crop$harvest_kg <- ifelse(NAto0(crop$harvest_2)==0, NAto0(crop$harvest_1), NAto0(crop$harvest_2))
# if total use is higher than harvest, make sure to use the highest harvest
totuse <- rowSums(crop[,c("sold_2", "kind_kg", "seed_kg", "consumed_kg")])
crop$harvest_kg <- ifelse(totuse>crop$harvest_kg & crop$harvest_1>crop$harvest_kg,
                          crop$harvest_1, crop$harvest_kg)

# add sells, Module 4 part C
t4c$name <- as.character(as_factor(t4c$m4_pc_1_c1_a))
t4c$name[grep("^Other", t4b$name)] <- "Other"
t4c <- t4c[!is.na(t4c$name),]
t4c$cropid <- paste(t4c$hhid, t4c$name, sep="_")
kg <- tapply(NAto0(t4c$m4_pc_1_c7), t4c$cropid, sum)
lcu <- tapply(NAto0(t4c$m4_pc_1_c7)*NAto0(t4c$m4_pc_1_c8), t4c$cropid, sum)

cropid <- strsplit(names(kg), "_")
crop_sell <- data.frame(
  hhid=sapply(cropid, function(x)x[[1]]),
  name=sapply(cropid, function(x) ifelse(length(x)>1, x[[2]], "")),
  sold_3=as.numeric(kg),
  income_lcu=as.numeric(lcu)
)

#merge the crop production and the crop sell table
crop <- left_join(crop, crop_sell, by = join_by(hhid, name))
# use the value that are not zero, and trust more sold_2 because better link with uses
crop$sold_kg <- ifelse(crop$sold_2==0, crop$sold_3,crop$sold_2)

crop$name[crop$name%in%-99:117] <- "Other"
crop <- crop[NAto0(crop$land_area_ha)>0 | NAto0(crop$harvest_kg)>0 | crop$name!="Other",]

crop$use_list <- ifelse(NAto0(crop$consumed_kg)>0, "consume", "")
crop$use_list <- paste0(crop$use_list,
                        ifelse(NAto0(crop$sold_kg)>0, " sell", ""))
crop$use_list <- paste0(crop$use_list,
                        ifelse(NAto0(crop$kind_kg)>0, " payment", ""))
crop$use_list <- paste0(crop$use_list,
                        ifelse(NAto0(crop$seed_kg)>0, " seed", ""))
# table(crop$use_list)

# consistency among quantities
# if yield to high, use minimum among both
# else use maximum among both
maxkg <- ifelse(crop$harvest_kg>crop$consumed_kg+crop$sold_kg,
                crop$harvest_kg,
                crop$consumed_kg+crop$sold_kg)
minkg <- ifelse(crop$harvest_kg>crop$consumed_kg+crop$sold_kg,
                crop$consumed_kg+crop$sold_kg,
                crop$harvest_kg)

crop$harvest_kg <- ifelse(maxkg/crop$land_area_ha>20000,
                          minkg, maxkg)
psold <- crop$sold_kg/(crop$consumed_kg+crop$sold_kg)
# compute relative quantities to harvest_kg
crop$sold_kg <- ifelse(crop$consumed_kg+crop$sold_kg>crop$harvest_kg,
                                psold*crop$harvest_kg,
                                crop$sold_kg)
crop$consumed_kg <- ifelse(crop$consumed_kg+crop$sold_kg>crop$harvest_kg,
                                (1-psold)*crop$harvest_kg,
                                crop$consumed_kg)

#create hhid
crop$hhid <- paste(prefix, crop$hhid, sep="_")

sel <- c("hhid", "name", "land_area_ha",
         "harvest_kg", "consumed_kg",
         "sold_kg","income_lcu", "use_list")
crop <- crop[,sel]


# 3. Create livestock table ---------------------
livestock <- data.frame(
  "hhid"=paste(prefix, t10$HHID, sep="_"),
  "name"=as.character(as_factor(t10$livtype)),
  "n"=NAto0(t10$M6_P1_A3)
)

cattle <- c("Crossbred/exotic", "Heifers", "Bulls", "Calves", "Oxen", "Indigenous cows")
livestock$name[livestock$name%in%cattle] <- "cattle"

horseys <- c("Mule", "Donkeys", "Horse")
livestock$name[livestock$name%in%horseys] <- "donkey_horse"

livestock$name[grep("^Poutry", livestock$name)] <- "poultry"

livestock$name <- bestname(livestock$name, names(conv_tlu))
#divide between livestock and livestock production table
prod <- c("butter", "cheese", "eggs", "honey", "milk", "skinandhide")

lstk <- livestock[!livestock$name%in%prod & !is.na(livestock$name) & livestock$n>0,]
# simplify table
lstk$lstkid <- paste(lstk$hhid, lstk$name, sep=";")
nsum <- tapply(lstk$n, lstk$lstkid, sum, na.rm=TRUE)
id <- strsplit(names(nsum), ";")
lstk <- data.frame(
  hhid=sapply(id, function(x)x[[1]]),
  name=sapply(id, function(x)x[[2]]),
  n=as.numeric(nsum)
)

# 4. Create livestock production table ----------
lstk_prod <- data.frame(
  "hhid"=livestock$hhid,
  "name"=ifelse(livestock$name%in%prod,
                conv_prod[livestock$name], livestock$name),
  "prod"=ifelse(livestock$name%in%prod,
                livestock$name, "whole"),
  "sold_kg" = NAto0(t10$M6_P1_A6),
  "income_1"= NAto0(t10$M6_P1_A6)*NAto0(t10$M6_P1_A7),
  "income_2"= NAto0(t10$valsold)
)

#keep relevant information with production or sales
lstk_prod <- lstk_prod[(lstk_prod$income_1>0 | lstk_prod$sold_kg>0) & !is.na(lstk_prod$name),]

#most probably whole >500 is not real
lstk_prod$sold_kg[lstk_prod$prod %in% "whole" & lstk_prod$sold_kg>500] <- NA

#calculate the median price
lstk_prod$nameprod <- paste(lstk_prod$name, lstk_prod$prod, sep="_")
fullprice <- data.frame(
  "price"=c(lstk_prod$income_1/lstk_prod$sold_kg,
            lstk_prod$income_2/lstk_prod$sold_kg),
  "nameprod"=rep(paste(lstk_prod$name, lstk_prod$prod, sep="_"),2)
)
fullprice <- fullprice[NAto0(fullprice$price)>0,]
medprice <- tapply(fullprice$price, fullprice$nameprod, median, na.rm=TRUE)
lstk_prod$med_income <- lstk_prod$sold_kg* medprice[match(lstk_prod$nameprod, names(medprice))]

out1 <- lstk_prod$income_1<lstk_prod$med_income*0.2 | lstk_prod$income_1>lstk_prod$med_income*5 | is.na(lstk_prod$sold_kg)
out2 <- lstk_prod$income_2<lstk_prod$med_income*0.2 | lstk_prod$income_2>lstk_prod$med_income*5 | is.na(lstk_prod$sold_kg)

lstk_prod$income_lcu <- ifelse(out1 & !out2, lstk_prod$income_2, lstk_prod$income_1)

#convert egg into kg
lstk_prod$sold_kg <- ifelse(lstk_prod$prod %in% "egg",
                            lstk_prod$sold_kg*conv_egg, lstk_prod$sold_kg)


lstk_prod$sold_kg <- ifelse(lstk_prod$prod %in% "whole",
                            lstk_prod$sold_kg*conv_tlu[lstk_prod$name]*250,
                            lstk_prod$sold_kg)
lstk_prod$harvest_kg <- lstk_prod$sold_kg
lstk_prod$consumed_kg <- 0

sel <- c("hhid","name","prod","harvest_kg","consumed_kg","sold_kg","income_lcu")
lstk_prod <- lstk_prod[,sel]
# table(lstk_prod$name, lstk_prod$prod)

# 5. Create household table ---------------------
# start from geographic information
hhinfo <- geo

# create hhid
hhinfo$hhid <- paste(prefix, hhinfo$hhldid, sep="_")
hhinfo$id_form <- prefix

# gps coordinates
hhinfo$gps_lon <- NA
hhinfo$gps_lat <- NA

hhinfo$country <- "Malawi"
hhinfo$year <- "2013"

# currency conversion
hhinfo$currency_conversion_lcu_to_ppp <- conv_lcu


# calculate household size
# remove members without age nor gender
t2 <- t2[!(is.na(t2$Sex)&is.na(t2$Age)&is.na(t2$M2_P1_A4B)&is.na(t2$Member_ID)),]
# calculate age with month when missing
t2$Age <- ifelse(is.na(t2$Age), t2$M2_P1_A4B/12, t2$Age)
# table(is.na(t2$Age))

#calculate number of members per household
hh_size_members <- tapply(t2$Member_ID, t2$HHID, lunique)
hhinfo$hh_size_members <- hh_size_members[match(hhinfo$hhldid, names(hh_size_members))]

ageC <- cut(t2$Age, breaks = c(0,4,10,24,50, 150), include.lowest = TRUE)
# table(ageC, useNA="ifany") #71 NA, sum(is.na(t2$Age))/nrow(t2) <1%
convAG <- ifelse(as_factor(t2$Sex)=="Male",
                 conv_M[ageC], conv_F[ageC])
hh_size_mae <- tapply(convAG, t2$HHID, sum)
hhinfo$hh_size_mae <- hh_size_mae[match(hhinfo$hhldid, names(hh_size_mae))]
# table(hhinfo$hh_size_mae, useNA="ifany")
# plot(hhinfo$hh_size_members,hhinfo$hh_size_mae)
# some outliers

# household head
t2_head <- t2[t2$Relationship%in%1,]
# table(duplicated(t2_head$HHID))# some duplicates .. :(
head_age <- tapply(t2_head$Age, t2_head$HHID, max)
simplsex <- ifelse(as_factor(t2_head$Sex)=="Male", "m", "f")
head_gender <- tapply(simplsex, t2_head$HHID, function(x)
  {paste(sort(unique(x)), collapse="")})
hhinfo$head_age <- head_age[match(hhinfo$hhldid, t2_head$HHID)]
hhinfo$head_gender <- head_gender[match(hhinfo$hhldid, t2_head$HHID)]

# Off farm income
# from individual survey
# table(is.na(t11$cashincome), is.na(t11$totalincome))

#sum per household
off_farm_lcu <- tapply(t11$totalincome, t11$hhid, sum, na.rm=TRUE)
# number of income sources
off_div <- tapply(NAto0(t11$totalincome)>0, t11$hhid, sum, na.rm=TRUE)
# boxplot(off_job_lcu~off_div)

hhinfo$off_farm_lcu <- NAto0(off_farm_lcu[match(hhinfo$hhldid, names(off_farm_lcu))])
hhinfo$off_farm_div <- NAto0(off_div[match(hhinfo$hhldid, names(off_div))])


# food security
# calculate hdds

#clean the list of diet group
t8 <- t8[t8$M5_P1_1%in%1:18 & !t8$M5_P1_2%in%c("0"), ]
# simplify food groups
t8$cat <- cat_hdds[as_factor(t8$M5_P1_2)]

#remove the Meal outside home
t8 <- t8[!is.na(t8$cat),]
# make the 101, 110, 111 like "1"
t8$M5_P1_4 <- substr(as.numeric(t8$M5_P1_4), 0, 1)

diet <- tapply(t8$M5_P1_4%in%"1", list(t8$hhid, t8$cat), any)
# hypothese, if NA = NO
diet[is.na(diet)] <- FALSE
dietBin <- apply(diet,2, as.numeric)
row.names(dietBin) <- row.names(diet)

dietBin <- dietBin[match(hhinfo$hhldid, rownames(diet)),]
hhinfo$hdds_score <- rowSums(dietBin, na.rm=TRUE)
#table(hhinfo$hdds, useNA="ifany")
hhinfo <- cbind(hhinfo, dietBin)

#number of month with food shortage
# colM <- grep("s8q07_", names(thh8))
# nmonth <- ifelse(thh8$s8q06=="1. YES",
#                  rowSums(thh8[,colM]), 0)
# nmonth <- ifelse(nmonth>12, 12, nmonth)
#
# wmonth <- apply(thh8[,colM], 1, function(x){
#   paste(unique(month.abb[c(5:12, 1:6)][x==1]), collapse = " ")
# })
# wmonth <- ifelse(nmonth==0, "", wmonth)
# hhinfo$foodshortage_count <- nmonth[match(hhinfo$household_id, thh8$household_id)]
# hhinfo$foodshortage_months <- wmonth[match(hhinfo$household_id,  thh8$household_id)]


selc <- c("hhid","id_form", "country", "year",
         "gps_lat", "gps_lon",
         "hh_size_members", "hh_size_mae",
         "head_age", "head_gender",
         "off_farm_lcu", "off_farm_div",
         "hdds_score", colnames(dietBin),
         "currency_conversion_lcu_to_ppp")

# keep only crop or livestock farmers
# and remove urban households
selhh <- hhinfo$hhid%in%crop$hhid | hhinfo$hhid%in%lstk$hhid | hhinfo$hhid%in%lstk_prod$hhid
hhinfo <- hhinfo[selhh,selc]
crop <- crop[crop$hhid %in%hhinfo$hhid,]
lstk <- lstk[lstk$hhid %in%hhinfo$hhid,]
lstk_prod <- lstk_prod[lstk_prod$hhid %in%hhinfo$hhid,]


# 6. Add GIS data -------------------------------
# Get administrative information
adm <- vect("../Data/MalawiCS/MW_Admin3_2003.shp")
# adm <- vect("../Data/GIS/gadm41_MWI_3.shp")

# get coordinates from administrative borders
coo <- crds(centroids(adm), df=TRUE)
# centroid per district
cooDisx <- tapply(coo$x, adm$ADMIN2, mean)
cooDisy <- tapply(coo$y, adm$ADMIN2, mean)
geo$District <- gsub("Denza", "Dedza", as_factor(geo$Distric2))
#unique(geo$District) %in% names(cooDisx)
# coordinates EPA
coo3x <- coo$x[match(as_factor(geo$EPA2), adm$ADMIN3)]
coo3y <- coo$y[match(as_factor(geo$EPA2), adm$ADMIN3)]
coo2x <- cooDisx[match(geo$District, names(cooDisx))]
coo2y <- cooDisy[match(geo$District, names(cooDisy))]

hhinfo$gps_lon <- ifelse(is.na(coo3x), coo2x, coo3x)
hhinfo$gps_lat <- ifelse(is.na(coo3y), coo2y, coo3y)

p <- vect(cbind(hhinfo$gps_lon, hhinfo$gps_lat))
admp <- extract(adm, p)

hhinfo$region <- as_factor(geo$region2) #admp$ADMIN1
hhinfo$adm1 <- geo$District #admp$ADMIN2
hhinfo$adm1 <- as_factor(geo$EPA2) #admp$ADMIN3
hhinfo$adm3 <- admp$ADMIN3
hhinfo$large_region <- "Eastern Africa"

# Farming systems from Dixon
fs <- rast("../Data/GIS/cell5m_Agroecology_FarmingSystems_FS_2012_TX.tif")
activeCat(fs) <- 2 #to get the full name of AGROECOLOG
# plot(fs)
# points(p)
fsp <- terra::extract(fs, p)
#replace "" by NA
fsp$AGROECOLOG[fsp$AGROECOLOG==""] <- NA
table(fsp$AGROECOLOG, useNA="ifany")
hhinfo$farming_system <- as.character(fsp$AGROECOLOG)

# Population
# pop <- rast("../Data/GIS/gpw_v4_population_density_rev11_2020_30s.tif")
# popp <- terra::extract(pop, p)$gpw_v4_population_density_rev11_2020_30_sec
hhinfo$population_2020 <- NA



# Travel time to cities
# ttc <- rast("../Data/GIS/travel_time_to_cities_u6.tif")
# ttcp <- terra::extract(ttc, p)$travel_time_to_cities_1
hhinfo$travel_time_cities <- NA

#koeppen
kg <- rast("../Data/GIS/Beck_KG_V1_present_0p0083.tif")

kgp <- terra::extract(kg, p)$Beck_KG_V1_present_0p0083
conv_kgp <- c(
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

#remove household on sea
kgp[kgp==0] <- NA
#plot(kg, col=c("black", "red"), breaks=c(-0.5,0.5,30.5))
hhinfo$koeppen <- conv_kgp[kgp]


# 7. Post processing ---------------------
# add summary of farm production
crop$name <- bestname(crop$name, names(conv_energy), warn = TRUE)
lstk$name <- bestname(lstk$name, names(conv_tlu), warn = TRUE)
lstk_prod$name <- bestname(lstk_prod$name, names(conv_tlu), warn = TRUE)
lstk_prod$prod <- bestname(lstk_prod$prod, names(conv_energy), warn = TRUE)

# create a farmhousehold object
hhdb_simlesa_mwi2013 <- farmhousehold(
  "crop"=crop, 
  "lstk"=lstk, 
  "lstk_prod"=lstk_prod, 
  "hhinfo"=hhinfo,
  "conv_tlu"=conv_tlu, 
  "conv_energy"=conv_energy
)

# calculate crop and livestock characteristics
hhdb_simlesa_mwi2013<- update_farmhousehold(hhdb_simlesa_mwi2013)


# 8. Save dataset ---------------------
saveRDS(hhdb_simlesa_mwi2013,
     file="../Data/Processed/HHDB_SIMLESA_MWI2013.rds", compress="xz")
