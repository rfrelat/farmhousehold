# Transform LSMS Malawi 2019 dataset in farmhousehold format
# Last update: 27/05/2024

# Make sure to use the last update of the farmhousehold package
# devtools::install_github("rfrelat/farmhousehold")
library(farmhousehold)

library(dplyr) # for fast row binding
library(terra) # for spatial information

# Load the parameters
source("Param_LSMS_MWI2019.R")
conv_fun

# 1. Load data ----------------------------------
# geographic variable
geo <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/householdgeovariables_ihs5.csv")
# Crop
# in rainy season
tagc <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_c.csv") #plot rainy season
tagg <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_g.csv") #crop rainy season
tagi <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_i.csv") #use rainy season
#ag_mod_d contains information on fertilizers

#in dry season
tagj <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_j.csv") #plot dry season
tagm <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_m.csv") #crop dry season
tago <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_o.csv") #use dry season
#ag_mod_i contains information on fertilizers

#perennial crops
tagp <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_p.csv") #tree
tagq <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_q.csv") #use

# Livestock 
tagr1 <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_r1.csv")
tags <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/ag_mod_s.csv")

# HDDS
thhg <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/HH_MOD_G2.csv")

# household composition
thhb <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/HH_MOD_B.csv")

# food secure in last 12 months
thhh <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/HH_MOD_H.csv")

# off farm
thhe <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/HH_MOD_E.csv")
thhn2 <- read.csv("../Data/MWI_2019_IHS-V_v05_M_CSV/HH_MOD_N2.csv")

# 2. Create crop table --------------------------
# Rainy season
# could still add information on :
# intercrop, crop_use, crop_residue_use

# information per plot: join G, C 
rainy <- left_join(tagg, tagc,by = join_by(case_id,HHID, gardenid, plotid))

# land area
# transform gps record in acre to hectare
hagps <- rainy$ag_c04c*conv_ha["ACRE"]
# convert farmer estimation to hectare 
hafest <- rainy$ag_c04a*conv_fun(conv_ha, rainy$ag_c04b, rainy$ag_c04b_oth)
# use gps if recorded, else farmer estimation
rainy$landha <- ifelse(is.na(hagps), hafest, hagps)
# add factor for land area
rainy$percland <- ifelse(rainy$ag_g03=="", 1, 
                        conv_pland[rainy$ag_g03])

# so far, divided area among crop on the plot
rainy$cid <- paste(rainy$case_id, rainy$HHID, rainy$gardenid, rainy$plotid, sep="_")
mha <- tapply(rainy$landha, rainy$cid, mean) #plot size
# table(tapply(rainy$landha, rainy$cid, sd)) #==0
sha <- tapply(rainy$landha*rainy$percland, rainy$cid, sum) #plot used
percuse <- ifelse(sha>mha, mha/sha, 1)
rainy$percuse <- percuse[match(rainy$cid, names(percuse))]
rainy$land_area_ha <- rainy$landha*rainy$percland*rainy$percuse

# harvest
# replace other units
ug13b <- conv_fun(conv_kg, rainy$ag_g13b, rainy$ag_g13b_oth)
# convert into kg
rainy$harvest_kg <- rainy$ag_g13a * ug13b

# simplify per crop and household
rainy$cropid <- paste(rainy$case_id, rainy$HHID, rainy$crop_code, sep="_")
ha <- tapply(rainy$land_area_ha, rainy$cropid, sum, na.rm=TRUE)
kg <- tapply(rainy$harvest_kg, rainy$cropid, sum, na.rm=TRUE)
cropid <- strsplit(names(ha), "_")
rainy_crop <- data.frame(
  case_id=as.numeric(sapply(cropid, function(x)x[[1]])),
  HHID=sapply(cropid, function(x)x[[2]]),
  crop_code=sapply(cropid, function(x) ifelse(length(x)>2, x[[3]], "")),
  land_area_ha=as.numeric(ha),
  harvest_kg=as.numeric(kg)
)

# add crop use
rainy_crop <- left_join(rainy_crop, tagi, 
                        by = join_by(case_id, HHID, crop_code))

# sold
ui02 <- conv_fun(conv_kg, rainy_crop$ag_i02b, rainy_crop$ag_i02b_oth)
rainy_crop$sold_kg <- NAto0(rainy_crop$ag_i02a * ui02)

rainy_crop$income_lcu <- NAto0(rainy_crop$ag_i03)

# consumed
ui30 <- conv_fun(conv_kg, rainy_crop$ag_i30b, rainy_crop$ag_i30b_oth)
rainy_crop$consumed_kg <- NAto0(rainy_crop$ag_i30a * ui30)

# other use
ui31 <- conv_fun(conv_kg, rainy_crop$ag_i31b, rainy_crop$ag_i31b_oth)
rainy_crop$gift_kg <- NAto0(rainy_crop$ag_i31a * ui31)
ui32 <- conv_fun(conv_kg, rainy_crop$ag_i32b, rainy_crop$ag_i32b_oth)
rainy_crop$debt_kg <- NAto0(rainy_crop$ag_i32a * ui32)
ui33 <- conv_fun(conv_kg, rainy_crop$ag_i33b, rainy_crop$ag_i33b_oth)
rainy_crop$feed_kg <- NAto0(rainy_crop$ag_i33a * ui33)
ui34 <- conv_fun(conv_kg, rainy_crop$ag_i34b, rainy_crop$ag_i34b_oth)
rainy_crop$byprod_kg <- NAto0(rainy_crop$ag_i34a * ui34)
ui35 <- conv_fun(conv_kg, rainy_crop$ag_i35b, rainy_crop$ag_i35b_oth)
rainy_crop$seed_kg <- NAto0(rainy_crop$ag_i35a * ui35)
ui36 <- conv_fun(conv_kg, rainy_crop$ag_i36b, rainy_crop$ag_i36b_oth)
rainy_crop$lost_kg <- NAto0(rainy_crop$ag_i36a * ui36)

rainy_crop$use_list <- ifelse(rainy_crop$consumed_kg>0, "consume", "")
rainy_crop$use_list <- paste0(rainy_crop$use_list, 
                            ifelse(rainy_crop$sold_kg>0, " sell", ""))
rainy_crop$use_list <- paste0(rainy_crop$use_list, 
                            ifelse(rainy_crop$gift_kg>0, " gift", ""))
rainy_crop$use_list <- paste0(rainy_crop$use_list, 
                            ifelse(rainy_crop$debt_kg>0, " payment", ""))
rainy_crop$use_list <- paste0(rainy_crop$use_list, 
                            ifelse(rainy_crop$feed_kg>0, " feed", ""))
table(rainy_crop$use_list)


sel <- c("case_id", "HHID", "crop_code", "land_area_ha", 
         "harvest_kg", "consumed_kg", 
         "sold_kg","income_lcu", "use_list") 
rainy_crop <- rainy_crop[,sel]



# dry season
# information per plot: join K and M 
dry <- left_join(tagm, tagj, by = join_by(case_id,HHID, gardenid, plotid))

# land area
uj05 <- conv_fun(conv_ha, dry$ag_j05b, dry$ag_j05b_oth)
dry$landha <- dry$ag_j05a*uj05

# add factor for land area
dry$percland <- ifelse(dry$ag_m03=="", 1, 
                         conv_pland[dry$ag_m03])
# not sure what to do with intercropping
# table(dry$ag_m01)
# so far, divided area among crop on the plot
dry$cid <- paste(dry$case_id, dry$HHID, dry$gardenid, dry$plotid, sep="_")
mha <- tapply(dry$landha, dry$cid, mean) #plot size
# table(tapply(dry$landha, dry$cid, sd)) #==0
sha <- tapply(dry$landha*dry$percland, dry$cid, sum) #plot used
percuse <- ifelse(sha>mha, mha/sha, 1)
dry$percuse <- percuse[match(dry$cid, names(percuse))]
dry$land_area_ha <- dry$landha*dry$percland*dry$percuse
# boxplot(dry$percuse~dry$ag_m01)

# harvest convert into kg
um11 <- conv_fun(conv_kg, dry$ag_m11b, dry$ag_m11b_oth)
dry$harvest_kg <- dry$ag_m11a * um11

# simplify per crop and household
dry$cropid <- paste(dry$case_id, dry$HHID, dry$crop_code, sep="_")
ha <- tapply(dry$land_area_ha, dry$cropid, sum, na.rm=TRUE)
kg <- tapply(dry$harvest_kg, dry$cropid, sum, na.rm=TRUE)
cropid <- strsplit(names(ha), "_")
dry_crop <- data.frame(
  case_id=as.numeric(sapply(cropid, function(x)x[[1]])),
  HHID=sapply(cropid, function(x)x[[2]]),
  crop_code=sapply(cropid, function(x) ifelse(length(x)>2, x[[3]], "")),
  land_area_ha=as.numeric(ha),
  harvest_kg=as.numeric(kg)
)

# add crop use
dry_crop <- left_join(dry_crop, tago, 
                      by = join_by(case_id, HHID, crop_code))

# sold
uo02 <- conv_fun(conv_kg, dry_crop$ag_o02b)
dry_crop$sold_kg <- NAto0(dry_crop$ag_o02a * uo02)

dry_crop$income_lcu <- NAto0(dry_crop$ag_o03)

uo31 <- conv_fun(conv_kg, dry_crop$ag_o31b, dry_crop$ag_o31b_oth)
dry_crop$gift_kg <- NAto0(dry_crop$ag_o31a * uo31)
uo32 <- conv_fun(conv_kg, dry_crop$ag_o32b, dry_crop$ag_o32b_oth)
dry_crop$debt_kg <- NAto0(dry_crop$ag_o32a * uo32)
uo33 <- conv_fun(conv_kg, dry_crop$ag_o33b, dry_crop$ag_o33b_oth)
dry_crop$feed_kg <- NAto0(dry_crop$ag_o33a * uo33)
uo34<- conv_fun(conv_kg, dry_crop$ag_o34b, dry_crop$ag_o34b_oth)
dry_crop$byprod_kg <- NAto0(dry_crop$ag_o34a * uo34)
uo35<- conv_fun(conv_kg, dry_crop$ag_o35b, dry_crop$ag_o35b_oth)
dry_crop$seed_kg <- NAto0(dry_crop$ag_o35a * uo35)
uo36 <- conv_fun(conv_kg, dry_crop$ag_o36b, dry_crop$ag_o36b_oth)
dry_crop$lost_kg <- NAto0(dry_crop$ag_o36a * uo36)

# consumed - no data :(
dry_crop$use_kg <- dry_crop$sold_kg+dry_crop$debt_kg+
  dry_crop$byprod_kg+ dry_crop$feed_kg + dry_crop$seed_kg+dry_crop$lost_kg

dry_crop$consumed_kg <- ifelse(dry_crop$use_kg<dry_crop$harvest_kg,
                                        dry_crop$harvest_kg-dry_crop$use_kg,0)

dry_crop$use_list <- ifelse(dry_crop$consumed_kg>0, "consume", "")
dry_crop$use_list <- paste0(dry_crop$use_list, 
                           ifelse(dry_crop$sold_kg>0, " sell", ""))
dry_crop$use_list <- paste0(dry_crop$use_list, 
                           ifelse(dry_crop$gift_kg>0, " gift", ""))
dry_crop$use_list <- paste0(dry_crop$use_list, 
                           ifelse(dry_crop$debt_kg>0, " payment", ""))
dry_crop$use_list <- paste0(dry_crop$use_list, 
                           ifelse(dry_crop$feed_kg>0, " feed", ""))


sel <- c("case_id", "HHID", "crop_code", "land_area_ha", 
         "harvest_kg", "consumed_kg", 
         "sold_kg","income_lcu", "use_list") 
dry_crop <- dry_crop[,sel]


# Perenial crop
tagp$ag_p0c <- ifelse(tagp$ag_p0c=="OTHER (SPECIFY)",
                      tagp$ag_p0c_oth, tagp$ag_p0c)

# create a unique id per crop and household
tagp$cropid <- paste(tagp$case_id, tagp$HHID, tagp$ag_p0c, sep="_")
# table(duplicated(tagp$cropid)) #some duplicates

# convert into kg
up09 <- conv_fun(conv_kg, tagp$ag_p09b, tagp$ag_p09b_oth)
tagp$harvest_kg <- tagp$ag_p09a * up09

# convert to ha
up02 <- conv_fun(conv_ha, tagp$ag_p02b, tagp$ag_p02b_oth)
tagp$land_area_ha <- tagp$ag_p02a*up02

# simplify per crop and household
ha <- tapply(tagp$land_area_ha, tagp$cropid, sum, na.rm=TRUE)
kg <- tapply(tagp$harvest_kg, tagp$cropid, sum, na.rm=TRUE)

cropid <- strsplit(names(ha), "_")
tree_crop <- data.frame(
  case_id=as.numeric(sapply(cropid, function(x)x[[1]])),
  HHID=sapply(cropid, function(x)x[[2]]),
  crop_code=sapply(cropid, function(x) ifelse(length(x)>2, x[[3]], "")),
  land_area_ha=as.numeric(ha),
  harvest_kg=as.numeric(kg)
)

# add crop use
tree_crop <- left_join(tree_crop, tagq, 
                       by = join_by(case_id, HHID, crop_code))

uq02 <- conv_fun(conv_kg, tree_crop$ag_q02b, tree_crop$ag_q02b_oth)
tree_crop$sold_kg <- NAto0(tree_crop$ag_q02a * uq02)
tree_crop$income_lcu <- NAto0(tree_crop$ag_q03)

uq31 <- conv_fun(conv_kg, tree_crop$ag_q31b, tree_crop$ag_q31b_oth)
tree_crop$gift_kg <- NAto0(tree_crop$ag_q31a * uq31)
uq32 <- conv_fun(conv_kg, tree_crop$ag_q32b, tree_crop$ag_q32b_oth)
tree_crop$debt_kg <- NAto0(tree_crop$ag_q32a * uq32)
uq33 <- conv_fun(conv_kg, tree_crop$ag_q33b, tree_crop$ag_q33b_oth)
tree_crop$byprod_kg <- NAto0(tree_crop$ag_q33a * uq33)
uq34 <- conv_fun(conv_kg, tree_crop$ag_q34b, tree_crop$ag_q34b_oth)
tree_crop$seed_kg <- NAto0(tree_crop$ag_q34a * uq34)
uq35<- conv_fun(conv_kg, tree_crop$ag_q35b, tree_crop$ag_q35b_oth)
tree_crop$lost_kg <- NAto0(tree_crop$ag_q35a * uq35)

# consumed - no data :(
tree_crop$use_kg <- tree_crop$sold_kg + tree_crop$debt_kg + 
  tree_crop$gift_kg + tree_crop$byprod_kg+ tree_crop$seed_kg+tree_crop$lost_kg

tree_crop$consumed_kg <- ifelse(tree_crop$use_kg<tree_crop$harvest_kg,
                                tree_crop$harvest_kg-tree_crop$use_kg,0)

tree_crop$use_list <- ifelse(tree_crop$consumed_kg>0, "consume", "")
tree_crop$use_list <- paste0(tree_crop$use_list, 
                            ifelse(tree_crop$sold_kg>0, " sell", ""))
tree_crop$use_list <- paste0(tree_crop$use_list, 
                            ifelse(tree_crop$gift_kg>0, " gift", ""))
tree_crop$use_list <- paste0(tree_crop$use_list, 
                            ifelse(tree_crop$debt_kg>0, " payment", ""))
tree_crop$use_list <- paste0(tree_crop$use_list, 
                            ifelse(tree_crop$seed_kg>0, " seed", ""))
table(tree_crop$use_list) #448 gift only ?!?

sel <- c("case_id", "HHID", "crop_code", "land_area_ha", 
         "harvest_kg", "consumed_kg", 
         "sold_kg","income_lcu", "use_list") 
tree_crop <- tree_crop[,sel]


crop <- rbind(rainy_crop, dry_crop, tree_crop)

# a. which total is most probable? based on yield
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

#simplify crop name
crop$crop_code[grep("MAIZE", crop$crop_code)] <- "MAIZE"
crop$crop_code[grep("GROUNDNUT", crop$crop_code)] <- "GROUNDNUT"
crop$crop_code[grep("RICE", crop$crop_code)] <- "RICE"
crop$crop_code[grep("TOBACCO", crop$crop_code)] <- "TOBACCO"

#create hhid
crop$hhid <- paste(prefix, crop$case_id, sep="_")

# set with standard name
crop$name <- crop$crop_code
sel <- c("hhid", "name", "land_area_ha", 
         "harvest_kg", "consumed_kg", 
         "sold_kg","income_lcu", "use_list")
crop <- crop[,sel]


# 3. Create livestock table ---------------------
# remove livestock with NA
tagr1 <- tagr1[!is.na(tagr1$ag_r02),]

tagr1$ag_r0a <- ifelse(tagr1$ag_r0a=="OTHER (SPECIFY)",
                       tagr1$ag_r01_oth, tagr1$ag_r0a)


sel <- c("case_id", "HHID", "ag_r0a", "ag_r02")
lstk <- tagr1[,sel]
names(lstk)[3:4] <- c("name", "n")

# Simplify name of livestock
poul <- c("LOCAL-HEN", "LOCAL-COCK", "CHICKEN-LAYER/CHICKEN-BROILER", "CHICKS")
lstk$name[lstk$name%in%poul] <- "POULTRY"

cattle <- c("COW", "BULL", "OX", "CALF", "STEER/HEIFER", "IMPROVED")
lstk$name[lstk$name%in%cattle] <- "CATTLE"

lstk$name[grep("BEE", lstk$name)] <- "BEEKEEPING"

rabbit <- c("HARE", "HARE AND MBIRA", "MBIRA,AND HARE", "MBILA")
lstk$name[lstk$name%in%rabbit] <- "RABBIT"

#create hhid
lstk$hhid <- paste(prefix, lstk$case_id, sep="_")

sel <- c("hhid", "name", "n")
lstk <- lstk[,sel]

lstk$n[lstk$n>10000] <- NA
# 20000 goats
# 150000 cattle
#lstk_sell <- tagr1$ag_r17
#lstk_meat <- tagr1$ag_r19

# 4. Create livestock production table ----------
#create hhid
tags$hhid <- paste(prefix, tags$case_id, sep="_")

# keep only the production
tags <- tags[tags$ag_s01=="Yes",]

us03 <- conv_fun(conv_kg, tags$ag_s03b, tags$ag_s03b_oth)
produce_kg <- as.numeric(tags$ag_s03a*NAto1(us03))
produce_kg <- NAto0(produce_kg)*NAto0(tags$ag_s02)

us05 <- conv_fun(conv_kg, tags$ag_s05b, tags$ag_s05b_oth)
sell_kg <- as.numeric(tags$ag_s05a*NAto1(us05))

income_lcu <- NAto0(tags$ag_s06)

us09 <- conv_fun(conv_kg, tags$ag_s09b, tags$ag_s09b_oth)
consume_kg <- as.numeric(tags$ag_s09a*NAto1(us09))

us10 <- conv_fun(conv_kg, tags$ag_s10b, tags$ag_s10b_oth)
gift_kg <- as.numeric(tags$ag_s10a*NAto1(us10))

us11 <- conv_fun(conv_kg, tags$ag_s11b, tags$ag_s11b_oth)
debt_kg <- as.numeric(tags$ag_s11a*NAto1(us11))

# check for consistency
sumprod <- NAto0(sell_kg)+NAto0(consume_kg)+NAto0(gift_kg)+NAto0(debt_kg)

#make sure production is >0
produce_kg <- ifelse(NAto0(produce_kg)==0, sumprod, produce_kg)
#take the lowest of the highest value
produce_kg <- ifelse(produce_kg>10000 & produce_kg>sumprod, sumprod, produce_kg)
#take the highest of the lowest value
produce_kg <- ifelse(produce_kg<1000 & produce_kg<sumprod, sumprod, produce_kg)

# make sure the sum = the production
# boxplot(produce_kg/ifelse(sumprod==0, 1, sumprod))
sell_kg <- sell_kg*produce_kg/ifelse(sumprod==0, 1, sumprod)
consume_kg <- consume_kg*produce_kg/ifelse(sumprod==0, 1, sumprod)
gift_kg <- gift_kg*produce_kg/ifelse(sumprod==0, 1, sumprod)
debt_kg <- debt_kg*produce_kg/ifelse(sumprod==0, 1, sumprod)

lstk_use <- ifelse(NAto0(consume_kg)>0, "consume", "")
lstk_use <- paste0(lstk_use, 
                   ifelse(NAto0(sell_kg)>0, " sell", ""))
lstk_use <- paste0(lstk_use, 
                   ifelse(NAto0(gift_kg)>0, " gift", ""))
lstk_use <- paste0(lstk_use, 
                   ifelse(NAto0(debt_kg)>0, " payment", ""))

lstk_prod <- data.frame(
  "hhid"=tags$hhid,
  conv_lstk_prod[match(tags$ag_s0a, conv_lstk_prod$id),2:3],
  "harvest_kg"=produce_kg,
  "consumed_kg"=consume_kg, 
  "sold_kg"=sell_kg,
  "income_lcu"=income_lcu, 
  "use_list"=lstk_use
)

ntlu <- tapply(lstk$n*conv_tlu[lstk$name], list(lstk$hhid,lstk$name), sum, na.rm=TRUE)
maxlstkperhh <- colnames(ntlu)[apply(ntlu,1,which.max)]
maxlstk <- maxlstkperhh[match(lstk_prod$hhid, row.names(ntlu))]
lstk_prod$name <- ifelse(is.na(lstk_prod$name), maxlstk, lstk_prod$name)

whole <- data.frame(
  lstk[c("hhid", "name")],
  "prod"="whole",
  "harvest_kg"=conv_tlu[lstk$name]*250*tagr1$ag_r16,
  "consumed_kg"=0,
  "sold_kg"=conv_tlu[lstk$name]*250*tagr1$ag_r16,
  "income_lcu"=tagr1$ag_r17,
  "use_list"="sell"
)
keep <- NAto0(whole$harvest_kg>0) |NAto0(whole$income_lcu>0)
whole <- whole[keep,]

lstk_prod <- rbind(lstk_prod, whole)


# 5. Create household table ---------------------
# Merge geographic information and diet
hhinfo <- full_join(geo, thhg, by = join_by(case_id))

# create hhid
hhinfo$hhid <- paste(prefix, hhinfo$case_id, sep="_")
hhinfo$id_form<- prefix

# Get administrative information
adm <- vect("../Data/MalawiCS/MW_Admin3_2003.shp")

hhinfo$gps_lon <- hhinfo$ea_lon_mod
hhinfo$gps_lat <- hhinfo$ea_lat_mod

nogps <- hhinfo$gps_lon==0 | hhinfo$gps_lat ==0
hhinfo$gps_lon[nogps] <- NA
hhinfo$gps_lat[nogps] <- NA

p <- vect(cbind(hhinfo$gps_lon, hhinfo$gps_lat))
admp <- extract(adm, p)

hhinfo$adm1 <- admp$ADMIN1
hhinfo$adm2 <- admp$ADMIN2
hhinfo$adm3 <- admp$ADMIN3

# table(hhinfo$adm1) # 3 provinces
# table(hhinfo$adm2) # 28 XX
# table(hhinfo$adm3) #177 EPAs

hhinfo$country <- "Malawi"
hhinfo$year <- "2019"
hhinfo$large_region <- "Eastern Africa"

# calculate household size
hh_size_members <- tapply(thhb$PID, thhb$case_id, max)
hhinfo$hh_size_members <- hh_size_members[match(hhinfo$case_id, names(hh_size_members))]

ageC <- cut(thhb$hh_b05a, breaks = c(0,4,10,24,50, 150), include.lowest = TRUE)
convAG <- ifelse(thhb$hh_b03=="MALE",
                 conv_M[ageC], conv_F[ageC])
hh_size_mae <- tapply(convAG, thhb$case_id, sum)
hhinfo$hh_size_mae <- hh_size_mae[match(hhinfo$case_id, names(hh_size_mae))]


# Off farm income

#how many weeks worked in a year
nmonths <- ifelse(thhe$hh_e22>12, NA,thhe$hh_e22)
nweeks <- nmonths * ifelse(thhe$hh_e23>4, NA,thhe$hh_e23)
# how many weeks paid
cleantime <- as.numeric(ifelse(thhe$hh_e26a<clean_paid[thhe$hh_e26b], thhe$hh_e26a, NA))
paidweek <- cleantime*conv_paid[thhe$hh_e26b]
ratio <- ifelse(thhe$hh_e26b=="MONTH", nmonths/cleantime, nweeks/paidweek)
ratio <- ifelse(is.infinite(ratio) | ratio<1, 1, ratio)
off_farm_lcu <- thhe$hh_e25 * ratio

#off farm work 2
nmonths_2 <- ifelse(thhe$hh_e36>12, NA,thhe$hh_e36)
nweeks_2 <- nmonths_2 * ifelse(thhe$hh_e37>4, NA,thhe$hh_e37)
# how many weeks paid
cleantime_2 <- as.numeric(ifelse(thhe$hh_e40a<clean_paid[thhe$hh_e40b], thhe$hh_e40a, NA))
paidweek_2 <- cleantime_2*conv_paid[thhe$hh_e40b]
ratio_2 <- ifelse(thhe$hh_e40b=="MONTH", nmonths_2/cleantime_2, nweeks_2/paidweek_2)
ratio_2 <- ifelse(is.infinite(ratio_2) | ratio_2<1, 1, ratio_2)
off_farm_lcu_2 <- thhe$hh_e39 * ratio_2
off_farm <- NAto0(off_farm_lcu)+NAto0(off_farm_lcu_2)
#sum per household
off_job_lcu <- tapply(off_farm, thhe$case_id, sum, na.rm=TRUE)
# number of income sources
off_div_1 <- tapply(NAto0(off_farm_lcu)>0, thhe$case_id, sum, na.rm=TRUE)
off_div_2 <- tapply(NAto0(off_farm_lcu_2)>0, thhe$case_id, sum, na.rm=TRUE)

# enterprise
thhn2$hh_n14 <- ifelse(thhn2$hh_n14=="Other (Specify)", thhn2$hh_n14_oth, thhn2$hh_n14)
profit_hh <- thhn2$hh_n40*conv_profit[thhn2$hh_n14]
nmonth <- ifelse(is.na(thhn2$hh_n38), ifelse(is.na(thhn2$hh_n34), 12, 15), 9)
business <- profit_hh*nmonth
#per household
business_perhh <- tapply(business, thhn2$HHID, sum, na.rm=TRUE)
business_div <- tapply(business>0, thhn2$HHID, sum, na.rm=TRUE)
thhe$business_lcu <- business_perhh[match(thhe$HHID, names(business_perhh))]
thhe$business_div <- business_div[match(thhe$HHID, names(business_div))]
business_lcu <- tapply(thhe$business_lcu, thhe$case_id, mean, na.rm=TRUE)
business_div <- tapply(thhe$business_div, thhe$case_id, mean, na.rm=TRUE)

off_farm_lcu <- off_job_lcu + business_lcu
off_farm_div <- NAto0(off_div_1) + NAto0(off_div_2) + NAto0(business_div)
hhinfo$off_farm_lcu <- off_farm_lcu[match(hhinfo$case_id, names(off_farm_lcu))]
hhinfo$off_farm_div <- off_farm_div[match(hhinfo$case_id, names(off_farm_div))]


hhinfo$currency_conversion_lcu_to_ppp <- conv_lcu

#food security

# calculate hdds
dietC <- paste0("hh_g08", letters[1:10])
hhinfo$hdds_score <- rowSums(hhinfo[,dietC]>0, na.rm=TRUE)
# table(hhinfo$hdds, useNA="ifany")

dietBin <- apply(hhinfo[dietC]>0,2,as.numeric)
colnames(dietBin) <- labdiet
hhinfo <- cbind(hhinfo, dietBin)

# months with food shortag
nmonth <- ifelse(thhh$hh_h04=="YES", 
                 rowSums(thhh[,grep("hh_h05", names(thhh))]=="X"),
                 0)
wmonth <- apply(thhh[,grep("hh_h05", names(thhh))], 1, function(x){
  paste(unique(month.abb[c(4:12, 1:12,1:4)][x=="X"]), collapse = " ")
})
wmonth <- ifelse(nmonth==0, "", wmonth)
hhinfo$foodshortage_count <- nmonth[match(hhinfo$case_id, thhh$case_id)]
hhinfo$foodshortage_months <- wmonth[match(hhinfo$case_id,  thhh$case_id)]



sel <- c("hhid","id_form", "case_id", "country", "year",
         "gps_lat", "gps_lon", "adm1", "adm2", "adm3","large_region",
         "hh_size_members", "hh_size_mae",
         "hdds_score", labdiet, "off_farm_lcu", 
         "off_farm_div", "foodshortage_count", "foodshortage_months",
         "currency_conversion_lcu_to_ppp")
hhinfo <- hhinfo[,sel]


# 6. Add GIS data -------------------------------

p <- vect(cbind(hhinfo$gps_lon, hhinfo$gps_lat))

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
hhinfo$ssa_aez09 <- geo$ssa_aez09

# Population
pop <- rast("../Data/GIS/gpw_v4_population_density_rev11_2020_30s.tif")
popp <- terra::extract(pop, p)$gpw_v4_population_density_rev11_2020_30_sec
# boxplot(popp)
# Some NAs due to NAs in coordinates
# table(is.na(popp)) #1615 NAs
# plot(p, col=ifelse(is.na(popp), "red", "blue"))
# points(p[is.na(popp)], col="red")
# cbind(tab$gps_lon, tab$gps_lat)[is.na(popp),]
hhinfo$population_2020 <- popp
hhinfo$popdensity <- geo$popdensity


# Travel time to cities
ttc <- rast("../Data/GIS/travel_time_to_cities_u6.tif")
ttcp <- terra::extract(ttc, p)$travel_time_to_cities_1
# boxplot(ttcp)
# Some NAs due to NAs in coordinates
# table(is.na(ttcp)) #1615 NAs
hhinfo$travel_time_cities <- ttcp
hhinfo$dist_popcenter <- geo$dist_popcenter

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

# check the name of the crop and livestock and find best matching one
crop$name <- bestname(crop$name, names(conv_energy), warn = TRUE)
lstk$name <- bestname(lstk$name, names(conv_tlu), warn = TRUE)
lstk_prod$name <- bestname(lstk_prod$name, names(conv_tlu), warn = TRUE)
lstk_prod$prod <- bestname(lstk_prod$prod, names(conv_energy), warn = TRUE)

# calculate crop and livestock summary characteristics
hhinfo <- calc_farm_prod(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy)

# remove livestock listed without any animals
lstk <- lstk[lstk$n>0,]

# 8. Save dataset ---------------------
hhdb_lsms_mwi2019 <- farmhousehold(
  "crop"=crop, 
  "lstk"=lstk, 
  "lstk_prod"=lstk_prod, 
  "hhinfo"=hhinfo,
  "conv_tlu"=conv_tlu, 
  "conv_energy"=conv_energy
)

saveRDS(hhdb_lsms_mwi2019,
        file="../Data/Processed/HHDB_LSMS_MWI2019.rds", compress="xz")

