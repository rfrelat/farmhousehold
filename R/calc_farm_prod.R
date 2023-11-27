calc_farm_prod <- function(crop, lstk, lstk_prod, hhinfo,
                                  conv_tlu, conv_energy){
  if (any(!crop$name%in%names(conv_energy))) {
    miss <- crop$name[!crop$name%in%names(conv_energy)]
    #only the duplicated names
    miss <- sort(unique(miss[!is.na(miss)&duplicated(miss)]))
    if(length(miss)>0){
      warning(paste("Missing crop energy conversion:", paste(miss, collapse = ", ")))
    }
  }
  if (any(!lstk_prod$prod%in%names(conv_energy))) {
    miss <- lstk_prod$prod[!lstk_prod$prod%in%names(conv_energy)]
    #only the duplicated names
    miss <- sort(unique(miss[!is.na(miss)&duplicated(miss)]))
    if(length(miss)>0){
      warning(paste("Missing livestock product in energy conversion:", paste(miss, collapse = ", ")))
    }
  }

  if (any(!lstk$name%in%names(conv_tlu))) {
    miss <- lstk$name[!lstk$name%in%names(conv_tlu)]
    #only the duplicated names
    miss <- sort(unique(miss[!is.na(miss)&duplicated(miss)]))
    if(length(miss)>0){
      warning(paste("Missing livestock TLU conversion:", paste(miss, collapse = ", ")))
    }
  }

  new <- c()
  #calculate crop summary
  land_cultivated_ha <- tapply(crop$land_area_ha, crop$hhid, sum, na.rm=TRUE)
  cropdiv <- tapply(crop$name, crop$hhid, lunique)
  cropname <- tapply(crop$name, crop$hhid, punique)
  cropharvest <- tapply(NAto0(crop$harvest_kg), crop$hhid, sum, na.rm=TRUE)
  cropsold <- tapply(NAto0(crop$sold_kg), crop$hhid, sum, na.rm=TRUE)
  cropincdiv <- tapply(NAto0(crop$sold_kg)>0, crop$hhid, sum, na.rm=TRUE)
  cropincome <- tapply(NAto0(crop$income_lcu), crop$hhid, sum, na.rm=TRUE)
  cropyield <- ifelse(land_cultivated_ha>0,
                      cropharvest/land_cultivated_ha, NA)
  croppercsold <- ifelse(cropharvest>0,
                         cropsold/cropharvest, NA)

  cropkcal <- conv_energy[crop$name]
  cropcons <- tapply(NAto0(crop$consumed_kg)*cropkcal, crop$hhid, sum, na.rm=TRUE)

  #estimate the value of crop consumed
  #estimate crop price per project
  isold <- NAto0(crop$income_lcu)>0 & NAto0(crop$sold_kg)>0
  crop$id_form <- hhinfo$id_form[match(crop$hhid, hhinfo$hhid)]
  crop$name <- as.factor(crop$name)
  cropprice <- tapply(crop$income_lcu[isold]/crop$sold_kg[isold],
                      list(crop$name[isold],crop$id_form[isold]), median)
  #fill with Q1 of prices per project if NA
  q1price <- apply(cropprice, 2, quantile, probs=0.25, na.rm=TRUE)
  fullprice <- as.matrix(apply(cropprice, 1, function(x) ifelse(is.na(x), q1price, x)))
  if(length(q1price)>1){fullprice <- t(fullprice)}
  dimnames(fullprice) <- dimnames(cropprice)
  #value per kg
  crop$price <- fullprice[cbind(match(crop$name, rownames(fullprice)),match(crop$id_form, colnames(fullprice)))]
  crop$notsold <- ifelse(crop$harvest_kg>=crop$sold_kg,
                         crop$harvest_kg-crop$sold_kg, 0)
  cropvalue <- tapply(crop$notsold*crop$price, crop$hhid, sum, na.rm=TRUE)

  #save crop summary into hhinfo
  mcid <- match(hhinfo$hhid, names(land_cultivated_ha))
  new$land_cultivated_ha <- NAto0(land_cultivated_ha[mcid])
  new$crop_div <- NAto0(cropdiv[mcid])
  new$crop_name <- cropname[mcid]
  new$crop_harvest_kg <- NAto0(cropharvest[mcid])
  new$crop_yield_kg_per_ha <- cropyield[mcid]

  new$crop_sold_kg <- NAto0(cropsold[mcid])
  new$crop_sold_perc <- NAto0(croppercsold[mcid])*100
  new$crop_income_div <- NAto0(cropincdiv[mcid])
  new$crop_income_lcu <- NAto0(cropincome[mcid])
  new$crop_value_lcu <- NAto0(cropvalue[mcid])
  new$crop_consumed_kcal <- NAto0(cropcons[mcid])
  # yield in term of value (income + value not sold)
  new$crop_yield_value_lcu_per_ha <- ifelse(new$land_cultivated_ha>0,
      (new$crop_income_lcu+new$crop_value_lcu)/new$land_cultivated_ha, NA)


  #calculate livestock summary
  livestock_tlu <- tapply(lstk$n*conv_tlu[lstk$name], lstk$hhid, sum, na.rm=TRUE)
  lstkdiv <- tapply(lstk$name, lstk$hhid, lunique)
  lstkname <- tapply(lstk$name, lstk$hhid, punique)

  lstkharvest <- tapply(NAto0(lstk_prod$harvest_kg), lstk_prod$hhid, sum, na.rm=TRUE)
  lstksold <- tapply(NAto0(lstk_prod$sold_kg), lstk_prod$hhid, sum, na.rm=TRUE)
  lstkincdiv <- tapply(NAto0(lstk_prod$sold_kg)>0, lstk_prod$hhid, sum, na.rm=TRUE)
  lstkincome <- tapply(NAto0(lstk_prod$income_lcu), lstk_prod$hhid, sum, na.rm=TRUE)
  lstkpercsold <- ifelse(lstkharvest>0,
                         lstksold/lstkharvest, NA)
  lstkkcal <- conv_energy[lstk_prod$prod]
  lstkcons <- tapply(NAto0(lstk_prod$consumed_kg)*lstkkcal, lstk_prod$hhid, sum, na.rm=TRUE)

  #estimate the value of livestock product not sold
  isold <- lstk_prod$income_lcu>0 & lstk_prod$sold_kg>0
  lstk_prod$id_form <- hhinfo$id_form[match(lstk_prod$hhid, hhinfo$hhid)]
  lstk_prod$prod <- as.factor(lstk_prod$prod)
  lstkprice <- tapply(lstk_prod$income_lcu[isold]/lstk_prod$sold_kg[isold],
                      list(lstk_prod$prod[isold],lstk_prod$id_form[isold]), median)
  #fill with Q1 of prices per project if NA
  q1price <- apply(lstkprice, 2, quantile, probs=0.25, na.rm=TRUE)
  fullprice <- as.matrix(apply(lstkprice, 1, function(x) ifelse(is.na(x), q1price, x)))
  if(length(q1price)>1){fullprice <- t(fullprice)}
  dimnames(fullprice) <- dimnames(lstkprice)
  #value per kg
  lstk_prod$price <- fullprice[cbind(match(lstk_prod$prod, rownames(fullprice)),match(lstk_prod$id_form, colnames(fullprice)))]
  lstk_prod$notsold <- ifelse(lstk_prod$harvest_kg>=lstk_prod$sold_kg,
                              lstk_prod$harvest_kg-lstk_prod$sold_kg, 0)
  lstkvalue <- tapply(lstk_prod$notsold*lstk_prod$price, lstk_prod$hhid, sum, na.rm=TRUE)


  mcid <- match(hhinfo$hhid, names(livestock_tlu))
  new$livestock_tlu <- NAto0(livestock_tlu[mcid])
  new$lstk_div <- NAto0(lstkdiv[mcid])
  new$lstk_name <- lstkname[mcid]

  mcid <- match(hhinfo$hhid, names(lstkharvest))
  new$lstk_harvest_kg <- NAto0(lstkharvest[mcid])
  new$lstk_yield_kg_per_tlu <- ifelse(new$livestock_tlu>0,
                                      new$lstk_harvest_kg/new$livestock_tlu, NA)
  new$lstk_sold_kg <- NAto0(lstksold[mcid])
  new$lstk_sold_perc <- NAto0(lstkpercsold[mcid])*100
  new$lstk_income_div <- NAto0(lstkincdiv[mcid])
  new$lstk_income_lcu <- NAto0(lstkincome[mcid])
  new$lstk_value_lcu <- NAto0(lstkvalue[mcid])
  new$lstk_consumed_kcal <- NAto0(lstkcons[mcid])

  # yield in term of value (income + value not sold)
  new$lstk_yield_value_lcu_per_tlu <- ifelse(new$livestock_tlu>0,
                                            (new$lstk_income_lcu+new$lstk_value_lcu)/new$livestock_tlu, NA)

  new <- as.data.frame(new)
  new$farm_div <- new$crop_div + new$lstk_div
  new$farm_harvest_kg <- new$crop_harvest_kg+new$lstk_harvest_kg
  new$farm_sold_perc_kg <- ifelse(new$farm_harvest_kg>0,
                                  (new$crop_sold_kg+new$lstk_sold_kg)/new$farm_harvest_kg*100,
                                  NA)
  new$farm_income_div <- new$crop_income_div + new$lstk_income_div

  new$farm_income_lcu <-  new$crop_income_lcu + new$lstk_income_lcu
  new$farm_consumed_kcal <- new$crop_consumed_kcal + new$lstk_consumed_kcal

  new$income_lcu <-  new$farm_income_lcu + NAto0(hhinfo$off_farm_lcu)
  new$income_usd <- new$income_lcu / hhinfo$currency_conversion_lcu_to_ppp
  new$income_per_person_per_day_usd <- ifelse(hhinfo$hh_size_members>0, new$income_usd/ hhinfo$hh_size_members/365, NA)
  new$off_farm_perc <- ifelse(new$income_lcu>0, NAto0(hhinfo$off_farm_lcu) / new$income_lcu*100,0)
  new$pop_pressure_mae_per_ha <- ifelse(NAto0(new$land_cultivated_ha)>0,NAto0(hhinfo$hh_size_mae)/new$land_cultivated_ha,NA)
  new$lstk_pressure_tlu_per_ha <- ifelse(NAto0(new$land_cultivated_ha)>0,NAto0(new$livestock_tlu)/new$land_cultivated_ha,NA)

  hhinfo <- cbind(hhinfo[,!names(hhinfo)%in%names(new)], new)
  return(hhinfo)
}
