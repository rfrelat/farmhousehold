calc_ghg_emissions <- function(crop, lstk, lstk_prod, hhinfo,
                               conv_tlu, conv_energy){
  if (any(!crop$name%in%names(conv_energy))) {
    miss <- crop$name[!crop$name%in%names(conv_energy)]
    #only the duplicated names 
    miss <- sort(unique(miss[!is.na(miss)&duplicated(miss)]))
    if(length(miss)>0){
      warning(paste("Missing crop energy conversion for", paste(miss, collapse = ", ")))
    }
  }
  if (any(!lstk_prod$prod%in%names(conv_energy))) {
    miss <- lstk_prod$prod[!lstk_prod$prod%in%names(conv_energy)]
    #only the duplicated names 
    miss <- sort(unique(miss[!is.na(miss)&duplicated(miss)]))
    if(length(miss)>0){
      warning(paste("Missing livestock product in energy conversion", paste(miss, collapse = ", ")))
    }
  }
  
  new <- c()
  #calculate crop summary
  land_cultivated_ha <- tapply(crop$land_area_ha, crop$hhid, sum, na.rm=TRUE)
  cropdiv <- tapply(crop$name, crop$hhid, lunique)
  cropname <- tapply(crop$name, crop$hhid, punique)
  cropharvest <- tapply(NAto0(crop$harvest_kg_per_year), crop$hhid, sum, na.rm=TRUE)
  cropsold <- tapply(NAto0(crop$sold_kg_per_year), crop$hhid, sum, na.rm=TRUE)
  cropincdiv <- tapply(NAto0(crop$sold_kg_per_year)>0, crop$hhid, sum, na.rm=TRUE)
  cropincome <- tapply(NAto0(crop$income_lcu_per_year), crop$hhid, sum, na.rm=TRUE)
  cropyield <- ifelse(land_cultivated_ha>0, 
                      cropharvest/land_cultivated_ha, NA)
  croppercsold <- ifelse(cropharvest>0, 
                         cropsold/cropharvest, NA)

  cropkcal <- conv_energy[crop$name]
  cropcons <- tapply(NAto0(crop$consumed_kg_per_year)*cropkcal, crop$hhid, sum, na.rm=TRUE)
  
  #estimate the value of crop not sold
  #estimate crop price per project
  isold <- crop$income_lcu_per_year>0 & crop$sold_kg_per_year>0
  crop$id_form <- hhinfo$id_form[match(crop$hhid, hhinfo$hhid)]
  crop$name <- as.factor(crop$name)
  cropprice <- tapply(crop$income_lcu_per_year[isold]/crop$sold_kg_per_year[isold],
                      list(crop$name[isold],crop$id_form[isold]), median)
  #fill with Q1 of prices per project if NA
  q1price <- apply(cropprice, 2, quantile, probs=0.25, na.rm=TRUE)
  fullprice <- as.matrix(apply(cropprice, 1, function(x) ifelse(is.na(x), q1price, x)))
  if(length(q1price)>1){fullprice <- t(fullprice)}
  dimnames(fullprice) <- dimnames(cropprice)
  #value per kg
  crop$price <- fullprice[cbind(match(crop$name, rownames(fullprice)),match(crop$id_form, colnames(fullprice)))]
  crop$notsold <- ifelse(crop$harvest_kg_per_year>=crop$sold_kg_per_year,
                         crop$harvest_kg_per_year-crop$sold_kg_per_year, 0)
  cropvalue <- tapply(crop$notsold*crop$price, crop$hhid, sum, na.rm=TRUE)
  
  #save crop summary into hhinfo
  mcid <- match(hhinfo$hhid, names(land_cultivated_ha))
  new$land_cultivated_ha <- NAto0(land_cultivated_ha[mcid])
  new$crop_div <- NAto0(cropdiv[mcid])
  new$crop_name <- cropname[mcid]
  new$crop_harvest_kg_per_year <- NAto0(cropharvest[mcid])
  new$crop_yield_kg_per_year_per_ha <- NAto0(cropyield[mcid])
  new$crop_sold_kg_per_year <- NAto0(cropsold[mcid])
  new$crop_sold_perc <- NAto0(croppercsold[mcid])
  new$crop_income_div <- NAto0(cropincdiv[mcid])
  new$crop_income_lcu_per_year <- NAto0(cropincome[mcid])
  new$crop_value_lcu_per_year <- NAto0(cropvalue[mcid])
  new$crop_consumed_kcal_per_year <- NAto0(cropcons[mcid])
  
  #calculate livestock summary
  livestock_tlu <- tapply(lstk$n*conv_tlu[lstk$name], lstk$hhid, sum)
  lstkdiv <- tapply(lstk$name, lstk$hhid, lunique)
  lstkname <- tapply(lstk$name, lstk$hhid, punique)
  
  lstkharvest <- tapply(NAto0(lstk_prod$harvest_kg_per_year), lstk_prod$hhid, sum, na.rm=TRUE)
  lstksold <- tapply(NAto0(lstk_prod$sold_kg_per_year), lstk_prod$hhid, sum, na.rm=TRUE)
  lstkincdiv <- tapply(NAto0(lstk_prod$sold_kg_per_year)>0, lstk_prod$hhid, sum, na.rm=TRUE)
  lstkincome <- tapply(NAto0(lstk_prod$income_lcu_per_year), lstk_prod$hhid, sum, na.rm=TRUE)
  lstkpercsold <- ifelse(lstkharvest>0, 
                         lstksold/lstkharvest, NA)
  lstkkcal <- conv_energy[lstk_prod$prod]
  lstkcons <- tapply(NAto0(lstk_prod$consumed_kg_per_year)*lstkkcal, lstk_prod$hhid, sum, na.rm=TRUE)
  
  #estimate the value of livestock product not sold
  isold <- lstk_prod$income_lcu_per_year>0 & lstk_prod$sold_kg_per_year>0
  lstk_prod$id_form <- hhinfo$id_form[match(lstk_prod$hhid, hhinfo$hhid)]
  lstk_prod$prod <- as.factor(lstk_prod$prod)
  lstkprice <- tapply(lstk_prod$income_lcu_per_year[isold]/lstk_prod$sold_kg_per_year[isold],
                      list(lstk_prod$prod[isold],lstk_prod$id_form[isold]), median)
  #fill with Q1 of prices per project if NA
  q1price <- apply(lstkprice, 2, quantile, probs=0.25, na.rm=TRUE)
  fullprice <- as.matrix(apply(lstkprice, 1, function(x) ifelse(is.na(x), q1price, x)))
  if(length(q1price)>1){fullprice <- t(fullprice)}
  dimnames(fullprice) <- dimnames(lstkprice)
  #value per kg
  lstk_prod$price <- fullprice[cbind(match(lstk_prod$prod, rownames(fullprice)),match(lstk_prod$id_form, colnames(fullprice)))]
  lstk_prod$notsold <- ifelse(lstk_prod$harvest_kg_per_year>=lstk_prod$sold_kg_per_year,
                              lstk_prod$harvest_kg_per_year-lstk_prod$sold_kg_per_year, 0)
  lstkvalue <- tapply(lstk_prod$notsold*lstk_prod$price, lstk_prod$hhid, sum, na.rm=TRUE)
  
  
  mcid <- match(hhinfo$hhid, names(livestock_tlu))
  new$livestock_tlu <- NAto0(livestock_tlu[mcid])
  new$lstk_div <- NAto0(lstkdiv[mcid])
  new$lstk_name <- lstkname[mcid]
  mcid <- match(hhinfo$hhid, names(lstkharvest))
  new$lstk_harvest_kg_per_year <- NAto0(lstkharvest[mcid])
  new$lstk_sold_kg_per_year <- NAto0(lstksold[mcid])
  new$lstk_sold_perc <- NAto0(lstkpercsold[mcid])
  new$lstk_income_div <- NAto0(lstkincdiv[mcid])
  new$lstk_income_lcu_per_year <- NAto0(lstkincome[mcid])
  new$lstk_value_lcu_per_year <- NAto0(lstkvalue[mcid])
  new$lstk_consumed_kcal_per_year <- NAto0(lstkcons[mcid])
  
  
  new <- as.data.frame(new)
  new$farm_div <- new$crop_div + new$lstk_div
  new$farm_harvest_kg_per_year <- new$crop_harvest_kg_per_year+new$lstk_harvest_kg_per_year
  new$farm_sold_perc_kg <- ifelse(new$farm_harvest_kg_per_year>0, 
                                  (new$crop_sold_kg_per_year+new$lstk_sold_kg_per_year)/new$farm_harvest_kg_per_year,
                                  NA)
  new$farm_income_div <- new$crop_income_div + new$lstk_income_div
  
  new$farm_income_lcu_per_year <-  new$crop_income_lcu_per_year + new$lstk_income_lcu_per_year
  new$tot_income_lcu_per_year <-  new$farm_income_lcu_per_year + NAto0(hhinfo$off_farm_lcu_per_year)
  new$farm_consumed_kcal_per_year <- new$crop_consumed_kcal_per_year + new$lstk_consumed_kcal_per_year
  hhinfo <- cbind(hhinfo[,!names(hhinfo)%in%names(new)], new)
  return(hhinfo)
}


