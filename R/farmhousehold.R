#' Create a new farmhousehold object
#' @param crop crop table as data.frame
#' @param lstk livestock table as data.frame
#' @param lstk_prod livestock production table as data.frame
#' @param hhinfo household information table as data.frame
#' @param conv_tlu numeric vector with tlu conversion factors
#' @param conv_energy numeric vector with energy conversion factors
new_farmhousehold <- function(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy) {
  stopifnot(is.data.frame(crop))
  stopifnot(is.data.frame(lstk))
  stopifnot(is.data.frame(lstk_prod))
  stopifnot(is.data.frame(hhinfo))
  stopifnot(is.numeric(conv_tlu))
  stopifnot(is.numeric(conv_energy))
  stopifnot(!is.null(names(conv_tlu)))
  stopifnot(!is.null(names(conv_energy)))

  x <- list("crop"=crop,
            "lstk"=lstk,
            "lstk_prod"=lstk_prod,
            "hhinfo"=hhinfo,
            "conv_tlu"=conv_tlu,
            "conv_energy"=conv_energy)
  return(structure(x, class = "farmhousehold"))
}


#' Validate a farmhousehold object
#' @param x farmhousehold S3 object
validate_farmhousehold <- function(x) {
  #check if the key columns are present
  colcl <- c("hhid", "name", "harvest_kg", "consumed_kg", "sold_kg", "income_lcu")
  stopifnot(all(c(colcl, "land_area_ha") %in% names(x$crop)))
  stopifnot(all(c("hhid", "name") %in% names(x$lstk)))
  stopifnot(all(c(colcl, "prod") %in% names(x$lstk_prod)))
  colhh <- c("hhid", "country", "hh_size_mae", "hh_size_members", "off_farm_lcu", "off_farm_div")
  stopifnot(colhh %in% names(x$hhinfo))
  # check if the hhid is unique
  stopifnot(!any(duplicated(x$hhinfo$hhid)))
  # check
  stopifnot(any(NAto0(x$conv_tlu)>=0))
  stopifnot(any(NAto0(x$conv_energy)>=0))

  invisible(x)
}

#' Create a new farmhousehold object
#'
#' Binds the dataframe with farm household information
#' into a farmhousehold S3 object
#' @param crop crop table as data.frame
#' @param lstk livestock table as data.frame
#' @param lstk_prod livestock production table as data.frame
#' @param hhinfo household information table as data.frame
#' @param conv_tlu numeric vector with tlu conversion factors
#' @param conv_energy numeric vector with energy conversion factors
#' @keywords farmhousehold
#' @export
farmhousehold <- function(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy) {
  crop <- as.data.frame(crop)
  lstk <- as.data.frame(lstk)
  lstk_prod <- as.data.frame(lstk_prod)
  hhinfo <- as.data.frame(hhinfo)

  #select only households with crop or livestock
  selhh <- hhinfo$hhid%in%crop$hhid | hhinfo$hhid%in%lstk$hhid | hhinfo$hhid%in%lstk_prod$hhid
  hhinfo <- hhinfo[selhh,]
  crop <- crop[crop$hhid %in%hhinfo$hhid,]
  lstk <- lstk[lstk$hhid %in%hhinfo$hhid,]
  lstk_prod <- lstk_prod[lstk_prod$hhid %in%hhinfo$hhid,]

  # simplify the conversion factors
  conv_tlu <- conv_tlu[names(conv_tlu)%in%lstk$name]
  conv_energy <- conv_energy[names(conv_energy)%in%crop$name | names(conv_energy)%in%lstk_prod$prod]

  #create the S3 object and validate it
  x <- new_farmhousehold(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy)
  x <- validate_farmhousehold(x)
  return(x)
}

#' Print short summary of the farmhousehold data
#'
#' @param x farmhousehold object
#' @export
print.farmhousehold <- function(x){
  cat("Farm household dataset\n")
  cat("With", nrow(x$hhinfo), "households in", lunique(x$hhinfo$country), "different countries\n\n")

  cat("Crop production \n")
  cat("Records on", nrow(x$crop), "crop production, from", lunique(x$crop$name), "different crops.\n")
  top5 <- sort(table(x$crop$name), decreasing = TRUE)[1:5]
  cat("The most common crops are:", names(top5),"\n")
  top5 <- sort(tapply(x$crop$harvest_kg, x$crop$name, sum, na.rm=TRUE), decreasing = TRUE)[1:5]
  cat("The highest crop production are:", names(top5)," \n \n")

  cat("Livestock herd \n")
  cat("Records on", nrow(x$lstk), "herd, from", lunique(x$lstk$name), "different species.\n")
  top5 <- sort(table(x$lstk$name), decreasing = TRUE)[1:5]
  cat("The most popular livestock species are:", names(top5)," \n \n")
}

#' Update the crop and livestock production statistics per household
#'
#' @param x farmhousehold object
#' @examples
#' data(hhdb_rhomis)
#' # increase all price by 50%
#' hhdb_rhomis$crop$income_lcu <- hhdb_rhomis$crop$income_lcu*1.5
#' #update the summary
#' hhdb_rhomis_highprice <- update_farmhousehold(hhdb_rhomis)
#' median(hhdb_rhomis_highprice$hhinfo$crop_value_lcu/hhdb_rhomis$hhinfo$crop_value_lcu, na.rm=TRUE)
#' @export
update_farmhousehold <- function(x){
  newhhinfo <- calc_farm_prod(x$crop, x$lstk, x$lstk_prod, x$hhinfo,
                              x$conv_tlu, x$conv_energy)
  return(farmhousehold(x$crop, x$lstk, x$lstk_prod, newhhinfo,
                x$conv_tlu, x$conv_energy))
}


#' Subset the household
#'
#' @param x farmhousehold object
#' @param idlist vector with the hhid selected
#' @examples
#' data(hhdb_rhomis)
#' # select household with livestock
#' id <- sort(unique(hhdb_rhomis$lstk$hhid))
#' lstkfarm <- select_farmhousehold(hhdb_rhomis, id)
#' @export
select_farmhousehold <- function(x, idlist){
  stopifnot(sum(x$hhinfo$hhid%in%idlist)>0)
  # select the household in hhinfo
  x$hhinfo <- x$hhinfo[x$hhinfo$hhid%in%idlist,]
  # the crop, lvst, and lvst table will be selected in farmhousehold()
  return(farmhousehold(x$crop, x$lstk, x$lstk_prod, x$hhinfo,
                       x$conv_tlu, x$conv_energy))
}

#' Export the household table information as csv
#'
#' @param x farmhousehold object
#' @param file name of the output file
#' @param path directory of the output file
#' @examples
#' data(hhdb_rhomis)
#' # increase all price by 50%
#' saveCSV_hhinfo(hhdb_rhomis, "rhomis_hhinfo")
#' @export
saveCSV_hhinfo <- function(x, file, path=NULL){
  #add the extension if needed
  if(!grepl("\\.csv$", file)){
    file <- paste0(file, ".csv")
  }
  if(!is.null(path)){
    if(!dir.exists(path)){
      dir.create(path)
    }
    path <- ifelse(grepl("/$", path), dir, paste0(path, "/"))
    file <- paste0(path, file)
  }
  write.csv(x$hhinfo, file = file, row.names = FALSE)
}

#' Export the farmhousehold information as four csv files
#'
#' @param x farmhousehold object
#' @param name name of the dataset
#' @param dir directory of the output file
#' @examples
#' data(hhdb_rhomis)
#' saveCSV_farmhousehold(hhdb_rhomis, "rhomis")
#' @export
saveCSV_farmhousehold <- function(x, name, path=NULL){
  # make sure to remove the extension, if any
  if(grepl("\\.csv$", name)){
    name <- gsub("\\.csv$", "", name)
  }
  if(!is.null(path)){
    # make sure the directory exist, else create it
    if(!dir.exists(path)){
      dir.create(path)
    }
    path <- ifelse(grepl("/$", path), path, paste0(path, "/"))
    name <- paste0(path, name)
  }
  # make sure the farmhousehold object is valid
  x <- validate_farmhousehold(x)
  # and updated
  x <- update.farmhousehold(x)

  write.csv(x$hhinfo, file = paste0(name, "_hhinfo.csv"),
            row.names = FALSE)
  write.csv(x$crop, file = paste0(name, "_crop.csv"),
            row.names = FALSE)
  write.csv(x$lstk, file = paste0(name, "_lstk.csv"),
            row.names = FALSE)
  write.csv(x$lstk_prod, file = paste0(name, "_lstkprod.csv"),
            row.names = FALSE)
}

#' Export the farmhousehold information as rds files
#'
#' @param x farmhousehold object
#' @param name name of the dataset
#' @param dir directory of the output file
#' @examples
#' data(hhdb_rhomis)
#' saveRDS_farmhousehold(hhdb_rhomis, "rhomis")
#' @export
saveRDS_farmhousehold <- function(x, name, path=NULL){
  # make sure to remove the extension, if any
  if(grepl("\\.rds$", name)){
    name <- gsub("\\.rds$", "", name)
  }
  if(!is.null(path)){
    # make sure the directory exist, else create it
    if(!dir.exists(path)){
      dir.create(path)
    }
    path <- ifelse(grepl("/$", path), path, paste0(path, "/"))
    name <- paste0(path, name)
  }
  # make sure the farmhousehold object is valid
  x <- validate_farmhousehold(x)
  # and updated
  x <- update.farmhousehold(x)
  # save it in a rda file
  save(x, file=name)
}
