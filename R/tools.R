## TOOLS ------------------------------

#' Number of unique elements of a vector
#'
#' This function returns the number of unique elements
#' in a vector
#' @param x a vector
#' @param na.rm define if NA are removed from the count
#' @return number of unique elements
#' @export
#' @examples
#' lunique(c("a", 5, NA, 5, month.abb[1]))
#'
lunique <- function(x, na.rm=TRUE){
  if(na.rm) {
    x <- x[!is.na(x)]
    x <- x[x!="NA"]
  }
  return(length(unique(x)))
}


#' Collapse unique elements of a vector
#'
#' This function returns a string with
#' the unique elements collapsed together in a single string
#' @param x a vector
#' @param na.rm define if NA are removed from the count
#' @param sep separator between two elements
#' @return one string with all unique elements together
#' @export
#' @examples
#' punique(c("maize", NA, "banana", "maize", "beans"))
#'
punique <- function(x, sep=" ", na.rm=FALSE) {
  if(na.rm) x <- x[!is.na(x)]
  return(paste(unique(x), collapse = sep))
}


#' Unique values that are not NA
#'
#' @param x is a vector
#' @return the unique elements that are not NA
#' @examples
#' uniqueNA(c("a", 5, NA, 5, month.abb[1]))
#'
uniqueNA <- function(x){  #Remove NAs
  x <- x[!is.na(x)]
  x <- x[x!="NA"]
  return(unique(x))
}


#' Count the number of unique elements in a string
#'
#' This function evaluate the number of unique elements in a string
#' @param x is a string
#' @param na.rm define if NA are removed from the count
#' @param sep separator between two elements of the string
#' @return a number of unique element
#' @export
#' @examples
#' Count_Spaces("maize NA banana maize beans")
#'
Count_Spaces <- function(x, sep=" ", na.rm=TRUE){
  x <- strsplit(x, sep)
  n <- sapply(x, lunique, na.rm=TRUE)
  return(n)
}

#' Transform NA and negative values to 0
#'
#' @param x is a numeric vector
#' @return a vector with NA and negative values transformed into 0
#' @export
#' @examples
#' NAto0(c(-99, NA, -1:9))
#'
NAto0 <- function(x){
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  x[x<0] <- 0
  return(x)
}

#' Count number of 0, negative values or NA
#'
#' @param x is a numeric vector
#' @return a number of elements with NA, 0 or negative values
#' @export
#' @examples
#' Count_NA0(c(-99, NA, -1:9))
#'
Count_NA0 <- function(x){
  #Count the number of NA or 0s
  return(sum(NAto0(x)==0))
}

#' Sum all values apart of negative values and NA
#'
#' @param x is a numeric vector
#' @return the sum of the values of x
#' @export
#' @examples
#' sum_NAto0(c(-99, NA, -1:9))==sum(1:9)
#'
sum_NAto0 <- function(x){
  return(sum(NAto0(x)))
}

#' Transform NA and negative values to 1 (for conversion factors)
#'
#' @param x is a numeric vector
#' @return a vector with NA and negative values transformed into 1
#' @export
#' @examples
#' NAto1(c(-99, NA, -1:9))
#'
NAto1 <- function(x){
  x <- as.numeric(x)
  x[is.na(x)] <- 1
  x[x<0] <- 1
  return(x)
}

#' Scale values between 0 and 1
#'
#' @param x is a numeric vector
#' @param ... additional elements that are passed to min() and max()
#' @return the scaled vector with value between 0 and 1
#' @export
#' @examples
#' scale01(c(-99, NA, -1:9))
#'
scale01 <- function(x, ...) {
  if (lunique(x)>1) {
    return((x-min(x, ...))/(max(x, ...)-min(x, ...)))
  } else {
    return(x)
  }
}


#' Clean names
#'
#' @param x is a character vector
#' @return the cleaned names without brackets, and spaces
#' @export
#' @examples
#' cleanname(c("maize", "sweet potatoes", "apple(tree)"))
#'
cleanname <- function(x) {
  # remove brackets, /, . and -
  y <- tolower(gsub("[({/})]", " ", x))
  y <- gsub("\\[", " ", y)
  y <- gsub("\\]", " ", y)
  y <- gsub("-", " ", y)
  y <- gsub("\\.", " ", y)
  # remoce numbers
  y <- gsub("[0-9]", "", y)
  # remove last space or underscore
  y <- gsub("[ _]$", "", y)
  # remove first space or underscore
  y <- gsub("^[ _]", "", y)
  # replace space for underscore
  y <- gsub(" ", "_", y)
  # make sure there are no multiple underscores
  y <- gsub("___", "_", y)
  y <- gsub("__", "_", y)
  return(y)
}

#' Find the best match from a list of known names
#'
#' @param x is the original names
#' @param y is the list of known names
#' @param warn whether to show a warning if missing names
#' @return the names that best match the known names
#' @export
#' @examples
#' bestname(c("bean", "sweetpotatoes", "apples"),c("apple", "beans", "sweet_potatoes"))
#'
bestname <- function(x, y, warn=FALSE){
  #normal cleaning
  x <- cleanname(x)
  y1 <- cleanname(y)
  matchname <- y[match(x,y1)]
  # test without _
  x1 <- gsub("_", "", x)
  y2 <- gsub("_", "", y1)
  match1 <- y[match(x1, y2)]
  matchname <- ifelse(is.na(matchname), match1, matchname)
  # test without last s
  match2 <- y[match(gsub("s$", "", x1), y2)]
  matchname <- ifelse(is.na(matchname), match2, matchname)
  match3 <- y[match(x1, gsub("s$", "", y2))]
  matchname <- ifelse(is.na(matchname), match3, matchname)
  match4 <- y[match(gsub("s$", "", x1), gsub("s$", "", y2))]
  matchname <- ifelse(is.na(matchname), match4, matchname)

  #return the best name, if none, return x]
  if (warn & sum(is.na(matchname))>sum(is.na(x))){
    warning(paste("Missing labels for", paste(sort(unique(x[is.na(matchname)])), collapse = ", ")))
  }
  matchname <- ifelse(is.na(matchname), x, matchname)
  return(matchname)
}


#' Simplify a list of crop names with known names
#'
#' @param x is the original names
#' @param conv_name is the list of known names
#' @param warn add warning if the name is missing from conv_name
#' @return the vector wuth the unique crop names
#' @export
#' @examples
#' simplifynames(c("bean", "sweetpotatoes", "apples"),c("apple", "beans", "sweet_potatoes"))
#'
simplifynames <- function(x, conv_name, warn=TRUE){
  ux <- sort(unique(unlist(strsplit(x, " "))))
  sx <- sort(unique(bestname(ux, conv_name, warn=warn)))
}

#' Find conversion factors for a vector of units
#'
#' @param conv the conversion factors
#' @param x the original unit names
#' @param other the unit names of other (optional)
#' @param cate conversion categories if x are numbers instead of string
#' @param labother label for 'other'
#' @param warn whether to show a warning if missing names
#' @return the vector with the conversion factors
#' @export

conv_fun <- function(conv, x, other=NULL, cate=NULL,
                     labother=c("other", "otherspecify"),
                     warn=TRUE){
  if (!is.null(cate)){
    miss <- x[!as.character(x)%in%names(cate)]
    miss <- miss[!is.na(miss)&duplicated(miss)]
    if(length(miss)>0){
      warning(paste("Missing conversion categories for", paste(miss, collapse = ", ")))
    }
    x <- cate[as.character(x)]
  }
  if (!is.null(other)){
    x <- ifelse(cleanname(x)%in%labother, other, x)
  }
  x <- bestname(x, names(conv))
  # convert into kg
  y <- conv[x]
  # for no conversion, set to 1
  y[x==""] <- 1
  # check missing conversion
  miss <- table(x[is.na(y)&!is.na(x)])
  if(length(miss)>0 & warn){
    if(max(miss)>1){
      warning(paste("Missing conversion factor for", paste(names(miss)[miss>1], collapse = ", ")))
    }
  }
  return(y)
}

panel.cor.m <- function(x, y, digits=2, method="pearson")
{
  usr <- par("usr"); on.exit(par("usr"=usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,method = method)
  r2 <- abs(cor(x, y,method = method))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  cex <- 0.8/strwidth(txt)
  test <- cor.test(x,y,method = method)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  text(0.5, 0.5, txt, cex = cex * r2)
  text(.8, .8, Signif, cex=cex, col=2)
}



# Count_Unique <- function(x){
#   x <- x[!is.na(x)]
#   x <- x[x!="NA"]
#   return(length(unique(x)))
# }



