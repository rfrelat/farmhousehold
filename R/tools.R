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
  x <- is.numeric(x)
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

#' Scale values between 0 and 1
#'
#' @param x is a numeric vector
#' @param ... additional elements that are passed to min() and max()
#' @return the scaled vector with value between 0 and 1
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





simplestring <- function(x) return(tolower(gsub("[()]", "", x)))

cleanconv <- function(x) {
  #remove brackets and ' '  and lower case
  y <- tolower(gsub("[( )]", "", x))
  #remove last 's'
  y <- gsub("s$", "", y)
 return(y)
}

cleanname <- function(x){
  #lower case
  y <- tolower(x)
  #remove brackets
  y <- gsub("[{()}]", "", y)
  #replace ' ' by _
  y <- gsub(" ", "_", y)
  return(y)
}

conv_fun <- function(conv, x, other=NULL, cat=NULL, lab="otherspecify"){
  if (!is.null(cat)){
    miss <- x[!as.character(x)%in%names(cat)]
    miss <- miss[!is.na(miss)&duplicated(miss)]
    if(length(miss)>0){
      warning(paste("Missing conversion categories for", paste(miss, collapse = ", ")))
    }
    x <- cat[as.character(x)]
  }
  if (!is.null(other)){
    x <- ifelse(cleanconv(x)==lab, other, x)
  }
  x <- cleanconv(x)
  # make sure the names are lower string
  names(conv) <- cleanconv(names(conv))
  # convert into kg
  y <- conv[x]
  # for no conversion, set to 1
  y[x==""] <- 1
  # check missing conversion
  miss <- table(x[is.na(y)&!is.na(x)])
  if(length(miss)>0){
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


# NAto1 <- function(x){
#   x[is.na(x)] <- 1
#   return(x)
# }
