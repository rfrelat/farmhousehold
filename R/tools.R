## TOOLS ------------------------------
lunique <- function(x) length(unique(x))

punique <- function(x) paste(unique(x), collapse = ", ")

Count_Unique <- function(x){
  x <- x[!is.na(x)]
  x <- x[x!="NA"]
  return(length(unique(x)))
}

Count_Spaces <- function(x){
  x <- strsplit(x, " ")
  n<-sapply(x, Count_Unique)
  return(n)
}

Count_NA0 <- function(x){
  #Count the number of NA or 0s
  return(sum(NAto0(x)==0))
}

NAto0 <- function(x){
  x[x<0] <- 0
  x[is.na(x)] <- 0
  return(x)
}

NAto1 <- function(x){
  x[is.na(x)] <- 1
  return(x)
}

sumNAto0 <- function(x){
  return(sum(NAto0(x)))
}

scale01 <- function(x, ...) {
  if (length(unique(x))>1) {
    return((x-min(x, ...))/(max(x, ...)-min(x, ...)))
  } else {
    return(x)
  }
}

uniqueNA <- function(x){  #Remove NAs
  x <- x[!is.na(x)]
  x <- x[x!="NA"]
  return(unique(x))
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
  #make sure the names are lower string
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
