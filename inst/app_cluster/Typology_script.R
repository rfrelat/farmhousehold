# Household typology tutorial
# Last update: 03/10/2023
# Author: Romain Frelat

# 1. Load the package and the dataset ---------------------
# make sure the packages are installed
# install.packages(c("moments", "corrplot", "ade4"))
library(moments)
library(corrplot)
library(ade4)

# specify the correct path to the file
# here the file is located in the working directory 
tab <- read.csv("Household_Data.csv")
dim(tab)

# set parameters
logT <- TRUE #log transform skewed variables
outR <- FALSE #remove outliers
npc <- 3 #number of PC axis
cluA <- "hca" #set to "hca" for Hierarchical cluster analysis or "kmeans" 
nclu <- 3 #number of clusters
# PC axis for visualization
xaxis=1
yaxis=2

#2. Select the variables ----------------------------------
# update the section of variables according to your objectives 
varSelected <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
                "farm_div","farm_sold_perc_kg","farm_income_div")

# create a subset of tab only with selected variables
tabS <- tab[,varSelected]

# check that there is no NA in the selected variables
if (sum(!complete.cases(tabS)>0)){
  print(paste(sum(!complete.cases(tabS)), "households have incomplete information and are removed from the analysis"))
  tabS <- tabS[complete.cases(tabS),]
}

# 3. Select the variables ----------------------------------

# log transformation of skewed variables
if (logT){ #set to TRUE/FALSE to run (or not) the log-transformation step
  # compute skewness per variable with moments package
  sk <- apply(tabS, 2, moments::skewness, na.rm=TRUE)
  if (sum(abs(sk)>5)>0){
    print(paste("Variables log transformed: ", paste(names(sk)[abs(sk)>5], collapse = ", ")))
    for (i in which(abs(sk)>5))
      tabS[,i] <- log(tabS[,i]+1)
  }
}

# removing outliers
if (outR){ #set to TRUE/FALSE to run (or not) the exclusion of outliers
  # compute median and iqr
  m <- apply(tabS, 2, median, na.rm=TRUE)
  iq <- apply(tabS, 2, IQR, na.rm=TRUE)
  # the extreme outliers are the households with value above median + 3*IQR
  maxout <- m+3*iq
  vardif <- t(t(tabS)-maxout)
  outlier <- apply(vardif>0,1,sum)>0
  if (sum(outlier)>0){
    print(paste(sum(outlier), "outliers removed"))
  }
  tabS <- tabS[!outlier,]
}

# 4. Explore dataset ----------------------------------
#boxplot
boxplot(scale(tabS), ylab="Scaled variable")

#correlogram with corrplot package
corrplot.mixed(cor(tabS, use = "complete.obs", method="spearman"),
               lower = "ellipse",
               upper = "number", 
               tl.col = "black",
               mar = c(1,1,1,1))

# 5. Multivariate analysis --------------------------------

# run PCA with ade4 package
pca1 <- dudi.pca(tabS, scannf = FALSE, nf = npc)

# check the amount of inertia (usually variance) associated to each dimension.
screeplot(pca1)

# visualize the loading of variables
s.corcircle(pca1$co, clabel = 1,
            xax = xaxis, yax = yaxis)

#visualize the households' score
s.label(pca1$li, pch=16, clabel = 0, 
        cpoint=1, boxes=FALSE,
        xax = xaxis, yax = yaxis)


# 6. Cluster analysis -------------------------------------
if(cluA=="hca"){ #hierarchical clustering
  hc <- hclust(dist(pca1$li), method = "ward.D2")
  cluster <- cutree(hc, k=nclu)

  # set the color of the dots
  pcol <- rep("black", 20)
  pcol[nclu] <- "red"
  # represent the height of the cut
  plot(sort(hc$height, decreasing = TRUE)[1:20], 
       type="b", pch=16, col=pcol,
       xlab="Number of clusters", ylab="Height")
  # plot(hc) is also a very good alternative
  
} else { #k-means
  #calculate within sum of square for 1 to 20 clusters
  wss <- sapply(1:20, function(x) kmeans(pca1$li, x)$tot.withinss)
  plot(1:20, wss, 
       type="b", pch=16, col=pcol,
       xlab="Number of clusters", ylab="Within sum of square")
  
  #create clusters
  cluster <- kmeans(pca1$li, centers = nclu, nstart = 50)$cluster
}

# represent the clusters on PC axis
s.class(pca1$li, fac = as.factor(cluster),
        xax = xaxis, yax = yaxis)


# show the distribution of variable per cluster
boxplot(tabS[,1]~cluster,
        ylab=names(tabS)[1], xlab="cluster")

# or with numbers instead of graph
tapply(tabS[,1],cluster, summary)
