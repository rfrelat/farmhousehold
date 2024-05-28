suppressPackageStartupMessages({
  require(shiny)
  library(shinymeta)
  library(moments)
  library(corrplot)
  library(ade4)
  library(eaf)
  library(farmhousehold)
})

# new function for positive deviant analysis
posdevplot <- function(df, maxvar, sel=c("pr1", "prbt12", "prbt1")){

  pr<-pareto_rank(df,maximise = maxvar)

  # Calculate median values per variable
  medvar <- apply(df, 2, median)

  # Calculate the difference between household and the median
  vardif <- t(t(df)-medvar)

  # Number of variables with 'better' than median
  better <- rowSums(as.matrix(vardif[,maxvar]>=0)) + rowSums(as.matrix(vardif[,!maxvar]>=0))

  #compute the cross table
  a <- table("better"=better, "Pareto"=pr)

  layout(matrix(c(1,3,4,2), ncol=2), widths = c(2,1), heights = c(1,2))
  par(mar=c(3,0,3,0), xaxs="i", yaxs="i")
  barx <- barplot(table(pr), main="Pareto ranking")
  par(mar=c(0,3,0,0))
  bary <- barplot(table(better), horiz=TRUE, las=1)
  #                main="Better then median", xpd=NA)
  mtext("Better then median", side = 3, line = 1, xpd=NA, font=2)
  par(mar=c(0,0,0,0))
  plot(0, xlim=c(min(barx)-diff(barx)[1]/2, max(barx)+diff(barx)[1]/2),
       ylim=c(min(bary)-diff(bary)[1]/2, max(bary)+diff(bary)[1]/2), type="n")
  for (i in 1:nrow(a)){
    if (sel=="pr1"){
      coli <- c("red", rep("black", ncol(a)-1))
    } else if (sel=="prbt12"&i%in%((nrow(a)-1):nrow(a))){
      coli <- c("red", "red", rep("black", ncol(a)-2))
    } else if (sel=="prbt1"&i==nrow(a)){
      coli <- c("red", "red", rep("black", ncol(a)-2))
    } else if (sel=="prbt1"&i==(nrow(a)-1)){
      coli <- c("red", rep("black", ncol(a)-1))
    } else {
      coli <- rep("black", ncol(a))
    }
    text(barx, bary[i], a[i,], col=coli, cex=1.3)
  }

  #select the positive deviants
  if (sel=="pr1"){
    pd <- pr==1
  } else if (sel=="prbt12"){
    pd <- pr<=2 & better >=ncol(df)-1
  } else{
    pd <- pr==2 & better ==ncol(df) | pr==1 & better >=ncol(df)-1
  }

  plot.new()
  legend("center", bty="n", cex=1.5,
         legend = c("Positive Deviants", paste0("N=", sum(pd), " (", round(sum(pd)/length(pd)*100), "%)")))
  invisible(pd)
}

scaleChoices <- list("Country"="country",
                     "Region"='region',
                     "Project"="project",
                     "Köppen climate"="koeppen",
                     "Large region"= "large_region")


metChoices <- list("rank 1"="pr1",
                   "rank & better A"="prbt12",
                   "rank & better B"="prbt1")
