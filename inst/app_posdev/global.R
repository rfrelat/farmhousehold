suppressPackageStartupMessages({
  require(shiny)
  library(shinymeta)
  library(moments)
  library(corrplot)
  library(ade4)
  library(eaf)
  library(farmhousehold)
})


# new plot function for positive deviant analysis
posdevplot <- function(df, maxvar, sel=c("pr1", "pr1bt", "pr3bt", "pr2bt2")){

  pr<-pareto_rank(df,maximise = maxvar)

  # Calculate median values per variable
  medvar <- apply(df, 2, median)

  # Calculate the difference between household and the median
  vardif <- t(t(df)-medvar)

  # Number of variables with 'better' than median
  better <- rowSums(as.matrix(vardif[,maxvar]>=0)) + rowSums(as.matrix(vardif[,!maxvar]<=0))

  #compute the cross table
  a <- table("better"=better, "Pareto"=pr)
  if(ncol(a)>15){
    a <- a[,1:15]
  }
  layout(matrix(c(1,3,4,2), ncol=2), widths = c(2,1), heights = c(1,2))
  par(mar=c(3,0,3,0), xaxs="i", yaxs="i")
  barx <- barplot(colSums(a), main="Pareto ranking")
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
    } else if (sel=="pr1bt"&i==nrow(a)){
      coli <- c("red", rep("black", ncol(a)-1))
    } else if (sel=="pr3bt"&i==nrow(a)){
      coli <- c("red", "red","red", rep("black", ncol(a)-3))
    } else if (sel=="pr2bt2"&i%in%(nrow(a)-1):nrow(a)){
      coli <- c("red", "red", rep("black", ncol(a)-2))
    } else {
      coli <- rep("black", ncol(a))
    }
    text(barx, bary[i], a[i,], col=coli, cex=1.3)
  }

  #select the positive deviants
  if (sel=="pr1"){
    pd <- pr==1
  } else if (sel=="pr1bt"){
    pd <- pr==1 & better ==ncol(df)
  } else if (sel=="pr3bt"){
    pd <- pr<=3 & better ==ncol(df)
  } else if (sel=="pr2bt2"){
    pd <- pr<=2 & better >=ncol(df)-1
  }

  plot.new()
  legend("center", bty="n", cex=1.5,
         legend = c("Positive Deviants", paste0("N=", sum(pd), " (", round(sum(pd)/length(pd)*100), "%)")))
  invisible(pd)
}

scaleChoices <- list("Large region"= "large_region",
                     "Country"="country",
                     "Region"='region',
                     "Köppen climate"="koeppen",
                     "Farming system"="farming_system",
                     "Project"="project")


metChoices <- list("rank 1"="pr1",
                   "rank 1 & better "="pr1bt",
                   "rank 3 & better "="pr3bt",
                   "rank 2 & better 2 "="pr2bt2")

varChoices <- c("land_cultivated_ha","livestock_tlu", "hh_size_mae",
                "crop_yield_kg_per_ha","crop_sold_perc","crop_income_div",
                "lstk_yield_kg_per_tlu","lstk_sold_perc","lstk_income_div",
                "farm_div","crop_div","lstk_div",
                "income_per_person_per_day_usd",
                "farm_sold_perc_kg",  "off_farm_perc",
                "pop_pressure_mae_per_ha", "lstk_pressure_tlu_per_ha",
                "farm_income_div", "hdds_score", "fies_score","foodshortage_count",
                "population_2020", "travel_time_cities")
