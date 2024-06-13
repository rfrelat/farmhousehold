*by Romain Frelat, James Hammond, and Mark T. van Wijk*  
*This dashboard is part of the [farmhousehold](https://github.com/rfrelat/farmhousehold) R-package*


The positive deviant analysis follows six successive steps:

#### **1. Select the dataset**

You can load any csv file with households in row and variables in column or any `farmhousehold` data saved as rds file.  

Large data (N>10.000) slow down the analysis. You can subset your data based on geographic variables.   

If needed for testing, you can download an example data [here](https://github.com/rfrelat/farmhousehold/raw/main/inst/extdata/mini_rhomis.rds)

#### **2. Select variables**

By default, only numeric variables can be selected. Select which variables should be maximized and minimized by the pareto optimality algorithm.


#### **3. Outliers**

There are two main options to deal with outliers or right-skewed variables.  

- log transformation will log(x+1) all variables with a skewness higher than 5.    

- exclusion of outliers will remove all households that have values higher than the $median +3 * iqr$ (interquartile range) or lower than $median -3 * iqr$.  

There is no best solution to deal with outliers. Test them, visualize the results (see step 4) and select the combination that is best for your data. Another option worth considering is to exclude from the analysis highly skewed variables or variables with a lot of outliers.  


#### **4. Explore data**

The visualization of the data will help you select variables and deal with outliers. Variables should be Gaussian-like, and not too correlated among each other.    


#### **5. Pareto optimality**

The pareto rank is calculated with the function eaf::pareto_rank() that use genetic algorithm: NSGA-II (*Deb et al. 2002*).

To select positive deviants, there are three options.

- **rank 1**: non-dominated households (= Pareto rank 1), as in *Modernel et al. 2018* or *Liang et al. 2022*.    
- **rank 1 & better**: household Pareto rank 1 AND all variables better than the population median, as in *Adelhart Toorop et al. 2020*.
- **rank 3 & better**: household Pareto rank higher or equal to 3 AND all variables better than the population median as in *Shija et al. 2022, World*
- **rank 2 & better 2**: household Pareto rank 1 and 2 AND almost all variables better than the population median, a intermediate between the two previous selection



#### **6. Vizualisation**

The positive deviants can be visually compared on a multivariate space made of the two first principal components from a principal component analysis. The distribution of the selected variables is shown with boxplot. 



### References

Kalyanmoy Deb, A Pratap, S Agarwal, T Meyarivan (2002). “A fast and elitist multi-objective genetic algorithm: NSGA-II.” IEEE Transactions on Evolutionary Computation, 6(2), 182–197. doi 10.1109/4235.996017  

Liang, Z., van der Werf, W., Xu, Z., Cheng, J., Wang, C., Cong, W. F., ... & Groot, J. C. (2022). Identifying exemplary sustainable cropping systems using a positive deviance approach: Wheat-maize double cropping in the North China Plain. Agricultural Systems, 201, 103471. doi 10.1016/j.agsy.2022.103471

Modernel, Pablo, et al. "Identification of beef production farms in the Pampas and Campos area that stand out in economic and environmental performance." Ecological Indicators 89 (2018): 755-770. doi: 10.1016/j.ecolind.2018.01.038  

Shija, D. S., Mwai, O. A., Migwi, P. K., Komwihangilo, D. M., & Bebe, B. O. (2022). Identifying positive deviant farms using Pareto-Optimality ranking technique to assess productivity and livelihood benefits in smallholder dairy farming under contrasting stressful environments in Tanzania. World, 3(3), 639-656. doi 10.3390/world3030035    

Steinke, J., Mgimiloko, M. G., Graef, F., Hammond, J., van Wijk, M. T., & van Etten, J. (2019). Prioritizing options for multi-objective agricultural development through the positive deviance approach. PLoS One, 14(2), e0212926. doi: 10.1371/journal.pone.0212926   

Toorop, R. A., Ceccarelli, V., Bijarniya, D., Jat, M. L., Jat, R. K., Lopez-Ridaura, S., & Groot, J. C. (2020). Using a positive deviance approach to inform farming systems redesign: A case study from Bihar, India. Agricultural Systems, 185, 102942. doi: 10.1016/j.agsy.2020.102942   
