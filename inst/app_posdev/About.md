*by Romain Frelat, Elizabeth Getahun, Leah Gichuki, Birgit Habermann, James Hammond, Emmaculate Kiptoo, Tigist Worku, and Mark T. van Wijk*  
*This interactive dashboard is part of the [farmhousehold](https://github.com/rfrelat/farmhousehold) R-package*


Positive deviant analysis is a method to identify multi-functionally well performing farm households. It is used as an example of viable interventions based on empirical insights and as farm modeling input (Modernel *et al.* 2018, Steinke *et al.* 2019, Toorop *et al.* 2020).      
The positive deviant analysis is based on the Pareto optimality algorithm, which identifies non-inferior solutions in the multidimensional objective space. 

The analysis unfolds in six successive steps:  

#### **1. Select the dataset**

You can load any csv file with households in rows and variables in columns or any `farmhousehold` data saved as rds file.  

Large data (>5Mb) can't be uploaded to the online dashboard. Please consider using the desktop version from the `farmhousehold` package (`runPosdev()`). It is advised to upload a cleaned and simplified dataset for smooth and fast analysis (in the online version, you should aim for a maximum of 10.000 households or 100 variables).   

If needed for testing, you can download an example data [here](https://github.com/rfrelat/farmhousehold/raw/main/inst/extdata/mini_rhomis.rds).  

#### **2. Select variables**

By default, only numeric variables can be selected. Select which variables should be maximized and minimized by the Pareto optimality algorithm.


#### **3. Outliers**

There are two main options to deal with outliers or right-skewed variables.  

- log transformation will log(x+1) all variables with a skewness higher than 5.    

- exclusion of outliers will remove all households that have values higher than the $median +3 * iqr$ (interquartile range) or lower than $median -3 * iqr$.  

There is no perfect solution to deal with outliers. Test them, visualize the results (see step 4) and select the combination that is best for your data. Another option worth considering is to exclude from the analysis highly skewed variables or variables with many outliers.  


#### **4. Explore data**

The visualization of the data will help you select variables and identify outliers. Variables should be Gaussian-like, and not too correlated with each other.       


#### **5. Pareto optimality**

The Pareto rank is calculated with the function `eaf::pareto_rank()` that uses the genetic algorithm NSGA-II (*Deb et al. 2002*).

To select positive deviants, there are three options.

- **rank 1**: non-dominated households (= Pareto rank 1), as in *Modernel et al. 2018* or *Liang et al. 2022*.    
- **rank 1 & better**: household Pareto rank 1 AND all variables better than the population median, as in *Adelhart Toorop et al. 2020*.
- **rank 3 & better**: household Pareto rank higher or equal to 3 AND all variables better than the population median, as in *Shija et al. 2022, World*
- **rank 2 & better 2**: household Pareto rank 1 or 2 AND almost all variables better than the population median.



#### **6. Visualization**

The positive deviants can be visually compared on a multivariate space made of the two first principal components from a principal component analysis. The distribution of the selected variables is shown with a boxplot. 



### References

Kalyanmoy Deb, A Pratap, S Agarwal, T Meyarivan (2002). “A fast and elitist multi-objective genetic algorithm: NSGA-II.” IEEE Transactions on Evolutionary Computation, 6(2), 182–197. doi 10.1109/4235.996017  

Liang, Z., van der Werf, W., Xu, Z., Cheng, J., Wang, C., Cong, W. F., ... & Groot, J. C. (2022). Identifying exemplary sustainable cropping systems using a positive deviance approach: Wheat-maize double cropping in the North China Plain. Agricultural Systems, 201, 103471. doi 10.1016/j.agsy.2022.103471

Modernel, Pablo, et al. "Identification of beef production farms in the Pampas and Campos area that stand out in economic and environmental performance." Ecological Indicators 89 (2018): 755-770. doi: 10.1016/j.ecolind.2018.01.038  

Shija, D. S., Mwai, O. A., Migwi, P. K., Komwihangilo, D. M., & Bebe, B. O. (2022). Identifying positive deviant farms using Pareto-Optimality ranking technique to assess productivity and livelihood benefits in smallholder dairy farming under contrasting stressful environments in Tanzania. World, 3(3), 639-656. doi 10.3390/world3030035    

Steinke, J., Mgimiloko, M. G., Graef, F., Hammond, J., van Wijk, M. T., & van Etten, J. (2019). Prioritizing options for multi-objective agricultural development through the positive deviance approach. PLoS One, 14(2), e0212926. doi: 10.1371/journal.pone.0212926   

Toorop, R. A., Ceccarelli, V., Bijarniya, D., Jat, M. L., Jat, R. K., Lopez-Ridaura, S., & Groot, J. C. (2020). Using a positive deviance approach to inform farming systems redesign: A case study from Bihar, India. Agricultural Systems, 185, 102942. doi: 10.1016/j.agsy.2020.102942   
