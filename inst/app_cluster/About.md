*by Romain Frelat, James Hammond, and Mark T. van Wijk*  
*This dashboard is part of the [farmhousehold](https://github.com/rfrelat/farmhousehold) R-package*


The household typology (or cluster analysis) follows six successive steps:

#### **1. Select the dataset**

You can load any csv file with households in row and variables in column or any `farmhousehold` data saved as rds file.  

Large data (N>10.000) slow down the analysis. You can subset your data based on geographic variables.   

If needed for testing, you can download an example data [here](https://github.com/rfrelat/farmhousehold/raw/main/inst/extdata/mini_rhomis.rds)

#### **2. Select variables**

By default, the key variables from `farmhousehold` can be selected. Make sure to select enough variables to represent the diversity of farm households; but check the distribution and correlation among variables (see step 4).   


#### **3. Outliers**

There are two main options to deal with outliers or right-skewed variables.  

- log transformation will log(x+1) all variables with a skewness higher than 5.    

- exclusion of outliers will remove all households that have values higher than $median +3 * iqr$ (interquartile range) or lower than $median -3 * iqr$.     

There is no best solution to deal with outliers. Test them, visualize the results (see step 4) and select the combination that is best for your data. Another option worth considering is to exclude from the analysis highly skewed variables or variables with a lot of outliers.   


#### **4. Explore data**

The visualization of the data will help you select variables and deal with outliers. Variables should be Gaussian-like, and not too correlated among each other. Be aware that large outliers might drive the subsequent multivariate analysis.  


#### **5. Multivariate analysis**

Select the number of principal components that successfully summarize the diversity of your dataset. At this step, it is a good idea to check that no geographic pattern hides in your dataset. If you find strong differences among regions, you might want to run separate clustering analysis per region.  


#### **6. Cluster analysis**
You can select between hierarchical clustering and k-means algorithms.  

- Hierarchical clustering uses the euclidean distances between the households in the PC space and agglomerates households based on Ward's criterion. The height of the diverging branches in the hierarchical tree is plotted to help you select the optimum number of clusters.  

- K-mean method uses the algorithm of Hartigan and Wong 1979. The plot shows the within-cluster sum of squares per number of clusters to help you select the optimum number of clusters.  

Ideally, the clustering will not change significantly between the two methods (but group labeling will change).   

### References

Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108.  

Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification, 31, 274–295.  

Ward Jr, J. H. (1963). Hierarchical grouping to optimize an objective function. Journal of the American statistical association, 58(301), 236-244.  


