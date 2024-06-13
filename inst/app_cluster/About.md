*by Romain Frelat, James Hammond, and Mark T. van Wijk*



The household typology (or cluster analysis) follow six successive steps:

#### 1. **Select the dataset**

You can load any csv file with households in row and variables in column.
If you have `farmhousehold` data saved as rds file, you can load them directly.  

Large dataset (N>10.000) slow down the analysis. You can subset your data based on geographic variables.  

If needed for testing, you can download an example data [here](rhomis.rds)

#### 2. **Select the variables**
By default, the key variable from `farmhousehold` can be selected. Make sure to select enough variables to represent the diversity of farm households; but check the distribution and correlation among variables (see step 4).  

#### 3. **Outliers**
There are two main option to deal with outliers or right skewed variables.

The log transformation will log(x+1) all variables with a skewness higher than 5.  

The exclusion of outliers will remove all households that have values higher than median +3*iqr (interquartile range). 

There is no best solution to deal with outliers, test (and combine) them, visualize the results (see step 4) and select the one that is best for your dataset.

#### 4. **Explore data**
The visualization of the dataset will help you select variables and deal with outliers. Variables should be Gaussian-like, and not too correlated among each other. Be aware that large outliers might drive the subsequent multivariate analysis. 


#### 5. **Multivariate analysis**
Select the number of principal component that successfully summarize the diversity of your dataset. At this step, it is a good idea to check that no geographic pattern hides in your dataset. If you find strong differences among regions, then you might want to run separate analysis per region.

#### 6. **Cluster analysis**
You can select between hierarchical clustering and k-means algorithms.  

Hierarchical clustering use the euclidean distances between the households in the PC space and agglomerate households based on Ward's criterion. The height of the diverging branch in the resulting dendogram are represented to help you select the optimum number of cluster.  

The k-mean method use the algorithm of Hartigan and Wong 1979. It shows the within-cluster sum of squares per number of clusters to help you select the optimum number of cluster.  

Ideally, the clustering will not change significantly between the two methods (but group labeling will).  

## References

Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108. doi:10.2307/2346830.  

Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification, 31, 274–295. doi:10.1007/s00357-014-9161-z.  

Ward's (1963) 


