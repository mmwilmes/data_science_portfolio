Iris dataset exploration, analysis, visualisation, k-means clustering
================
Madlen Wilmes
2018-11-02

Purpose of the script
---------------------

Examplary analysis of the iconic iris data set. This script uses descriptive statistics, correlation analysis and visualization to explore a data set in R. It also uses k-means clustering to determine the species from two quantitative traits. The letter is an example of an unsupervised learning algorithm. In the end, the clustering results are compared to the actual species data.

Load Data and explore
---------------------

``` r
data(iris)
attach(iris)
head(iris, 3)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa

``` r
str(iris)
```

    ## 'data.frame':    150 obs. of  5 variables:
    ##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

The data is structured in 150 rows and 5 columns. Rows represent individual measurements of a flower in cm. The columns provide information on the sepal length, sepal width, petal length and petal width and species of each flower. There are 50 measurements for each of the three species, no missing data.

Explorative plots
-----------------

``` r
pairs(iris[,1:4])
```

![](iris_files/figure-markdown_github/simple%20correlation%20matrix-1.png)

``` r
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point() + aes(color = Species)
```

![](iris_files/figure-markdown_github/sepal%20by%20petal%20length%20by%20species-1.png)

``` r
ggplot(iris) + aes(x = Species, y = Sepal.Width, fill = Species) + geom_violin()
```

![](iris_files/figure-markdown_github/violin%20plot%20of%20sepal%20width-1.png)

Analysis and visualisation
--------------------------

``` r
# calculate all possible pairings (doubled as all are either x or y, doesn't matter with small data set like this)
for(traity in c("Sepal.Width", "Sepal.Width", "Petal.Length", "Petal.Width"))
for(traitx in c("Sepal.Width", "Sepal.Width", "Petal.Length", "Petal.Width")){
  if(traitx == traity) next
  print(paste(traity, "by", traitx, sep=" "))
  print(summary(aov(iris[,traity] ~ iris[,traitx])))
  print(
    ggplot(iris, aes(x = iris[,traitx], y = iris[,traity], color = Species)) + 
      geom_point() + 
      geom_smooth(method = "lm", se = T) +
      xlab(traitx) +
      ylab(traity) +
      ggtitle(paste(traity, "by", traitx, sep=" "))
  )
    }
```

    ## [1] "Sepal.Width by Petal.Length"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  5.196   5.196   33.27 4.51e-08 ***
    ## Residuals      148 23.111   0.156                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-1.png)

    ## [1] "Sepal.Width by Petal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  3.794   3.794   22.91 4.07e-06 ***
    ## Residuals      148 24.512   0.166                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-2.png)

    ## [1] "Sepal.Width by Petal.Length"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  5.196   5.196   33.27 4.51e-08 ***
    ## Residuals      148 23.111   0.156                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-3.png)

    ## [1] "Sepal.Width by Petal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  3.794   3.794   22.91 4.07e-06 ***
    ## Residuals      148 24.512   0.166                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-4.png)

    ## [1] "Petal.Length by Sepal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1   85.2   85.23   33.27 4.51e-08 ***
    ## Residuals      148  379.1    2.56                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-5.png)

    ## [1] "Petal.Length by Sepal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1   85.2   85.23   33.27 4.51e-08 ***
    ## Residuals      148  379.1    2.56                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-6.png)

    ## [1] "Petal.Length by Petal.Width"
    ##                 Df Sum Sq Mean Sq F value Pr(>F)    
    ## iris[, traitx]   1  430.5   430.5    1882 <2e-16 ***
    ## Residuals      148   33.8     0.2                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-7.png)

    ## [1] "Petal.Width by Sepal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  11.60  11.605   22.91 4.07e-06 ***
    ## Residuals      148  74.97   0.507                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-8.png)

    ## [1] "Petal.Width by Sepal.Width"
    ##                 Df Sum Sq Mean Sq F value   Pr(>F)    
    ## iris[, traitx]   1  11.60  11.605   22.91 4.07e-06 ***
    ## Residuals      148  74.97   0.507                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-9.png)

    ## [1] "Petal.Width by Petal.Length"
    ##                 Df Sum Sq Mean Sq F value Pr(>F)    
    ## iris[, traitx]   1  80.26   80.26    1882 <2e-16 ***
    ## Residuals      148   6.31    0.04                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](iris_files/figure-markdown_github/anova%20and%20plot%20with%20regression%20line%20by%20species-10.png)

Strong association of Petal.Length ~ Petal.Width (highest F-value)

``` r
select(iris, -Species) %>% cor()
```

    ##              Sepal.Length Sepal.Width Petal.Length Petal.Width
    ## Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
    ## Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
    ## Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
    ## Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

Numerical confirmation that Petal.Length ~ Petal.Width are highly (positively) correlated.

``` r
# use clustering to visualize correlation of variables; 
# the closer, the stronger correlated
# blue for positive correlation, red for negative
select(iris, -Species) %>% correlate() %>% network_plot()
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

![](iris_files/figure-markdown_github/correlation%20in%20numbers-1.png)

``` r
chart.Correlation(iris[,1:4], histogram=TRUE, pch=19)
```

![](iris_files/figure-markdown_github/plot%20sophisticated%20correlation%20matrix-1.png) \#\# k-means clustering

-   form clusters but assigning a group to data; values in one group (cluster) are closer together than in another
-   unsupervised (i.e., assumes no known labels, no expected oucome), exploratory analysis
-   randomly choose centroid, minimize
-   k is the number of clusters (here known, as there are three species; often k is unknown so choosing k is another issue)
-   cannot deal with missing data
-   bases on calculation of distance matrix (i.e., a measure of similarity), several are available, Euclidian is most common
-   algorithm randomly assigns value to cluster -&gt; calclulates new centroid (mean value of all the data points in the cluster at this step) -&gt; repeat until within cluster variation cannot be reduced any further (within cluster variation is sum of the euclidean distance between the data points and their respective cluster centroids)

Caution: - sensitive to outliers (explore data first!) - results rely on sensible choice of number of clusters

There is no normalization required for the iris data set as all traits are measured in the same unit (cm). If different units where used (and conversion not possible), values should be normalized so they have mean zero and standard deviation one.

``` r
set.seed(9) # ensure reporducibility by setting seed
cluster <- kmeans(iris[, c("Petal.Length", "Petal.Width")], centers = 3, nstart = 25) 
# centers = k
# nstart = number of random sets
# report set with lowest within cluster variation
cluster
```

    ## K-means clustering with 3 clusters of sizes 48, 50, 52
    ## 
    ## Cluster means:
    ##   Petal.Length Petal.Width
    ## 1     5.595833    2.037500
    ## 2     1.462000    0.246000
    ## 3     4.269231    1.342308
    ## 
    ## Clustering vector:
    ##   [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [71] 3 3 3 3 3 3 3 1 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1
    ## [106] 1 3 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 3 1
    ## [141] 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 16.29167  2.02200 13.05769
    ##  (between_SS / total_SS =  94.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
# given that we know the actual species, we can compare cluster results
table(cluster$cluster, iris$Species)
```

    ##    
    ##     setosa versicolor virginica
    ##   1      0          2        46
    ##   2     50          0         0
    ##   3      0         48         4

The default method is Hartigan-Wong which defines the total within-cluster variation as the sum of squared Euclidean distances between values and the corresponding centroid

$$
W(C\_k) = \\sum\_ {x\_i\\in C\_k} (x\_i - μ\_k)^2 \\\\
$$
 where *x*<sub>*i*</sub> is a data point belonging to the cluster *C*<sub>*k*</sub> and *μ*<sub>*k*</sub> is the mean value of the points assigned to the cluster *C*<sub>*k*</sub>

Visual representation of k-means clustering results
---------------------------------------------------

``` r
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
     geom_point(size = 4) +
      geom_point(data = iris, aes(x = Petal.Length, y = Petal.Width), shape = 7, color = cluster$cluster)
```

![](iris_files/figure-markdown_github/visual%20representation%20of%20clustering%20accuracy-1.png)

useful resources:
-----------------

k-means clustering intro: <https://uc-r.github.io/kmeans_clustering>
