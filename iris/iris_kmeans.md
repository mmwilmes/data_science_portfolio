Iris dataset k-means clustering and visualization
================
Madlen Wilmes
2018-11-05

Purpose of the script
---------------------

Quantitative traits are clustered using K-means clustering to infer the iris species. The latter is an example of an unsupervised learning algorithm. The clustering results are visually compared to the actual species data.

Background info k-means clustering
----------------------------------

-   form clusters but assigning a group to data; values in one group (cluster) are closer together than in another
-   unsupervised (i.e., assumes no known labels, no expected oucome), exploratory analysis
-   randomly choose centroid, minimize
-   k is the number of clusters (here known, as there are three species; often k is unknown so choosing k is another issue)
-   cannot deal with missing data
-   bases on calculation of distance matrix (i.e., a measure of similarity), several are available, Euclidian is most common
-   algorithm randomly assigns value to cluster -&gt; calclulates new centroid (mean value of all the data points in the cluster at this step) -&gt; repeat until within cluster variation cannot be reduced any further (within cluster variation is sum of the euclidean distance between the data points and their respective cluster centroids)

Caution: - sensitive to outliers (explore data first!) - results rely on sensible choice of number of clusters

There may be no normalization required for the iris data set as all traits are measured in the same unit (cm). If different units where used (and conversion not possible), values should be normalized so they have mean zero and standard deviation one.

``` r
set.seed(9) ## ensure reporducibility by setting seed
cluster <- kmeans(iris[, c("Petal.Length", "Petal.Width")], centers = 3, nstart = 25) 
## centers = k
## nstart = number of random sets
## report set with lowest within cluster variation
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

### Explanation of results

-   cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
-   centers: A matrix of cluster centers.
-   totss: The total sum of squares.
-   withinss: Vector of within-cluster sum of squares, one component per cluster.
-   tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
-   betweenss: The between-cluster sum of squares, i.e. *t**o**t**s**s* − *t**o**t*.*w**i**t**h**i**n**s**s*.
-   size: The number of points in each cluster.

``` r
## given that we know the actual species, we can compare cluster results
table(cluster$cluster, iris$Species)
```

    ##    
    ##     setosa versicolor virginica
    ##   1      0          2        46
    ##   2     50          0         0
    ##   3      0         48         4

Some theory of k-means clustering
---------------------------------

The default method is Hartigan-Wong which defines the total within-cluster variation as the sum of squared Euclidean distances between values and the corresponding centroid

*W*(*C*<sub>*k*</sub>)=∑<sub>*x*<sub>*i*</sub> ∈ *C*<sub>*k*</sub></sub>(*x*<sub>*i*</sub> − *μ*<sub>*k*</sub>)<sup>2</sup>
 where *x*<sub>*i*</sub> is a data point belonging to the cluster *C*<sub>*k*</sub> and *μ*<sub>*k*</sub> is the mean value of the points assigned to the cluster *C*<sub>*k*</sub>

Repeat analysis with standardized data
--------------------------------------

``` r
iris_normalized <- iris
## subtracts the mean and divides by the standard deviation
iris_normalized[,1:4] <- scale(iris[,1:4])
## check that we get mean of 0 and sd of 1
colMeans(iris_normalized[,1:4])  ## faster version of apply(scaled.dat, 2, mean)
```

    ##  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width 
    ## -4.480675e-16  2.035409e-16 -2.844947e-17 -3.714621e-17

``` r
apply(iris_normalized[,1:4], 2, sd)
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
    ##            1            1            1            1

``` r
set.seed(9) ## ensure reporducibility by setting seed
cluster <- kmeans(iris_normalized[, c("Petal.Length", "Petal.Width")], centers = 3, nstart = 25) 
## centers = k
## nstart = number of random sets
## report set with lowest within cluster variation
cluster
```

    ## K-means clustering with 3 clusters of sizes 48, 50, 52
    ## 
    ## Cluster means:
    ##   Petal.Length Petal.Width
    ## 1    1.0245672   1.1242119
    ## 2   -1.3006301  -1.2507035
    ## 3    0.3048515   0.1648655
    ## 
    ## Clustering vector:
    ##   [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [36] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [71] 1 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1
    ## [106] 1 3 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1
    ## [141] 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 9.293174 1.410870 7.202739
    ##  (between_SS / total_SS =  94.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"    
    ## [5] "tot.withinss" "betweenss"    "size"         "iter"        
    ## [9] "ifault"

``` r
## given that we know the actual species, we can compare cluster results
table(cluster$cluster, iris$Species)
```

    ##    
    ##     setosa versicolor virginica
    ##   1      0          2        46
    ##   2     50          0         0
    ##   3      0         48         4

Standadization has no effect on clustering results for the chosen trait.

Visual representation of k-means clustering results
---------------------------------------------------

``` r
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
     geom_point(size = 4) +
      geom_point(data = iris, aes(x = Petal.Length, y = Petal.Width), shape = 7, color = cluster$cluster)
```

![](iris_kmeans_files/figure-markdown_github/overlay%20of%20clustering%20results%20and%20actual%20(labelled)%20data-1.png)

``` r
fviz_cluster(cluster, data = iris[,1:4])
```

![](iris_kmeans_files/figure-markdown_github/representation%20of%20clustering%20results-1.png)

useful resources:
-----------------

pre-processing: <https://www.edupristine.com/blog/k-means-algorithm> k-means clustering intro: <https://uc-r.github.io/kmeans_clustering>
