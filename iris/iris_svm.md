Iris dataset support vector machines clustering and visualization
================
Madlen Wilmes
2018-11-05

Purpose of the script
---------------------

Use iris data set to explore clustering via support vector machines (SVM), multiclass SVM and Support Vector Clustering (SVC).

A bit of theory
---------------

-   useful for labeled data with unknown distribution
-   linear SVM is rarely used as real world data is usually more messy than two classes
-   linear SVM comprable to linear regression, non-linear SVM comparable to logistic regression
-   for linear, binary data, one chooses a classifier line (y = ax + b) and make it equidistant from data points closest to the line of either side. Maximize the margin (m), so data in the test case can be classified even if farther away from training test cluster.

Caution: - SVM is prone to overfitting

![Linear SVM](LinearSVM.jpg)

Support vector clustering has the following idea: let us transform the points from their space to a higher dimensionality feature space. Find a minimal enclosing sphere in this feature space. In the original space, the sphere becomes a set of disjoing regions. Each region becomes a cluster. (There are also important details like how we choose the feature space, and how we do the transformation, and how we define disjoint regions.)

These disjoint regions are completely different from the regions around K centers in the K-means algorithm. For example, in 2 dimensions, the K-means regions are polygons. The regions of SVC are amoeba-like areas.

Load Data
---------

``` r
data(iris)
attach(iris)
```

Prep data set
-------------

``` r
## split ramdomly into training and test sets
iris[,"train"] <- ifelse(runif(nrow(iris)) < 0.8, 1, 0)
## separate training and test sets by using the flag culumn "train"
trainset <- iris[iris$train == 1,]
testset <- iris[iris$train == 0,]
## remove training flag column
trainset <- subset(trainset, select = -c(train))
testset <- subset(testset, select = -c(train))
```

``` r
## build model – linear kernel and C-classification (soft margin) with default cost (C = 1)
## predict Species, using all data points (signified by the dot)
svm_model <- svm(Species ~ ., data = trainset, kernel ="radial")
summary(svm_model)
```

    ## 
    ## Call:
    ## svm(formula = Species ~ ., data = trainset, kernel = "radial")
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ##       gamma:  0.25 
    ## 
    ## Number of Support Vectors:  42
    ## 
    ##  ( 6 18 18 )
    ## 
    ## 
    ## Number of Classes:  3 
    ## 
    ## Levels: 
    ##  setosa versicolor virginica

plot results of training model
------------------------------

``` r
plot(svm_model, data = trainset, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
```

![](iris_svm_files/figure-markdown_github/plot%20training%20model%20for%20two%20traits,%20keep%20other%20two%20constant-1.png)

``` r
pred_train <- predict(svm_model, trainset)
## test set predictions
pred_test <- predict(svm_model, testset)
```

Confusion matrix and Misclassification error
--------------------------------------------

``` r
accu_tab <- table(Predicted = pred_test, Real = testset$Species)
accu_tab
```

    ##             Real
    ## Predicted    setosa versicolor virginica
    ##   setosa          9          0         0
    ##   versicolor      0         10         0
    ##   virginica       0          1         9

``` r
## misclassification rate
1 - sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.03448276

``` r
## classification accuracy
sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.9655172

Misclasification rate = 1 /30 = 0.033 Classification accuracy = 29 / 30 = 0.967

Model Tuning (hyperparameter optimization)
------------------------------------------

``` r
set.seed(42)
## don't run more combinations than your machine can handle (with large data sets)
tune_model <- tune(svm, Species~.,
                   data = trainset, 
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:7))
                   )
## darker regions mean better results
plot(tune_model)
```

![](iris_svm_files/figure-markdown_github/tune%20svm%20model-1.png)

``` r
summary(tune_model)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  epsilon cost
    ##        0    4
    ## 
    ## - best performance: 0.03269231 
    ## 
    ## - Detailed performance results:
    ##    epsilon cost      error dispersion
    ## 1      0.0    4 0.03269231 0.04224613
    ## 2      0.1    4 0.03269231 0.04224613
    ## 3      0.2    4 0.03269231 0.04224613
    ## 4      0.3    4 0.03269231 0.04224613
    ## 5      0.4    4 0.03269231 0.04224613
    ## 6      0.5    4 0.03269231 0.04224613
    ## 7      0.6    4 0.03269231 0.04224613
    ## 8      0.7    4 0.03269231 0.04224613
    ## 9      0.8    4 0.03269231 0.04224613
    ## 10     0.9    4 0.03269231 0.04224613
    ## 11     1.0    4 0.03269231 0.04224613
    ## 12     0.0    8 0.03269231 0.04224613
    ## 13     0.1    8 0.03269231 0.04224613
    ## 14     0.2    8 0.03269231 0.04224613
    ## 15     0.3    8 0.03269231 0.04224613
    ## 16     0.4    8 0.03269231 0.04224613
    ## 17     0.5    8 0.03269231 0.04224613
    ## 18     0.6    8 0.03269231 0.04224613
    ## 19     0.7    8 0.03269231 0.04224613
    ## 20     0.8    8 0.03269231 0.04224613
    ## 21     0.9    8 0.03269231 0.04224613
    ## 22     1.0    8 0.03269231 0.04224613
    ## 23     0.0   16 0.03269231 0.04224613
    ## 24     0.1   16 0.03269231 0.04224613
    ## 25     0.2   16 0.03269231 0.04224613
    ## 26     0.3   16 0.03269231 0.04224613
    ## 27     0.4   16 0.03269231 0.04224613
    ## 28     0.5   16 0.03269231 0.04224613
    ## 29     0.6   16 0.03269231 0.04224613
    ## 30     0.7   16 0.03269231 0.04224613
    ## 31     0.8   16 0.03269231 0.04224613
    ## 32     0.9   16 0.03269231 0.04224613
    ## 33     1.0   16 0.03269231 0.04224613
    ## 34     0.0   32 0.04102564 0.05845492
    ## 35     0.1   32 0.04102564 0.05845492
    ## 36     0.2   32 0.04102564 0.05845492
    ## 37     0.3   32 0.04102564 0.05845492
    ## 38     0.4   32 0.04102564 0.05845492
    ## 39     0.5   32 0.04102564 0.05845492
    ## 40     0.6   32 0.04102564 0.05845492
    ## 41     0.7   32 0.04102564 0.05845492
    ## 42     0.8   32 0.04102564 0.05845492
    ## 43     0.9   32 0.04102564 0.05845492
    ## 44     1.0   32 0.04102564 0.05845492
    ## 45     0.0   64 0.04935897 0.05789376
    ## 46     0.1   64 0.04935897 0.05789376
    ## 47     0.2   64 0.04935897 0.05789376
    ## 48     0.3   64 0.04935897 0.05789376
    ## 49     0.4   64 0.04935897 0.05789376
    ## 50     0.5   64 0.04935897 0.05789376
    ## 51     0.6   64 0.04935897 0.05789376
    ## 52     0.7   64 0.04935897 0.05789376
    ## 53     0.8   64 0.04935897 0.05789376
    ## 54     0.9   64 0.04935897 0.05789376
    ## 55     1.0   64 0.04935897 0.05789376
    ## 56     0.0  128 0.06602564 0.05251805
    ## 57     0.1  128 0.06602564 0.05251805
    ## 58     0.2  128 0.06602564 0.05251805
    ## 59     0.3  128 0.06602564 0.05251805
    ## 60     0.4  128 0.06602564 0.05251805
    ## 61     0.5  128 0.06602564 0.05251805
    ## 62     0.6  128 0.06602564 0.05251805
    ## 63     0.7  128 0.06602564 0.05251805
    ## 64     0.8  128 0.06602564 0.05251805
    ## 65     0.9  128 0.06602564 0.05251805
    ## 66     1.0  128 0.06602564 0.05251805

retrieve model with 'best' parameters
-------------------------------------

``` r
## grab the optimized model
opti_model <- tune_model$best.model
summary(opti_model)
```

    ## 
    ## Call:
    ## best.tune(method = svm, train.x = Species ~ ., data = trainset, 
    ##     ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:7)))
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  4 
    ##       gamma:  0.25 
    ## 
    ## Number of Support Vectors:  33
    ## 
    ##  ( 6 14 13 )
    ## 
    ## 
    ## Number of Classes:  3 
    ## 
    ## Levels: 
    ##  setosa versicolor virginica

``` r
plot(opti_model, data = trainset, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
```

![](iris_svm_files/figure-markdown_github/new%20prediction%20with%20optimized%20model%20parameters-1.png)

### re-run on training set

``` r
pred_train <- predict(opti_model, trainset)
accu_tab <- table(Predicted = pred_train, Real = trainset$Species)
accu_tab
```

    ##             Real
    ## Predicted    setosa versicolor virginica
    ##   setosa         41          0         0
    ##   versicolor      0         37         0
    ##   virginica       0          2        41

``` r
## misclassification rate
1 - sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.01652893

``` r
## classification accuracy
sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.9834711

### re-run on test set

``` r
pred_test <- predict(opti_model, testset)
accu_tab <- table(Predicted = pred_test, Real = testset$Species)
accu_tab
```

    ##             Real
    ## Predicted    setosa versicolor virginica
    ##   setosa          9          0         0
    ##   versicolor      0         10         0
    ##   virginica       0          1         9

``` r
## misclassification rate
1 - sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.03448276

``` r
## classification accuracy
sum(diag(accu_tab)) / sum(accu_tab)
```

    ## [1] 0.9655172
