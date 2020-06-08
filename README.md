
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blb

<!-- badges: start -->

<!-- badges: end -->

The goal of blb is to utilize bootstrapping to create regression models,
specifically linear regression models or logistic regression models.
Users are also given the option to use more than one CPU when fitting a
model to the data.

## Installation

You can install the released version of blb from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("blb")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(blb)
```

``` r
#use iris dataset
library(datasets)
data(iris)
train1 = iris[1:75,]
test1 = iris[76:150,]

linear_regression_fit = blblm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = train1, m = 8, B = 100, parallel = TRUE, cl = 2)
```

``` r
library(titanic)

train2 = titanic_train[1:550,]
test2 = titanic_train[551:891,]
```

``` r
logistic_regression_fit = blb_logreg(Survived ~ Age + Sex + Pclass, data = train2, m = 8, B = 100, parallel = TRUE, cl = 2)
logistic_regression_fit
#> blb_logreg model: Survived ~ Age + Sex + Pclass
```
