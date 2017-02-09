
<!-- README.md is generated from README.Rmd. Please edit that file -->
ZeligChoice
===========

<a href="http://zeligproject.org"><img src="README_files/img/zelig.png" align="left" height="80" vspace="8" hspace="18"></a>

[![CRAN Version](http://www.r-pkg.org/badges/version/ZeligChoice)](http://cran.r-project.org/package=ZeligChoice) [![Travis-CI Build Status](https://travis-ci.org/IQSS/ZeligChoice.svg?branch=master)](https://travis-ci.org/IQSS/ZeligChoice) [![codecov](https://codecov.io/gh/IQSS/ZeligChoice/branch/master/graph/badge.svg)](https://codecov.io/gh/IQSS/ZeligChoice) [![Gitter chat](https://badges.gitter.im/Zelig-dev/gitter.png)](https://gitter.im/Zelig-dev/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link) [Dev-Blog](https://medium.com/zelig-dev)

**ZeligChoice** is a module with additional models for the [Zelig](https://github.com/IQSS/Zelig) statistical package in R.

Project page and publications available at: <http://zeligproject.org>

![Example Page](README_files/img/output2.png)

Use
---

ZeligChoice follows the same workflow as core [Zelig](https://github.com/IQSS/Zelig#zelig-5-workflow-overview).

Here us a simple example using the Zelig 4 wrappers (you can also use Zelig 5 reference classes):

``` r
# Load required packages
library(Zelig)
library(ZeligChoice)

# Load example data
data(mexico)

# Extimate model
z.out <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok,
               model = "mlogit", data = mexico)
```

    ## How to cite this model in Zelig:
    ##   Thomas W. Yee. 2007.
    ##   mlogit: Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values
    ##   in Christine Choirat, Christopher Gandrud, James Honaker, Kosuke Imai, Gary King, and Olivia Lau,
    ##   "Zelig: Everyone's Statistical Software," http://zeligproject.org/

``` r
# Set fitted values
x.out <- setx(z.out)

# Simulate quantities of interest
s.out <- sim(z.out, x.out)

# Plot results
plot(s.out)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Installation
------------

You can install ZeligChoice from CRAN or from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("IQSS/ZeligChoice")
```
