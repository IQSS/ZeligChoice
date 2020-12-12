
<!-- README.md is generated from README.Rmd. Please edit that file -->
ZeligChoice
===========

<a href="https://zeligproject.org"><img src="README_files/img/zelig.png" align="left" height="80" vspace="8" hspace="18"></a>

[![CRAN Version](https://www.r-pkg.org/badges/version/ZeligChoice)](https://cran.r-project.org/package=ZeligChoice) [![Travis-CI Build Status](https://travis-ci.org/IQSS/ZeligChoice.svg?branch=master)](https://travis-ci.org/IQSS/ZeligChoice) [![codecov](https://codecov.io/gh/IQSS/ZeligChoice/branch/master/graph/badge.svg)](https://codecov.io/gh/IQSS/ZeligChoice) [![Gitter chat](https://badges.gitter.im/Zelig-dev/gitter.png)](https://gitter.im/Zelig-dev/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link) [Dev-Blog](https://medium.com/zelig-dev)

**ZeligChoice** is a module with additional models for the [Zelig](https://github.com/IQSS/Zelig) statistical package in R.

Project page and publications available at: <https://zeligproject.org>

![Example Page](README_files/img/output2.png)

Use
---

**ZeligChoice** follows the same workflow as core [Zelig](https://github.com/IQSS/Zelig#zelig-5-workflow-overview).

Here us a simple example using the Zelig 4 wrappers (you can also use Zelig 5 reference classes). Note: installing and loading the **zeligverse** package will install and load all of the packages you need to use **ZeligChoice**.

``` r
# Load required packages
library(zeligverse)

# Load example data
data(mexico)

# Extimate model
z.out <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok,
               model = "mlogit", data = mexico, cite = FALSE)

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

You can install ZeligChoice from CRAN or, the developerment version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("IQSS/ZeligChoice")
```
