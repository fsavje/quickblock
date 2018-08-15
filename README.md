# quickblock

[![CRAN Status](https://www.r-pkg.org/badges/version/quickblock)](https://cran.r-project.org/package=quickblock)
[![Build Status](https://travis-ci.org/fsavje/quickblock.svg?branch=master)](https://travis-ci.org/fsavje/quickblock)
[![Build status](https://ci.appveyor.com/api/projects/status/kvewap50vohivud8/branch/master?svg=true)](https://ci.appveyor.com/project/fsavje/quickblock/branch/master)
[![codecov](https://codecov.io/gh/fsavje/quickblock/branch/master/graph/badge.svg)](https://codecov.io/gh/fsavje/quickblock)

`quickblock` provides functions for assigning treatments in randomized experiments using near-optimal threshold blocking. The package is made with large data sets in mind and derives blocks more than an order of magnitude quicker than other methods.


## How to install

`quickblock` is on CRAN and can be installed by running:

```R
install.packages("quickblock")
```


## How to install development version

It is recommended to use the stable CRAN version, but the latest development version can be installed directly from Github using [devtools](https://github.com/hadley/devtools):

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fsavje/quickblock")
```

The package contains compiled code, and you must have a development environment to install the development version. (Use `devtools::has_devel()` to check whether you do.) If no development environment exists, Windows users download and install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and macOS users download and install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835).


## Example on how to use quickblock

```R
# Load package
library("quickblock")

# Construct example data
my_data <- data.frame(x1 = runif(100),
                      x2 = runif(100))

# Make distances to be used when making blocking
my_distances <- distances(my_data, dist_variables = c("x1", "x2"))

# Make blocking with at least four units in each block
my_blocking <- quickblock(my_distances, size_constraint = 4L)

# Two treatment conditions
my_treatments <- assign_treatment(my_blocking, treatments = c("T", "C"))

# Run experiment
my_outcomes <- my_data$x1 + (my_treatments == "T") * my_data$x2 + rnorm(100)

# Estimate treatment effects and variance
blocking_estimator(my_outcomes, my_blocking, my_treatments)
```
