# quickblock

[![Build Status](https://travis-ci.org/fsavje/quickblock.svg?branch=master)](https://travis-ci.org/fsavje/quickblock)
[![Build status](https://ci.appveyor.com/api/projects/status/kvewap50vohivud8/branch/master?svg=true)](https://ci.appveyor.com/project/fsavje/quickblock/branch/master)
[![codecov](https://codecov.io/gh/fsavje/quickblock/branch/master/graph/badge.svg)](https://codecov.io/gh/fsavje/quickblock)

`quickblock` provides functions for constructing near-optimal threshold blockings.
The package is made with large data sets in mind and derives blockings more than
an order of magnitude quicker than other methods.


## Install quickblock

`quickblock` is currently under development and is not yet available on CRAN. The
development version and its dependencies can be installed with
[devtools](https://github.com/hadley/devtools) using the following code:
```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fsavje/quickblock")
```

The package contains compiled code, and you must have a development environment
to install the development version. (Use `devtools::has_devel()` to check whether
you do.) If no development environment exists, Windows users download and install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) and macOS users download
and install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835).

## Example

```R
# Construct example data
my_data <- data.frame(x1 = runif(100),
                      x2 = runif(100))

# Make distances
my_distances <- distances(my_data, dist_variables = c("x1", "x2"))

# Make blocking with at least two units in each block
quickblock(my_distances)

# Require at least three units in each block
quickblock(my_distances, size_constraint = 3)

# Impose caliper
quickblock(my_distances, caliper = 0.2)

# Call `quickblock` directly with covariate data (ie., not pre-calculating distances)
quickblock(my_data[c("x1", "x2")])

# Call `quickblock` directly with covariate data using Mahalanobis distances
quickblock(my_data[c("x1", "x2")], normalize = "mahalanobize")
```
