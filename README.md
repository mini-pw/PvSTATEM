# PvSTATEM - an R package for automated analysis of serological data

<!-- badges: start -->
[![R-CMD-check](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml)
![Downloads](https://cranlogs.r-pkg.org/badges/PvSTATEM)
<!-- badges: end -->

### 🟡 *Semi-release* version



## Overview
This package is a simple tool that handles various formats of raw data produced in Multiplex Bead Assay (MBA). In short, it reads the unstructured, raw data from, e.g., the Luminex device and outputs normalised and well-structured data, which can be used later in more advanced downstream analysis.

The package is in a pre-release version. Thus, it lacks most of the functionalities. It is planned to be released by the end of September 2024.

The package includes three main steps for preprocessing the data:

1.  data reading and manipulation
2.  quality control
3.  data normalisation

`PvSTATEM` package is developed within the project of the same name - [PvSTATEM](https://www.pvstatem.eu/), an international project aiming at malaria elimination.

## Installation

The easiest way to install the package is using the CRAN repository:
``` r
install.packages("PvSTATEM")
require(PvSTATEM) # load the installed package
```

Now, you are ready to use the package to read your files! 

Optionally, it is possible to build and install the package by hand. It can be done using a simple command `install_github` available in `devtools` library:

``` r
require(devtools)
install_github("mini-pw/PvSTATEM")
require(PvSTATEM) # load the installed package
```

The first command loads the `devtools` library (you might need to install it first - using command `install_packages("devtools")`), and the second one sources the git repository with the code of our package and automatically installs it. 

## Examples and instructions

The example use of the package and its functionalities can be found in [the vignettes](https://mini-pw.github.io/PvSTATEM/articles/example_script.html).
For more detailed documentation of the package, check [the package website](https://mini-pw.github.io/PvSTATEM).
