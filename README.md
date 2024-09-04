# PvSTATEM - an R package for automated analysis of serological data

<!-- badges: start -->
[![R-CMD-check](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml)
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

For now, the only way to install the (unreleased) package is to build it by hand. The easiest way to do that is using a simple command `install_github` available in `devtools` library:

``` r
require(devtools)
install_github("mini-pw/PvSTATEM")
```

The first command loads the `devtools` library (you might need to install it first - using command `install_packages("devtools")`), and the second one sources the git repository with the code of our package and automatically installs it. Now, you are ready to use the package to read your files! 

The example use of the package and its functionalities can be found in the [vignettes](https://mini-pw.github.io/PvSTATEM/articles/example_script.html).
