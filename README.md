# PvSTATEM - an R package for automated analysis of serological data

<!-- badges: start -->
[![R-CMD-check](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZetrextJG/PvSTATEM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### 🔴 *Pre-release* version



## Overview

This package is a simple tool, which handles the raw data of various formats, produced in Multiplex Bead Assay (MBA). In short, it reads the unstructured, raw data from e.g. Luminex device and output normalized and well-structured data which can be later used in more advanced, downstream analysis.

The package is in pre-release version, thus it lacks most of the functionalities. It is planned to be released by the end of September 2024.

The package includes 3 main steps of preprocessing the data:

1.  data reading and manipulation
2.  quality control
3.  data normalization

`PvSTATEM` package is developed within the project of the same name - [PvSTATEM](https://www.pvstatem.eu/), which is an international project which aims into malaria elimination.

## Installation

For now, the only way to install the (unreleased) package is to build it by hand. The easiest way to do that is using a simple command `install_github` available in `devtools` library.

``` r
library(devtools)
install_github("mini-pw/PvSTATEM")
```

First command loads the `devtools` library (you might need to install it first - using command `install_packages("devtools")`) and the second one sources the git repository with code of our package and automatically installs it.
