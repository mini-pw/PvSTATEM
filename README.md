# SerolyzeR - an R package for automated analysis of serological data

<!-- badges: start -->
[![R-CMD-check](https://github.com/mini-pw/SerolyzeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mini-pw/SerolyzeR/actions/workflows/R-CMD-check.yaml)
![Downloads](https://cranlogs.r-pkg.org/badges/SerolyzeR)
[![codecov](https://codecov.io/gh/mini-pw/SerolyzeR/graph/badge.svg?token=11EVHXMHDO)](https://app.codecov.io/gh/mini-pw/SerolyzeR)
<!-- badges: end -->


## Overview
This package is a simple tool that handles various raw data formats produced in Multiplex Bead Assay (MBA). In short, it reads the unstructured, raw data from, e.g., the Luminex device and outputs normalised and well-structured data, which can be used later in more advanced downstream analysis.

The package includes three main steps for preprocessing the data:

1.  data reading and manipulation
2.  quality control
3.  data normalisation


The graphical overview of the package can be seen in the image below:
![overview](https://github.com/mini-pw/SerolyzeR/blob/main/inst/img/overview.png)

`SerolyzeR` package is developed within the project [PvSTATEM](https://www.pvstatem.eu/), an international project aiming at malaria elimination and is financially supported by the HORIZON grant HORIZON-WIDERA-2022-ACCESS-07-01.


## Installation

The easiest way to install the package is using the CRAN repository:
``` r
install.packages("SerolyzeR")
require(SerolyzeR) # load the installed package
```
Now, you are ready to use the package to read your files!

Please note that since uploading the package to the CRAN repository requires the volunteers' time to manually run checks on the packages, the **package version currently released on CRAN might not be the latest**.

The package is under heavy development, with new features being released weekly. Therefore, if you'd like to test the latest package functionalities, we recommend installing it in the development version. It can be done using a simple command `install_github` available in the `devtools` library:

``` r
require(devtools)
install_github("mini-pw/SerolyzeR")
require(SerolyzeR) # load the installed package
```

The first command loads the `devtools` library (you might need to install it first - using the command `install_packages("devtools")`), and the second one sources the git repository with the code of our package and automatically installs it.

## Examples and instructions

The example use of the package and its functionalities can be found in [the vignettes](https://mini-pw.github.io/SerolyzeR/articles/example_script.html).
For more detailed documentation, check [the package website](https://mini-pw.github.io/SerolyzeR/).


## Contributing and issues

As a project in the development phase, we are open to any suggestions, bug reports, and contributions. If you have any ideas or issues, please report them in the [Issues](https://github.com/mini-pw/SerolyzeR/issues) section. Our team of developers will address them as soon as possible.
