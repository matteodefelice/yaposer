# yaposer

<!-- badges: start -->
<!-- badges: end -->

The goal of `yaposer` is to help the user to manipulate and visualise inputs and outputs for the [YAPOS power system model](https://github.com/matteodefelice/yapos). This package is designed to support the user to work with the model in a consistent and error-proof workflow in particular when working with batch simulations.

`yaposer` provides the possibilities:

- load into the R workspace the input of a simulation from a folder or directly from the results of another simulation
- check the consistency of the inputs and carry out a simple yet useful pre-processing to detect potential issues in the input parameters
- write to disk a set of inputs for YAPOS starting from data structures in the R workspace
- visualise the results with an interactive Shiny application

## Installation

`yaposer` is not yet on [CRAN](https://CRAN.R-project.org), you can install it using the `remotes` package

``` r
remotes::install_github("matteodefelice/yaposer")
```

## Examples

Examples and tutorials are available on the YAPOS homepage. 
