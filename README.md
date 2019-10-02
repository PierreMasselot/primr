# primr
 Patient rule-induction method
 
## Description

The `primr` package provides function for performing the patient rule-induction method (PRIM) proposed by Friedman and Fisher (1999). PRIM is designed for bump hunting, i.e. to find a subdomain of `x` inputs in which an objective function of a response `y` is high.

## Installation

1. In R, install the package directly from github using the command (the package `devtools` is required):
```r
> library(devtools)
> install_github("PierreMasselot/primr", build = TRUE, 
  build_opts = c("--no-resave-data", "--no-manual"))
```
2. The package can then be loaded as usual: `library(primr)`.
3. You can see the vignette for simple examples: `vignette("toy_example")`.
4. You can see the list of functions in the file https://github.com/PierreMasselot/primr/blob/master/primr_0.0.0.9000.pdf or access help from R with `?peeling`.

## Functions

The `primr` package revolves around two main functions :
* `peeling`: Performs the top-down peeling consisting by iteratively peeling a box containing the whole dataset such that the objective function increases.
* `pasting`: Refines the final box's edges by slightly expanding it, increasing the objective function value.

Both function produce a `prim` object that contains the peeling trajectory, i.e. the successive peeled boxes. The stopping box of the peeling algorithm can be chosen through different functions:
* `jump.prim`: Selects the stopping box in a `prim` object through a 'jump' criterion.
* `cv.trajectory`: Produces a cross-validated peeling trajectory.
* `plot_trajectory`: Plots the peeling trajectory.

In addition, `prim` objects can be passed to several functions for analysis:
* `extract.box`: Extracts a particular box from a `prim` object.
* `plot_box`: Plots a bidimensional projection of the data with one or several boxes.
* `predict.prim`: For a new set of data, predicts whether each observation falls in the chosen box.

## References

Friedman, J.H., Fisher, N.I., 1999. Bump hunting in high-dimensional data. *Statistics and Computing* 9, 123-143. https://doi.org/10.1023/A:1008894516817

Masselot P., Chebana F., Campagna C., Lavigne E., Ouarda T.B.M.J., Gosselin P. On threshold identification for weather-health warning systems. *Submitted*.