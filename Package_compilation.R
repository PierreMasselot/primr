library(usethis)
library(devtools)

pkg_path <- "C:/Users/masselpl/Documents/Recherche/2017-2019 - Post-doc/Programmes/R/primr"

# Initial creation
create_package(pkg_path)

setwd(pkg_path)

# Load all functions from package
load_all()

# Creation of the test directory
use_testthat()
use_test("peeling")
use_test("pasting")
use_test("useful_functions")
use_test("best_box")
# Test the functions
test()  # All

unit_test <- "pasting"
test_file(path = sprintf("%s/tests/testthat/test-%s.R",
  pkg_path, unit_test))  # A single file

# Creation of the vignette
use_vignette("Toy_example", title = "A basic primr example")
# Knitting the vignette
library(rmarkdown)
setwd(sprintf("%s/vignettes",pkg_path))
render("Toy_example.Rmd")
