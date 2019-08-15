library(usethis)
library(devtools)

# Initial creation
pkg_path <- "C:/Users/masselpl/Documents/Recherche/2017-2019 - Post-doc/Programmes/R/primr"
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
setwd(sprintf("%s/tests/testthat",pkg_path))
test_file(path = "test-pasting.R")  # A single file

# Creation of the vignette
use_vignette("Toy_example", title = "A basic primr example")
# Knitting the vignette
library(rmarkdown)
setwd(sprintf("%s/vignettes",pkg_path))
render("Toy_example.Rmd")
