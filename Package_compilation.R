library(usethis)

# Initial creation
create_package("C:/Users/masselpl/Documents/Recherche/2017-2019 - Post-doc/Programmes/R/primr")

setwd("C:/Users/masselpl/Documents/Recherche/2017-2019 - Post-doc/Programmes/R/primr")

# Creation of the test directory
use_testthat()
use_test("peeling")
use_test("pasting")
use_test("useful_functions")
use_test("best_box")

# Creation of the vignette
use_vignette("toy_example")