test_that("in.box works", {
  # With numerical variable
  expect_setequal(which(in.box(1:10, list(c(5,7)))), 5:7)
  
  # With categorical variable
  expect_setequal(
    which(in.box(rep(letters[1:5], each = 3), list(c("b","e")))), 
    c(4:6, 13:15)
  )
  
  # With both
  x <- data.frame(V1 = 1:15, V2 = rep(letters[1:5], each = 3))
  lims <- list(c(5,13), c("b","e"))
  expect_setequal(which(in.box(x, lims)), c(5, 6, 13))
})

test_that("extract.box works", {
  x <- matrix(runif(200), ncol = 2)
  y <- 100*x[,1] + rnorm(100)
  extr <- 10
  peel_res <- peeling(y, as.data.frame(x))
  
  # Extraction using npeel
  expect_equal(
    extract.box(peel_res, npeel = extr)$npeel,
    extr
  )
  expect_equal(
    extract.box(peel_res, npeel = extr)$support,
    peel_res$support[extr + 1]
  )
  expect_equal(
    extract.box(peel_res, npeel = extr)$yfun,
    peel_res$yfun[extr + 1]
  )
  expect_equal(
    extract.box(peel_res, npeel = extr)$limits,
    peel_res$limits[extr + 1]
  )
  
  # Extraction using support
  expect_equal(
    extract.box(peel_res, support = peel_res$support[extr])$npeel,
    extr - 1
  )
  expect_equal(
    extract.box(peel_res, support = peel_res$support[extr])$support,
    peel_res$support[extr]
  )
  expect_equal(
    extract.box(peel_res, support = peel_res$support[extr])$limits,
    peel_res$limits[extr]
  )
  expect_equal(
    extract.box(peel_res, support = peel_res$support[extr])$yfun,
    peel_res$yfun[extr]
  )
  
  # Extraction using yfun
  expect_equal(
    extract.box(peel_res, yfun = peel_res$yfun[extr])$npeel,
    extr - 1
  )
  expect_equal(
    extract.box(peel_res, yfun = peel_res$yfun[extr])$support,
    peel_res$support[extr]
  )
  expect_equal(
    extract.box(peel_res, yfun = peel_res$yfun[extr])$limits,
    peel_res$limits[extr]
  )
  expect_equal(
    extract.box(peel_res, yfun = peel_res$yfun[extr])$yfun,
    peel_res$yfun[extr]
  )
})

test_that("construct_objfun works", {
  y <- 1:10
  ffun <- mean
  objfun <- construct_objfun(ffun)
  expect_equal(objfun(y, inbox = rep(T, 10)), 5.5)
  expect_equal(objfun(y, inbox = c(rep(T, 5), rep(F, 5))), 3)
  expect_equal(objfun(y, inbox = c(rep(T, 1), rep(F, 9))), 1)
  expect_equal(objfun(y, inbox = c(rep(F, 9), rep(T, 1))), 10)
  expect_equal(objfun(y, x = 11:20, inbox = rep(T, 10)), 5.5)
  
  ffun <- var
  objfun <- construct_objfun(ffun)
  expect_equal(objfun(y, inbox = rep(T, 10)), var(1:10))
  expect_equal(objfun(y, inbox = c(rep(T, 5), rep(F, 5))), var(1:5))
  
  # Function with y as the main argument
  ffun <- function(y) mean(y)
  objfun <- construct_objfun(ffun)
  expect_equal(objfun(y, inbox = rep(T, 10)), 5.5)
  expect_equal(objfun(y, inbox = c(rep(T, 5), rep(F, 5))), 3)
  
  # Function with y and x arguments
  ffun <- function(y, x) mean(y - x)
  objfun <- construct_objfun(ffun)
  expect_equal(objfun(y, data.frame(x = y / 2)), 2.75)
  expect_equal(objfun(y, data.frame(x = y / 2), inbox = rep(c(T, F), each = 5)), 
    1.5)
    
  # Already defined function
  ffun <- function(y, x, inbox){
    mean(y[inbox]) / mean(x[inbox,1])
  }
  objfun <- construct_objfun(ffun)
  expect_equal(ffun, objfun)
  expect_equal(objfun(y, data.frame(y / 2), rep(T, 10)), 2)
  expect_equal(objfun(y, data.frame(y + 2), rep(c(T,F), each = 5)), 0.6)
})