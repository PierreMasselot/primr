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

test_that("predict.prim works", {
  x <- matrix(runif(200), ncol = 2)
  y <- 100*x[,1] + rnorm(100)
  peel_res <- peeling(y, as.data.frame(x))
  
  x2 <- data.frame(1:10/10, runif(10))
  pred_res <- predict(peel_res, x = x2)
  expect_null(pred_res$yfun)
  expect_length(pred_res$support, peel_res$npeel + 1)
  
  y2 <- 100*x2[,1] + rnorm(10)
  pred_res2 <- predict(peel_res, x = x2, y = y2)
  expect_length(pred_res2$yfun, peel_res$npeel + 1)
  expect_length(pred_res2$support, peel_res$npeel + 1)
})