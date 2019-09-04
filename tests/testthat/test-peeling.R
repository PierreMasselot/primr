test_that("peeling numeric variables works", {
  x <- runif(100)
  y <- 5*x + rnorm(100)
  
  # General peeling
  peel_res <- peel(y, as.matrix(x), limits = list(c(0,1)))
  expect_equal(length(peel_res$y), 95)
  expect_equal(nrow(peel_res$x), 95)
  
  # Left peeling
  peel_res <- peel(y, as.data.frame(x), limits = list(c(0,1)), peeling.side = -1)
  expect_equal(peel_res$limits[[1]][1], sort(x)[6])
  expect_equal(peel_res$limits[[1]][2], max(x))
  
  # Right peeling
  peel_res <- peel(y, as.data.frame(x), limits = list(c(0,1)), peeling.side = 1)
  expect_equal(peel_res$limits[[1]][2], sort(x)[95])
  expect_equal(peel_res$limits[[1]][1], min(x))
  
  # Peeling two variables
  x <- as.data.frame(matrix(runif(200), ncol = 2))
  xlims <- lapply(x, range)
  y <- 1000*x[,1] + rnorm(100)
  
  peel_res <- peel(y, x, limits = xlims)
  xlims[[1]][1] <- sort(x[,1])[6]
  expect_equal(peel_res$limits, xlims)
})

test_that("peeling categorical variables works", {
  library(dlnm)
  nonadat <- na.omit(chicagoNMMAPS)
  y <- nonadat$death
  x <- nonadat[,"dow", drop = F]
  
  peel_res <- peel(y, x, limits = list(unique(x[,1])), numeric.vars = F)
  expect_equal(length(peel_res$limits[[1]]), 6)
  expect_setequal(peel_res$limits[[1]], 
    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
})

test_that("peeling sequence works", {
  x <- matrix(runif(200), ncol = 2)
  y <- 100 * x[,1] + rnorm(100)
  
  peel_res <- peeling(y, x)
  expect_gte(peel_res$yfun[peel_res$npeel], mean(y))
  expect_true(all(diff(peel_res$support) < 0))
  x1lims <- sapply(peel_res$limits, "[[", 1)
  expect_true(all(diff(x1lims[1,]) >= 0))
  expect_true(all(diff(x1lims[2,]) <= 0))
  x2lims <- sapply(peel_res$limits, "[[", 2)
  expect_true(all(diff(x2lims[1,]) >= 0))
  expect_true(all(diff(x2lims[2,]) <= 0))
})