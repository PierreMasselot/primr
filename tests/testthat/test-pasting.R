test_that("pasting numerical variables works", {
  x <- data.frame(V1 = runif(100))
  y <- 5*x[[1]] + rnorm(100)
  small.box <- list(c(.1, .2))
  past_res <- pasting(y, x, alpha = .2, small.box = small.box)
  # Check that 2 obs have been added
  expect_equal(length(past_res$y), sum(x >= .1 & x <= .2) + 2) 
  
  # Check that it had been added on the right
  past_res <- pasting(y, x, alpha = .2, small.box = small.box, 
    peeling.side = 1)
  expect_equal(past_res$limits[[1]][1], min(x[x >= .1]))
  expect_gt(past_res$limits[[1]][2], .2)
  
  # Check that it had been added on the left
  past_res <- pasting(y, x, alpha = .2, small.box = small.box, 
    peeling.side = -1)
  expect_equal(past_res$limits[[1]][2], max(x[x <= .2]))
  expect_lt(past_res$limits[[1]][1], .1)
})

test_that("pasting categorical variables works", {
  x <- data.frame(V1 = sample(letters[1:5], 100, replace = T))
  y <- rnorm(100)
  lettermeans <- aggregate(y, by = x, mean)
  worstletter <- as.character(lettermeans[which.min(lettermeans[,2]),1])
  bestletter <- as.character(lettermeans[which.max(lettermeans[,2]),1])
  past_res <- pasting(y, x, small.box = list(worstletter), numeric.vars = F)
  # Check that limits now contains the new best letter
  expect_setequal(past_res$limits[[1]], c(worstletter, bestletter))
  expect_gt(length(past_res$y), sum(x == worstletter)) 
})

test_that("sequence pasting works", {
  x <- runif(100)
  y <- -10 * abs(x - .5)
  small.box <- list(c(.1, .2))
  
  past_res <- pasting.sequence(y, x, small.box)
  expect_equal(length(past_res$limits), past_res$npaste + 1)
  expect_equal(length(past_res$yfun), past_res$npaste + 1)
  expect_equal(length(past_res$support), past_res$npaste + 1)
  expect_gte(past_res$limits[[past_res$npaste]][[1]][1], .1)
})