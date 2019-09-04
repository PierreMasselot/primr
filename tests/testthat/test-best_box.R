test_that("jump criterion finds a relevant box", {
  x <- runif(100)
  y <- 5*x + 10 * (x > .8) + rnorm(100)
  
  peel_res <- peeling(y, x, beta.stop = 0.1)
  best_box <- jump.prim(peel_res)
  expect_lt(abs(best_box$final.box$limits[[1]][1] - .8), 0.05)
})
