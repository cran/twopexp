test_that("test-cdfplot", {
  x <- seq(0,20,by=0.01)
  theta <- 6
  beta <- 2
  expect_equal(cdfplot(x,theta,beta), cdfplot(x,theta=6,beta=2))
})
