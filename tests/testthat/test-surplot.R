test_that("test-surplot", {
  x <- seq(0,20,by=0.01)
  theta <- 8
  beta <- 1
  expect_equal(surplot(x,theta,beta), surplot(x,theta=8,beta=1))
})
