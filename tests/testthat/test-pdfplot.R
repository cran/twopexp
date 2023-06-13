test_that("test-pdfplot", {
  x <- seq(0,20,by=0.01)
  theta <- 6
  beta <- 2
  expect_equal(pdfplot(x,theta,beta), pdfplot(x,theta=6,beta=2))
})
