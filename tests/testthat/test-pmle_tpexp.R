test_that("test-pmle_tpexp", {
  x1 <- c(25,43,53,65,76,86,95,115,132,150)
  expect_equal(pmle_tpexp(x1), pmle_tpexp(x1,theta = 0,beta = 1))
})
