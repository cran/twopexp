test_that("test-qm_tpexp", {
  x1 <- c(25,43,53,65,76,86,95,115,132,150)
  expect_equal(qm_tpexp(x1,"Q13"),qm_tpexp(x1))
})
