test_that("test-tpexp", {
  x <- rtpexp(10)
  expect_equal(dtpexp(x,theta=0,beta=1), dtpexp(x))
  expect_equal(ptpexp(x,theta=0,beta=1), ptpexp(x))
})
