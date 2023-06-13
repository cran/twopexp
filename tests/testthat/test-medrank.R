test_that("test-medrank", {
  x1 <- c(25,43,53,65,76,86,95,115,132,150)
  expect_equal(medrank(x1,"B"), medrank(x1))
})
