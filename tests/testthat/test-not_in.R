test_that("values not in vector", {
  x = c(2,3,4)
  y = c(0,1,2)
  expect_equal(x[x %!in% y], c(3,4))
})
