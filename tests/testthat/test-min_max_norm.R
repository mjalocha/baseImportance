test_that("normalize data", {
  x = c(2,3,4,20,15)
  expect_equal(min_max_norm(x), c(0, 1/18, 2/18, 1, 13/18))
})
