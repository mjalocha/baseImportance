test_that("decreasing importance", {
  test1 = c(0)
  test2 = c(0.2)

  expect_equal(decreasing_importance(test1, 0), 0)
  expect_equal(decreasing_importance(test2, c(0.05, 0.08, 0.03)), 0.26416667)
})
