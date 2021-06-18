test_that("Impact", {
  df1 = "./data/df1.xlsx"
  df2 = "./data/df2.xlsx"
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact(df1)$impact, c(1.00000000, 0.61538462, 0.00000000, 0.06538462))
  expect_equal(baseImpact(df2)$impact, c(1.00000000, 0.61538462, 0.00000000, 0.06538462, 0.9020979, 1.0000000, 0.7874126, 0.0000000))
  expect_equal(baseImpact(df3)$impact,
               (c(1.00000000, 0.61538462, 0.00000000, 0.06538462, 0.9020979, 1.0000000, 0.7874126, 0.0000000) + c(0.9219858, 0.2978723, 1.0000000, 0.0000000, 0.9661017, 0.5254237, 1.0000000, 0.0000000))/2 )
})


test_that("Weights result", {
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact(df3,weights = c(0.8, 0.2))$impact %>% round(7),
               c(0.9843972, 0.5518822, 0.2000000, 0.0523077, 0.9148987, 0.9050847, 0.8299301, 0.0000000))
})

test_that("Wrong weigths messages", {
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact(df3,weights = c(0.8, 0.3)) %>% unlist() %>% as.character(),"Weights have to sum up to 1")
  expect_equal(baseImpact(df3,weights = c(0.2, 0.3, 0.5)) %>% unlist() %>% as.character(), "Wrong number of weights is specified for path: 1")
})

test_that("Classes", {
  df2 = "./data/df2.xlsx"
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact(df2)$class, c(1, 2, 3, 1, 2, 1, 2, 4))
  expect_equal(baseImpact(df3)$class, c(1, 2, 1, 1, 2, 1, 1, 4))
})
