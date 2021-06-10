test_that("Base Impact Combined", {
  df2 = "./data/df2.xlsx"
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact_combined(df2, c(102,103), 104)$impact, c(0.95104895, 0.80769231, 0.39370630, 0.03269231))
  expect_equal(baseImpact_combined(df3, c(102,103), 104)$impact,c(0.94754635, 0.60967015, 0.69685315, 0.01634616))
})
