test_that("Base Impact Combined", {
  df2 = "./data/df2.xlsx"
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact_combined(df2, c(102,103), 104)$impact, c(0.95104895, 0.80769231, 0.39370630, 0.03269231))
  expect_equal(baseImpact_combined(df3, c(102,103), 104)$impact,c(0.94754635, 0.60967015, 0.69685315, 0.01634616))
})

test_that("Base Impact Combined weights", {
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact_combined(df3, c(102,103), 104, weights_models = c(0.8,0.2))$impact %>% round(8),
               c(0.95561429, 0.51784516, 0.57874126, 0.02615385))
})

test_that("Base impact wrong weigths", {
  df2 = "./data/df2.xlsx"
  df3 = "./data/df3.xlsx"
  expect_equal(baseImpact_combined(df3, c(102,103), 104, weights_models = c(0.7,0.2)) %>% unlist() %>% as.character(),"Weights have to sum up to 1")
  expect_equal(baseImpact_combined(df3, c(102,103), 104, weights_models = c(0.7,0.2,0.1)) %>% unlist() %>% as.character(),"You need to specify weight for each model")
  expect_equal(baseImpact_combined(df3, c(102,106), 104, weights_models = c(0.7,0.3)) %>% unlist() %>% as.character(),"Not all model_ids are available in the imported files")
  expect_equal(baseImpact_combined(c(df3,df3), c(102), 104, weights_metrics = list(c(0.7,0.3), c(1))),paste0("Wrong number of weights is specified for path: ", 2))
})
