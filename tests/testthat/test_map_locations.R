options(verbose = TRUE)


test_that("map Helicobacter pylori", {
  df1 <- suwo:::vignette_metadata$t_rufiventris

  a <- map_locations(df1, cluster = TRUE)

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")
})
