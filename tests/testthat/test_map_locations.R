test_that("map Helicobacter pylori", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = "Helicobacter pylori", format =  "image")

  map_locations(df1)

  expect_true(nrow(df1) >= 1)

})


