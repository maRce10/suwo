test_that("map Helicobacter pylori", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = "Helicobacter pylori", format =  "image",
                           verbose = TRUE)

  skip_if(is.null(df1))

  df1$latitude[1] <- NA

  a <- map_locations(df1, cluster = TRUE)

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")
})


