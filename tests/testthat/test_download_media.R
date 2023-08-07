test_that("search XC Phaethornis anthophilus default file names", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')

  test_keys <- c("532163", "568491")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))
  expect_equal(fls, c("Phaethornis_anthophilus-XC532163.mp3", "Phaethornis_anthophilus-XC568491.mp3"))
  })


###############

test_that("search wikiaves Glaucis dohrnii sp names", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type = "sound")

  test_keys <- c("2286824", "4522545")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Glaucis_dohrnii-WA2286824.mp3",
                      "Glaucis_dohrnii-WA4522545.mp3"))
})


################


test_that("search GBIF sp names", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type = "sound")

  test_keys <- c("3863342525", "3863345521")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Glaucis_dohrnii-GBIF3863342525.mp3",
                      "Glaucis_dohrnii-GBIF3863345521.mp3"))
})
