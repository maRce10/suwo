test_that("Xenocanto Phaethornis anthophilus download", {

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

test_that("wikiaves Glaucis dohrnii sp download", {

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


test_that("search GBIF sp download", {

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

################


test_that("search inaturalist sp download", {

  df1 <- query_inaturalist(term = 'Agalychnis lemur', type = "still image")

  test_keys <- c("149945235", "170947000")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = ".jpeg$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("Agalychnis_lemur-INAT149945235.jpeg",
                      "Agalychnis_lemur-INAT170947000.jpeg"))
})

################

test_that("search inaturalist sp download", {

  df1 <- query_inaturalist(term = 'rattus rattus', type = "sound")

  test_keys <- c("169748642", "157450820")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = ".wav$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("rattus_rattus-INAT169748642-1.wav",
                      "rattus_rattus-INAT169748642-2.wav",
                      "rattus_rattus-INAT169748642-3.wav"))
})


################

test_that("search inaturalist sp download", {

  df1 <- query_gbif(term = 'rattus rattus', type = "sound")

  test_keys <- c("169748642", "157450820")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = ".wav$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("rattus_rattus-INAT169748642-1.wav",
                      "rattus_rattus-INAT169748642-2.wav",
                      "rattus_rattus-INAT169748642-3.wav"))
})
