test_that("search XC Phaethornis anthophilus default file names", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')[1:2, ]

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("XC532163.mp3", "XC568491.mp3"))
  })


###############

test_that("search wikiaves Glaucis dohrnii sp names", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type = "audio")[1:2, ]

    download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Glaucis-dohrnii-XC427418.mp3",
                      "Glaucis-dohrnii-XC618872.mp3"))
})


################


test_that("search GBIF sp names", {

  df1 <- query_gbif(term = 'Glaucis dohrnii')[1:2, ]

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir())

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Glaucis-dohrnii-XC427418.mp3",
                      "Glaucis-dohrnii-XC618872.mp3"))
})
