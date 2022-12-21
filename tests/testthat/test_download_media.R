test_that("search XC Phaethornis anthophilus default file names", {
a
  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')[1:2, ]

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("XC532163.mp3", "XC568491.mp3"))
  })


###############

test_that("search XC Phaethornis anthophilus sp names", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')[1:2, ]

    download_media(metadata = df1, path = tempdir(), file.name = c("genus", "specific.epithet"))

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Phaethornis-anthophilus-XC532163.mp3",
                      "Phaethornis-anthophilus-XC568491.mp3"))
})

#######

test_that("search WA Phaethornis anthophilus default file names", {

  df1 <- query_wikiaves(term = 'Phaethornis pretei', type = "audio")
  # [1:2, ]

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expect_equal(fls, c("XC532163.mp3", "XC568491.mp3"))
})


###############

test_that("search Phaethornis anthophilus sp names", {

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus')[1:2, ]

  download_media(metadata = df1, path = tempdir(), file.name = c("genus", "specific.epithet"))

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Phaethornis-anthophilus-XC532163.mp3",
                      "Phaethornis-anthophilus-XC568491.mp3"))
})
