
options(verbose = TRUE)

test_that("search Glaucis dohrnii sound reading existing file", {

  tf <- tempfile()

  write.csv(file = tf, x = suwo:::vignette_metadata$t_rufiventris[1:3, ])

  df1 <- query_macaulay(path = tempdir(), files = basename(tf))

  expect_true(nrow(df1) == 3)

})

test_that("search Glaucis dohrnii sound", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(species = 'Glaucis dohrnii',
                        format =  "sound",
                        path = tempdir())

  expect_true(nrow(df1) >= 12)

})

test_that("search Glaucis dohrnii images", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(species = 'Glaucis dohrnii',
                        format =  "image",
                        path = tempdir())

  expect_true(nrow(df1) >= 10)

})


test_that("paging by date", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(
    species = 'Calypte costae',
    format = "image",
    path = tempdir(),
    dates = c(2022, 2024, 2025)
  )

  expect_true(nrow(df1) >= 15000)

})


test_that("non-existing taxon", {

  df1 <- query_macaulay(species = "asdasdasd",
    format =  "image",
                        path = tempdir())

  expect_true(is.null(df1))

})

test_that("non-existing taxon", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(taxon_code = "coshum",
                        species = "asdasd",
                        format =  "image",
                        path = tempdir())

  expect_true(nrow(df1) == 10000)

})


