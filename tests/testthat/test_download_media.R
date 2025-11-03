## Xeno-Canto
test_that("Xenocanto Phaethornis anthophilus download all.data  = FALSE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  xc1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  test_keys <- c("532163", "568491")

  sxc1 <- subset(xc1, key %in% test_keys)

  a <- download_media(metadata = sxc1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Phaethornis_anthophilus-XC532163.mp3",
    "Phaethornis_anthophilus-XC568491.mp3"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$download_file_name %in% expected_files))

})

## Xeno-Canto
test_that("Xenocanto Phaethornis anthophilus download folder_by", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  xc1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  test_keys <- c("532163", "568491")

  sxc1 <- subset(xc1, key %in% test_keys)

  a <- download_media(metadata = sxc1,
                      path = tempdir(),
                      folder_by = "date")

  fls <- list.files(path = tempdir(),
                    pattern = "mp3$",
                    recursive = TRUE)

  # remove filess
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "2020-03-05/Phaethornis_anthophilus-XC532163.mp3",
    "2020-06-14/Phaethornis_anthophilus-XC568491.mp3"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$download_file_name %in% expected_files))

})

test_that("Xenocanto Phaethornis anthophilus download all.data  = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  xc2 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  test_keys <- c("532163", "568491")

  sxc2 <- subset(xc2, key %in% test_keys)

  a <- download_media(metadata = sxc2, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Phaethornis_anthophilus-XC532163.mp3",
    "Phaethornis_anthophilus-XC568491.mp3"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(
    a$download_file_name %in% c(
      "Phaethornis_anthophilus-XC532163.mp3",
      "Phaethornis_anthophilus-XC568491.mp3"
    )
  ))

})

## wikiaves

test_that("wikiaves Glaucis dohrnii sp download, all.data = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(interactive())

  wa1 <- query_wikiaves(species = 'Glaucis dohrnii',
                        format = "sound",
                        all_data = TRUE)

  test_keys <- c("2286824", "4522545")

  swa1 <- subset(wa1, key %in% test_keys)

  a <- download_media(metadata = swa1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  expected_files <- c("Glaucis_dohrnii-WA2286824.mp3",
                      "Glaucis_dohrnii-WA4522545.mp3")

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$download_file_name %in% expected_files))

})

test_that("wikiaves Urubitinga solitaria sp download image all.data = FALSE",
          {
            skip_on_cran()
            skip_if_offline()

            wa2 <- query_wikiaves(species = 'Urubitinga solitaria',
                                  format = "image",
                                  all_data = FALSE)

            test_keys <- c("3227223", "5415237")

            swa2 <- subset(wa2, key %in% test_keys)

            a <- download_media(metadata = swa2, path = tempdir())

            fls <- list.files(path = tempdir(), pattern = "jpeg$")

            # remove files
            unlink(file.path(tempdir(), fls))

            # expected files
            expected_files <- c("Urubitinga_solitaria-WA3227223.jpeg",
                                "Urubitinga_solitaria-WA5415237.jpeg")

            # test
            expect_true(all(expected_files %in% fls))

            expect_true(all(a$download_file_name %in% expected_files))

          })

### GBIF

test_that("search GBIF sp download image all_data = TRUE", {
  skip_on_cran()
  skip_if_offline()

  gb1 <- query_gbif(species = 'Glaucis dohrnii',
                    format = "image",
                    all_data = TRUE)

  test_keys <- c("5154503342", "4525343483")

  sgb1 <- subset(gb1, key %in% test_keys)

  a <- download_media(metadata = sgb1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "jpeg$|png$")

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  expected_files <- c("Glaucis_dohrnii-GBIF5154503342.png",
                      "Glaucis_dohrnii-GBIF4525343483.jpeg")

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$download_file_name %in% expected_files))

})

test_that("search GBIF sp download sound all_data = FALSE", {
  skip_on_cran()
  skip_if_offline()

  gb2 <- query_gbif(species = 'Glaucis dohrnii',
                    format = "sound",
                    all_data = FALSE)

  test_keys <- c("3863342525", "2243728561")

  sgb2 <- subset(gb2, key %in% test_keys)

  a <- download_media(metadata = sgb2, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  expected_files <- c("Glaucis_dohrnii-GBIF3863342525.mp3",
                      "Glaucis_dohrnii-GBIF2243728561.mp3")

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$download_file_name %in% expected_files))
})

## iNaturalist

test_that("search inaturalist sp download all_data = TRUE", {
  skip_on_cran()
  skip_if_offline()

  in1 <- query_inaturalist(species = 'Agalychnis lemur',
                           format = "image",
                           all_data = TRUE)

  test_keys <- c("303641298", "303641290")

  sin1 <- subset(in1, key %in% test_keys)

  a <- download_media(metadata = sin1, path = tempdir())

  fls <- list.files(path = tempdir(),
                    pattern = ".jpeg$",
                    ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  exp_files <- c(
    "Agalychnis_lemur-INAT303641290-1.jpeg",
    "Agalychnis_lemur-INAT303641290-2.jpeg",
    "Agalychnis_lemur-INAT303641298-1.jpeg",
    "Agalychnis_lemur-INAT303641298-2.jpeg",
    "Agalychnis_lemur-INAT303641298-3.jpeg"
  )

  expect_true(all(exp_files %in% fls))
  expect_true(all(a$download_file_name %in% exp_files))

})

test_that("search inaturalist sp download sound all_data = FALSE", {
  skip_on_cran()
  skip_if_offline()

  in2 <- query_inaturalist(species = "Rattus rattus",
                           format = "sound",
                           all_data = FALSE)

  test_keys <- c("283643216", "281653293")

  sin2 <- subset(in2, key %in% test_keys)

  a <- download_media(metadata = sin2, path = tempdir())

  fls <- list.files(path = tempdir(),
                    pattern = ".mp3$|m4a$",
                    ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c("Rattus_rattus-INAT281653293.m4a",
                      "Rattus_rattus-INAT283643216.mp3")

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$download_file_name %in% fls))

})



##  Macaulay
test_that("search macaulay sp download all_data = TRUE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml1 <- query_macaulay(
    species = 'Glaucis dohrnii',
    format = "sound",
    all_data = TRUE,
    path = tempdir()
  )

  test_keys <- c(627919111, 624733750)

  sml1 <- subset(ml1, key %in% test_keys)

  a <- download_media(metadata = sml1, path = tempdir())

  fls <- list.files(path = tempdir(),
                    pattern = ".mp3$|m4a$",
                    ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c("Glaucis_dohrnii-ML627919111.mp3",
                      "Glaucis_dohrnii-ML624733750.mp3")

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$download_file_name %in% fls))

})


test_that("search macaulay Harpia harpyja download sound all_data = FALSE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml2 <- query_macaulay(
    species = 'Harpia harpyja',
    format = "image",
    all_data = FALSE,
    path = tempdir()
  )

  test_keys <- c("639757637", "639955653")

  sml2 <- subset(ml2, key %in% test_keys)

  a <- download_media(metadata = sml2, path = tempdir())

  fls <- list.files(path = tempdir(),
                    pattern = "jpeg$",
                    ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c("Harpia_harpyja-ML639757637.jpeg",
                      "Harpia_harpyja-ML639955653.jpeg")

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$download_file_name %in% fls))

})


test_that("search macaulay sp download video all_data = FALSE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml3 <- query_macaulay(
    species = 'Calypte anna',
    format = "video",
    all_data = FALSE,
    path = tempdir()
  )

  test_keys <- c("639261756", "639255345")

  sml3 <- subset(ml3, key %in% test_keys)

  a <- download_media(metadata = sml3, path = tempdir())

  fls <- list.files(path = tempdir(),
                    pattern = ".mp4$",
                    ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c("Calypte_anna-ML639261756.mp4",
                      "Calypte_anna-ML639255345.mp4")

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$download_file_name %in% fls))

})
