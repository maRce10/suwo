library(testthat)
test_that("search Glaucis dohrnii audio", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "sound")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) == 22)

})

test_that("search Aristolochia baetica images", {

  df1 <- query_gbif(term = 'Aristolochia baetica', type =  "still image")

        expect_true(nrow(df1) >= 1456)

})


test_that("search Floractus heimi audio (no audios)", {

  df1 <- query_gbif(term = 'Floractus heimi', type =  "sound")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  29)

})



test_that("no result", {

  df1 <- query_gbif(term = 'asdasdasd', type =  "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})


test_that("search Glaucis photos (3 species)", {

  df1 <- query_gbif(term = 'Glaucis', type =  "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  714)

})


test_that("search Aristolochia baetica photos in parallel", {

  df1 <- query_gbif(term = 'Aristolochia baetica', type =  "still image", cores = 3)


  system(paste("firefox", df1$`media-URL`[1]))

  expect_true(nrow(df1) >= 2003)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_gbif(term = 'a3', type =  "sound", verbose = FALSE, pb = FALSE))


  # system(paste("firefox", df1$link[1]))

  expect_true(df1 == "")

})
