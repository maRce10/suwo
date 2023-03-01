library(testthat)
test_that("search Glaucis dohrnii audio", {

  df1 <- query_observation(term = 'Glaucis dohrnii')


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >= 1)

})

test_that("search Aristolochia baetica images", {

  df1 <- query_observation(term = 'Aristolochia baetica')

  expect_true(nrow(df1) >= 1)

})


test_that("search Floractus heimi (no observations)", {

  df1 <- query_observation(term = 'Floractus heimi')


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})

test_that("no result", {

  df1 <- query_observation(term = 'asdasdasd')


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})


test_that("search Glaucis photos (3 species)", {

  df1 <- query_observation(term = 'Glaucis')


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  9)

})


test_that("search candida photos in parallel", {

  df1 <- query_observation(term = 'candida', cores = 3)


  #system(paste("firefox", df1$`media-URL`[1]))

  expect_true(nrow(df1) >= 177)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_observation(term = 'a3', verbose = FALSE, pb = FALSE))


  # system(paste("firefox", df1$link[1]))

  expect_true(df1 == "")

})
