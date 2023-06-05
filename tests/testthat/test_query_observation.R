library(testthat)
test_that("search Glaucis dohrnii audio", {


  df1 <- query_observation(term = 'Glaucis dohrnii', type = "still image", token = token)


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >= 1)

})

test_that("search Aristolochia baetica images", {

  df1 <- query_observation(term = 'Aristolochia baetica', type = "still image", token = token)

  expect_true(nrow(df1) >= 1)

})


test_that("search Floractus heimi (no observations)", {

  df1 <- query_observation(term = 'Floractus heimi', type = "still image", token = token)


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})

test_that("no result", {

  df1 <- query_observation(term = 'asdasdasd', type = "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})


test_that("search Turdus grayi photos in parallel", {

  df1 <- query_observation(term = 'Serinus serinus', type = "sound", token = token, cores = 2)


  #system(paste("firefox", df1$`media-URL`[1]))

  expect_true(nrow(df1) >= 177)

})

test_that("test verbose FALSE", {

  df1 <- capture_output(query_observation(term = 'a3', verbose = FALSE, pb = FALSE))


  # system(paste("firefox", df1$link[1]))

  expect_true(df1 == "")

})


