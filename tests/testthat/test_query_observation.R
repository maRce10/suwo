#
# test_that("search Aristolochia baetica images", {
#
# skip_on_cran()
# skip_if_offline()
#
#   df1 <- query_observation(species = 'Aristolochia baetica',
#   format = "image", token = token)
#
#   expect_true(nrow(df1) >= 1)
#
# })
#
#
# test_that("search Floractus heimi (no observations)", {
#
## skip_on_cran()
# skip_if_offline()
#
#  df1 <- query_observation(species = 'Floractus heimi', format = "image",
#  token = token)
#
#   expect_true(is.null(df1))
#
# })

 test_that("no result", {

   skip_on_cran()
   skip_if_offline()

  expect_null(query_observation(species = 'asdasdasd', format = "image"))

})

test_that("no result sound", {

  skip_on_cran()
  skip_if_offline()

  expect_null(query_observation(species = 'asdasdasd', format = "sound"))

})

test_that( "Invalid token for observation.org", {

  skip_on_cran()
  skip_if_offline()

  expect_null(query_observation(species = 'Serinus serinus', format = "sound"))

})
#
#
# test_that("search Turdus grayi photos in parallel", {
#
# skip_on_cran()
# skip_if_offline()

#     df1 <- query_observation(species = 'Serinus serinus', format = "sound",
#     token = token, cores = 2)
#
#   expect_true(nrow(df1) >= 177)
#
# })
#
# test_that("test verbose FALSE", {
#
# skip_on_cran()
# skip_if_offline()
#
#   df1 <- capture_output(query_observation(species = 'a3', verbose = FALSE,
#   pb = FALSE))
#
#   expect_true(df1 == "")
#
# })


