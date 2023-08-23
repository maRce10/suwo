#
# test_that("search Aristolochia baetica images", {
#
#   df1 <- query_observation(term = 'Aristolochia baetica', type = "still image", token = token)
#
#   expect_true(nrow(df1) >= 1)
#
# })
#
#
# test_that("search Floractus heimi (no observations)", {
#
#   df1 <- query_observation(term = 'Floractus heimi', type = "still image", token = token)
#
#   expect_true(is.null(df1))
#
# })

test_that("no result", {

  expect_error(query_observation(term = 'asdasdasd', type = "still image"), "Species was not found in database")

})
#
#
# test_that("search Turdus grayi photos in parallel", {
#
#     df1 <- query_observation(term = 'Serinus serinus', type = "sound", token = token, cores = 2)
#
#   expect_true(nrow(df1) >= 177)
#
# })
#
# test_that("test verbose FALSE", {
#
#   df1 <- capture_output(query_observation(term = 'a3', verbose = FALSE, pb = FALSE))
#
#   expect_true(df1 == "")
#
# })


