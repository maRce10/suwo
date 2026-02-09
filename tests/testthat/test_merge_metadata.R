options(verbose = TRUE)

test_that("merge query_wikiaves and xc", {
  wa <- suwo:::vignette_metadata$h_harpyja
  xc <- suwo:::vignette_metadata$a_hahneli
  merged_mt <- merge_metadata(wa, xc)

  expect_true(nrow(merged_mt) == nrow(wa) + nrow(xc))

  expect_true(all(unique(merged_mt$source) %in% c("wa", "xc")))

  # add a third source
  gbf <- suwo:::vignette_metadata$p_lotor

  merged_mt2 <- merge_metadata(wa, xc, gbf)

  expect_true(nrow(merged_mt2) == nrow(wa) + nrow(xc) + nrow(gbf))

  expect_true(all(unique(merged_mt2$source) %in% c("wa", "xc", "gbf")))

  # merge a list of data frames
  merged_mt3 <- merge_metadata(list(wikiaves = wa, xenocanto = xc, gbif = gbf))

  identical(merged_mt3, merged_mt2)
})


# Make simple example data frames with required cols
df1 <- suwo:::vignette_metadata$h_harpyja
df2 <- suwo:::vignette_metadata$h_wagneriana


test_that("merge_metadata works with multiple data frames", {
  res <- merge_metadata(df1, df2)

  expect_s3_class(res, "data.frame")
  expect_true("source" %in% names(res))
  expect_equal(sort(unique(res$source)), c("df1", "df2"))
  expect_equal(nrow(res), 8)
})

test_that("merge_metadata works with named data frames", {
  res <- merge_metadata(bats = df1, birds = df2)

  expect_equal(sort(unique(res$source)), c("bats", "birds"))
})

test_that("merge_metadata works with a named list", {
  res <- merge_metadata(list(bats = df1, birds = df2))

  expect_equal(sort(unique(res$source)), c("bats", "birds"))
})

test_that("merge_metadata works with an unnamed list", {
  res <- merge_metadata(list(df1, df2))

  expect_equal(sort(unique(res$source)), c("df1", "df2"))
})

test_that("duplicate names trigger a warning", {
  expect_warning(
    merge_metadata(a = df1, a = df2),
    regexp = "Duplicate names detected"
  )
})

test_that("non-data-frame inputs throw an error", {
  expect_error(
    merge_metadata(df1, "not a data frame"),
    regexp = "All inputs must be data frames"
  )
})

test_that("missing required columns cause an error", {
  bad_df <- data.frame(x = 1) # missing required cols

  expect_error(
    merge_metadata(df1, bad_df),
    regexp = "missing required columns"
  )
})

test_that("source column corresponds correctly to list element names", {
  res <- merge_metadata(list(alpha = df1, beta = df2))
  expect_equal(sort(unique(res$source)), c("alpha", "beta"))
})

test_that("single list behaves the same as multiple arguments", {
  res1 <- merge_metadata(df1, df2)
  res2 <- merge_metadata(list(df1, df2))

  expect_equal(res1$source, res2$source)
})
