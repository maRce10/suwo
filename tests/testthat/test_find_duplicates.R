options(verbose = TRUE)


test_that("find from gbif ml and xc", {

  skip_on_cran()
  skip_if_offline()

  label_dup_metadata <- find_duplicates(metadata = suwo:::merged_metadata)

  grps <- unique(label_dup_metadata$duplicate_group)

  expect_true(length(grps) > 458)

  expect_true(sum(is.na(label_dup_metadata$duplicate_group)) > 1221)
})

