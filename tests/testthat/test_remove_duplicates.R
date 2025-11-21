options(verbose = TRUE)


test_that("find from gbif ml and xc", {

  skip_on_cran()
  skip_if_offline()
  # find duplicates
  label_dup_metadata <- find_duplicates(metadata = suwo:::merged_metadata)

  # remove duplicates
  dedup_metadata <- remove_duplicates(label_dup_metadata)

  expect_true(nrow(dedup_metadata) > 2026)

  # remove duplicates
  dedup_metadata2 <- remove_duplicates(label_dup_metadata, same_repo = TRUE)

  expect_true(nrow(dedup_metadata2) > 1891)

  })

