options(verbose = TRUE)


test_that("find from gbif ml and xc", {

  # find duplicates
  label_dup_metadata <- find_duplicates(metadata = suwo:::merged_metadata)

  # remove duplicates
  dedup_metadata <- remove_duplicates(label_dup_metadata)

  expect_true(nrow(dedup_metadata) > 2026)

  # remove duplicates
  dedup_metadata2 <- remove_duplicates(label_dup_metadata, same_repo = TRUE)

  expect_true(nrow(dedup_metadata2) > 1891)

  })


test_that("error if missing colum", {

  expect_error(
    remove_duplicates(suwo:::merged_metadata),
    regexp = "duplicated_group"
  )

})
