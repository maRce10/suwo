test_that("find from gbif ml and xc", {

  skip_on_cran()
  skip_if_offline()

  wa <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")
  xc <- query_xenocanto(term = 'Glaucis dohrnii')

  merged_mt <- merge_metadata(wa, xc)

  expect_true(nrow(merged_mt) == nrow(wa) + nrow(xc))

  expect_true(all(unique(merged_mt$source) %in% c("wa", "xc")))

  # add a third source
  gbf <- query_gbif(term = 'Glaucis dohrnii', format =  "sound")

  merged_mt2 <- merge_metadata(wa, xc, gbf)

  expect_true(nrow(merged_mt2) == nrow(wa) + nrow(xc) + nrow(gbf))

  expect_true(all(unique(merged_mt2$source) %in% c("wa", "xc", "gbf")))
})

