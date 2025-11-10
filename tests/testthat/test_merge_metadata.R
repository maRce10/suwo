test_that("merge query_wikiaves and xc", {


  wa <- suwo:::vignette_metadata$h_harpyja
  xc <- suwo:::vignette_metadata$a_hahneli
  merged_mt <- merge_metadata(wa, xc)

  expect_true(nrow(merged_mt) == nrow(wa) + nrow(xc))

  expect_true(all(unique(merged_mt$source) %in% c("wa", "xc")))

  # add a third source
  wa <- suwo:::vignette_metadata$h_harpyja
  gbf <- suwo:::vignette_metadata$p_lotor

  merged_mt2 <- merge_metadata(wa, xc, gbf)

  expect_true(nrow(merged_mt2) == nrow(wa) + nrow(xc) + nrow(gbf))

  expect_true(all(unique(merged_mt2$source) %in% c("wa", "xc", "gbf")))
})

