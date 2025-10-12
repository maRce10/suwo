test_that("merge query_wikiaves and xc", {

  skip_on_cran()
  skip_if_offline()
  skip_if_not(interactive())

  wa <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")
  xc <- query_xenocanto(term = 'Glaucis dohrnii', api_key = Sys.getenv("XENO_CANTO_API_KEY"))

  merged_mt <- merge_metadata(wa, xc)

  expect_true(nrow(merged_mt) == nrow(wa) + nrow(xc))

  expect_true(all(unique(merged_mt$source) %in% c("wa", "xc")))

  # add a third source
  gbf <- query_gbif(term = 'Glaucis dohrnii', format =  "sound")

  merged_mt2 <- merge_metadata(wa, xc, gbf)

  expect_true(nrow(merged_mt2) == nrow(wa) + nrow(xc) + nrow(gbf))

  expect_true(all(unique(merged_mt2$source) %in% c("wa", "xc", "gbf")))
})

