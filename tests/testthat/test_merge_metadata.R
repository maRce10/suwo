test_that("merge query_wikiaves and xc", {

  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  wa <- query_wikiaves(species = 'Glaucis dohrnii', format =  "sound")
  xc <- query_xenocanto(species = 'Glaucis dohrnii',
                        api_key = Sys.getenv("XENO_CANTO_API_KEY"))

  merged_mt <- merge_metadata(wa, xc)

  expect_true(nrow(merged_mt) == nrow(wa) + nrow(xc))

  expect_true(all(unique(merged_mt$source) %in% c("wa", "xc")))

  # add a third source
  gbf <- query_gbif(species = 'Glaucis dohrnii', format =  "sound")

  merged_mt2 <- merge_metadata(wa, xc, gbf)

  expect_true(nrow(merged_mt2) == nrow(wa) + nrow(xc) + nrow(gbf))

  expect_true(all(unique(merged_mt2$source) %in% c("wa", "xc", "gbf")))
})

