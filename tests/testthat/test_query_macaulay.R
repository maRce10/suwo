test_that("search Glaucis dohrnii sound", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "sound")

  expect_true(nrow(df1) >= 25)

})

