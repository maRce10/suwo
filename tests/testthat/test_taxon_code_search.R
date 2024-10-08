library(testthat)
test_that("search Glaucis dohrnii taxon code", {

  gd_taxon_code <- taxon_code_search(term = 'Glaucis dohrnii')

  expect_true(gd_taxon_code == "hobher2")

})

test_that("search Aristolochia baetica images", {

  ti_taxon_code <- taxon_code_search(term = 'Turdus iliacus')

  expect_true(ti_taxon_code == "redwin")

})

test_that("no result", {

  taxon_code <- taxon_code_search(term = 'adsad')

  expect_true(is.null(taxon_code))

})
