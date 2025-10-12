test_that("search Phaethornis check rows", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    term = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  expect_true(nrow(df1) > 15)
  expect_true(ncol(df1) == length(.format_query_output(only_basic_columns = T)))
})

test_that("all data TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    term = 'Phaethornis anthophilus',
    all_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  expect_true(nrow(df1) >= 15)
  expect_true(ncol(df1) == 46)
})

test_that("no recs found", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  df1 <- query_xenocanto(term = '000', api_key = Sys.getenv("XENO_CANTO_API_KEY"))

  expect_true(is.null(df1))
})

test_that("check messages", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  msg <- capture.output(
    query_xenocanto(term = '000', verbose = TRUE, api_key = Sys.getenv("XENO_CANTO_API_KEY"))
  )
  expect_true(any(grepl("No sound files were found", msg)))

  msg <- capture.output(
    a <- query_xenocanto(term = '000', verbose = FALSE, api_key = Sys.getenv("XENO_CANTO_API_KEY"))
  )
  expect_true(length(msg) == 0)

  msg <- capture.output(
    a <- query_xenocanto(
      term = 'Phaethornis anthophilus',
      verbose = TRUE,
      pb = FALSE,
      api_key = Sys.getenv("XENO_CANTO_API_KEY")
    )
  )
  expect_true(length(msg) == 0)
})

test_that("test all_data FALSE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    term = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  expected_col_names <- .format_query_output(only_basic_columns = T)
  query_col_names <- colnames(df1)
  expect_true(all(expected_col_names %in% query_col_names) && all(query_col_names %in% expected_col_names), info = "Column names do not match the expected names")
})

test_that("test raw_data TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")), "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    term = 'Phaethornis anthophilus',
    raw_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  expect_true(ncol(df1) == 45)
})
