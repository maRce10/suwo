test_that("search Phaethornis check rows", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  skip_if(is.null(df1))

  expect_true(nrow(df1) > 15)
  expect_true(ncol(df1) == length(.format_query_output(only_basic_columns = T)))
})

test_that("all data TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 15)
})

test_that("no recs found", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(species = '000',
                         api_key = Sys.getenv("XENO_CANTO_API_KEY"))

  expect_true(is.null(df1))
})

test_that("check messages", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  msg <- capture_error(
    query_xenocanto(species = '000', verbose = TRUE,
                    api_key = "")
  )
  expect_true(as.character(msg) ==
                paste("Error: An API key is required for Xeno-Canto API v3.",
                      "Get yours at https://xeno-canto.org/account.\n"))

  msg <- capture.output(
    a <- query_xenocanto(species = '000', verbose = FALSE,
                         api_key = Sys.getenv("XENO_CANTO_API_KEY"))
  )

  expect_true(length(msg) == 0)

  msg <- capture.output(
    df1 <- query_xenocanto(
      species = 'Phaethornis anthophilus',
      verbose = TRUE,
      pb = FALSE,
      api_key = Sys.getenv("XENO_CANTO_API_KEY")
    )
  )

  skip_if(is.null(df1))

  expect_true(length(msg) == 0)
})

test_that("test all_data FALSE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  skip_if(is.null(df1))

  expected_col_names <- .format_query_output(only_basic_columns = TRUE)
  query_col_names <- colnames(df1)
  expect_true(all(expected_col_names %in% query_col_names) &&
                all(query_col_names %in% expected_col_names),
              info = "Column names do not match the expected names")
})

test_that("test raw_data TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    raw_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  skip_if(is.null(df1))

  expect_true(ncol(df1) == 45)
})


test_that("test tags", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'sp:"Phaethornis anthophilus" cnt:"Panama"',
    raw_data = TRUE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )
  skip_if(is.null(df1))

  expect_true(nrow(df1) > 1)

  femsong <- query_xenocanto(
    species = 'sp:"Thryothorus ludovicianus" type:"song" type:"female"',
                             api_key = Sys.getenv("XENO_CANTO_API_KEY"))
  skip_if(is.null(femsong))

  expect_true(nrow(femsong) > 490)
  })


