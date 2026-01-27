## date ranges
test_that(".date_ranges", {

  dts <- suwo:::.date_ranges(x = c(1976, 2020, 2022, 2024, 2025, 2026))

  # test
  expect_true(nrow(dts) == 5)

  dts <- suwo:::.date_ranges(x = seq(1976, 2026, length.out = 50))

  expect_true(nrow(dts) == 95)

})

test_that(".onLoad", {

  dts <- try(suwo:::.onLoad(), silent = TRUE)

  expect_true(base::inherits(dts, "try-error"))

})

test_that(".onUnload", {

  dts <- try(suwo:::.onUnload(), silent = TRUE)

  expect_true(is.null(dts))

})


test_that(".add_emoji", {

 em <- suwo:::.add_emoji("happy")

 expect_true(class(em) == "character")

 })


test_that(".monitor_new_files", {

  out <-suwo:::.monitor_new_files(path = tempdir(), break.time = 0.0001)

  expect_true(is.null(out))

})


test_that(".repo_from_call handles namespaced calls", {

  skip_on_cran()
  skip_if_offline()

  # construct call objects
  call_non_ns <- quote(query_gbif(species = 'Aristolochia baetica',
                                  format =  "image"))
  call_ns <- quote(suwo::query_gbif(species = 'Aristolochia baetica',
                                    format =  "image"))

  # call the internal function using the namespace
  f <- suwo:::.repo_from_call

  expect_identical(
    f(call_non_ns),
    "GBIF"
  )

  expect_identical(
    f(call_ns),
    "GBIF"
  )
})
