## date ranges
test_that(".date_ranges", {

  dts <- suwo:::.date_ranges(x = c(1976, 2020, 2022, 2024, 2025, 2026))

  # test
  expect_true(nrow(dts) == 5)

  dts <- suwo:::.date_ranges(x = seq(1976, 2026, length.out = 50))

  expect_true(nrow(dts) == 94)

})
