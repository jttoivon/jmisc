test_that("low_numbers_in_logicals_are_detected", {
  v <- rep(c(TRUE, FALSE, NA), c(5,5,2))
  expect_error(jsummary(v, min_count=5))

  v <- rep(c(TRUE, FALSE, NA), c(5,2,5))
  expect_error(jsummary(v, min_count=5))

  v <- rep(c(TRUE, FALSE, NA), c(2,5,5))
  expect_error(jsummary(v, min_count=5))

  v <- rep(c(TRUE, FALSE, NA), c(5,5,5))
  expect_no_error(jsummary(v, min_count=5))

})
