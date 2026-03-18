test_that("integers are whole numbers", {
  expect_true(is_whole_number(c(1L, 2L, 3L)))
})

test_that("doubles without decimals are whole numbers", {
  expect_true(is_whole_number(c(1, 2, 3)))
})

test_that("zero length vectors work", {
  expect_true(is_whole_number(integer()))
  expect_true(is_whole_number(double()))
})

test_that("NA values are ignored", {
  expect_true(is_whole_number(c(1L, NA)))
  expect_true(is_whole_number(c(1, NA)))
})

test_that("doubles with decimals are not whole numbers", {
  expect_false(is_whole_number(1.0001))
})
