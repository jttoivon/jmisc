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

test_that("simple_example_with_min_5_works", {
  v <- rep(letters[1:4], c(6,5,2,1)) %>% as.factor()
  res <- fct_min_count(v, min=5)
  expect_true(all(levels(res) == c("a", "Other")))
  expect_true(sum(res == "a") == 6)
  expect_true(sum(res == "Other") == 8)
})

test_that("simple_example_with_min_3_works", {
  v <- rep(letters[1:4], c(6,5,2,1)) %>% as.factor()
  res <- fct_min_count(v, min = 3)
  expect_true(all(levels(res) == c("a", "b", "Other")))
  expect_true(sum(res == "a") == 6)
  expect_true(sum(res == "b") == 5)
  expect_true(sum(res == "Other") == 3)
})

test_that("impossible_request_errors", {
  v <- factor(rep("a", 4))
  expect_error(fct_min_count(v, min=5))
})

test_that("non_occurring_levels_stay_intact", {
  v <- factor(rep("a", 5), levels=c("a", "b", "c"))
  res <- fct_min_count(v, min=5)
  expect_all_true(levels(res) == c("a", "b", "c"))
  expect_true(sum(res == "a") == 5)
  expect_true(sum(res == "b") == 0)
  expect_true(sum(res == "c") == 0)
})

test_that("non_occurring_levels_stay_intact2", {
  v <- factor(rep(c("a", "b"), c(3, 2)), levels=c("a", "b", "c"))
  res <- fct_min_count(v, min=5)
  expect_all_true(levels(res) == c("c", "Other"))
  expect_true(sum(res == "Other") == 5)
  expect_true(sum(res == "c") == 0)
})

