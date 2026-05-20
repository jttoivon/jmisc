
####
#### fct_min_count
####

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

test_that("small_number_of_nas_is_an_error", {
  v <- factor(rep(c("a", NA), c(5, 4)), levels=c("a"))
  expect_error(fct_min_count(v, min=5))
})

test_that("enough_nas_is_not_a_problem", {
  v <- factor(rep(c("a", NA), c(5, 5)), levels=c("a"))
  expect_no_error(fct_min_count(v, min=5))
})

test_that("the_result_has_at_most_max_levels", {
  v <- rep(letters[1:4], c(6,5,5,5)) %>% as.factor()
  res <- fct_min_count(v, min=5, max_levels = 3)
  expect_all_true(levels(res) == c("a", "b", "Other"))
  expect_true(sum(res == "a") == 6)
  expect_true(sum(res == "b") == 5)
  expect_true(sum(res == "Other") == 10)
})

test_that("result_is_intact_when_no_lumping_needed", {
  v <- rep(letters[1:4], c(6,5,5,5)) %>% as.factor()
  res <- fct_min_count(v, min=5, max_levels = 4)
  expect_all_true(levels(res) == c("a", "b", "c", "d"))
  expect_true(sum(res == "a") == 6)
  expect_true(sum(res == "b") == 5)
  expect_true(sum(res == "c") == 5)
  expect_true(sum(res == "d") == 5)
})


# add a test where there are non-occurring levels and max_levels is set
test_that("non_occurring_levels_and_max_levels_work", {
  v <- factor(rep(c("a", "b"), c(5, 5)), levels=c("a", "b", "c", "d"))
  res <- fct_min_count(v, max_levels = 4)
  expect_all_true(levels(res) == c("a", "b", "c", "d"))
  expect_true(sum(res == "a") == 5)
  expect_true(sum(res == "b") == 5)
  expect_true(sum(res == "c") == 0)
  expect_true(sum(res == "d") == 0)

  res <- fct_min_count(v, max_levels = 3)
  expect_all_true(levels(res) == c("a", "b", "c"))
  expect_true(sum(res == "a") == 5)
  expect_true(sum(res == "b") == 5)
  expect_true(sum(res == "c") == 0)

  # This needs still fixing!!!!!!!!!!!!!!!!!!!!!
  res <- fct_min_count(v, max_levels = 2)
  expect_all_true(levels(res) == c("a", "b"))
  expect_true(sum(res == "a") == 5)
  expect_true(sum(res == "b") == 5)

})
