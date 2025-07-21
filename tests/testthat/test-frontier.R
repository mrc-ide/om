test_that("frontier finds non dominated", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df)
  expect_equal(nrow(res), 3)
  expect_true(all(res$cost %in% c(1, 2, 4)))
})

test_that("keep_all works", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df, keep_all = TRUE)
  expect_equal(nrow(res), 5)
  expect_true("frontier" %in% names(res))
  expect_equal(sum(res$frontier), 3)
})

test_that("threshold filters", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df, threshold = 1)
  expect_true(all(res$cost %in% c(1, 2, 4)))
})

test_that("threshold with keep_all returns filter column", {
  df <- data.frame(
    cost = c(1, 2, 3),
    impact = c(1, 3, 2)
  )
  res <- frontier(df, threshold = 1.5, keep_all = TRUE)
  expect_true("threshold" %in% names(res))
  expect_equal(res$threshold, res$impact / res$cost >= 1.5)
})

test_that("maximise = FALSE finds lower frontier", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(5, 4, 3, 2, 1)
  )
  res <- frontier(df, maximise = FALSE)
  expect_equal(nrow(res), 5)
  expect_true(all(res$cost == 1:5))
})

test_that("threshold works with maximise = FALSE", {
  df <- data.frame(
    cost = c(1, 2, 3, 4),
    impact = c(4, 3, 2, 1)
  )
  res <- frontier(df, maximise = FALSE, threshold = 1)
  expect_true(all(res$cost %in% c(1, 2)))
})
