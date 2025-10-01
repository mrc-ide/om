test_that("frontier finds non dominated solutions", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df)
  expect_equal(nrow(res), 3)
  expect_equal(res$cost, c(1, 2, 4))
})

test_that("threshold filters by ICER", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df, threshold = 0.75)
  expect_equal(res$cost, c(1, 2))
})

test_that("input validation works", {
  df <- data.frame(cost = 1, impact = 1)
  expect_error(frontier(list(a = 1)), "data.frame")
  expect_error(frontier(data.frame(a = 1, b = 2)), "'cost' and 'impact'")
  expect_error(frontier(df, threshold = -1), "positive numeric")
})

test_that("Frontier works if only 1 row retained", {
  df <- data.frame(
    cost = c(1, 1, 1, 1, 1),
    impact = c(1, 3, 2, 5, 4)
  )

  res <- frontier(df)
  expect_equal(res$cost, 1)
  expect_equal(res$impact, 5)
})

test_that("Frontier behaves for negative impact and/or cost-saving  solutions", {
  df <- data.frame(
    cost = c(-5, 3, -2, 0, 2, 5),
    impact = c(-10, -3, 3, 2, 2, 10)
  )
  res <- frontier(df)
  expect_equal(res$cost, c(-5, -2, 5))
})


test_that("Numeric df inputs checked", {
  df <- data.frame(
    cost = c("A", "B"),
    impact = c(1, 3)
  )
  expect_error(frontier(df), "Cost column must be numeric")

  df <- data.frame(
    cost = c(1, 2),
    impact =  c("A", "B")
  )
  expect_error(frontier(df), "Impact column must be numeric")
})
