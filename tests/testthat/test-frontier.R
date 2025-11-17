test_that("frontier finds basic non-dominated solutions", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )
  res <- frontier(df)

  # Should find solutions at positions 1, 2, and 4 (cost 1, 2, 4)
  expect_equal(nrow(res), 3)
  expect_equal(res$cost, c(1, 2, 4))
  expect_equal(res$impact, c(1, 3, 5))

  # Check step numbering
  expect_equal(res$step, c(0, 1, 2))
})

test_that("threshold filters solutions by ICER acceptability", {
  df <- data.frame(
    cost = c(1, 2, 4, 8),
    impact = c(1, 3, 5, 6)
  )

  # Without threshold - all efficient solutions
  res_no_threshold <- frontier(df)
  expect_equal(nrow(res_no_threshold), 4)

  # With restrictive threshold - should stop early
  res_threshold <- frontier(df, threshold = 1.5)
  expect_lt(nrow(res_threshold), 4)
  expect_true(all(res_threshold$cost %in% c(1, 2, 4)))
})

test_that("start_index parameter works correctly", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 3, 2, 5, 4)
  )

  # Start from middle position
  res <- frontier(df, start_index = 3)

  # Starting solution should always be included
  expect_true(3 %in% res$cost)

  # Should have step = 0 for starting solution
  start_row <- res[res$cost == 3, ]
  expect_equal(start_row$step, 0)

  # Should have both positive and negative steps
  expect_true(any(res$step < 0))  # Cost-saving steps
  expect_true(any(res$step > 0))  # Cost-increasing steps
})

test_that("up_filter and down_filter work correctly", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5, 6),
    impact = c(1, 2, 3, 4, 5, 6),
    group = c("A", "B", "A", "B", "A", "B")
  )

  # Filter that only allows alternating groups
  up_filter <- function(candidates, current) {
    candidates[candidates$group != current$group, , drop = FALSE]
  }

  res <- frontier(df, up_filter = up_filter, start_index = 1)

  # Should alternate between groups
  if (nrow(res) > 1) {
    groups <- res$group
    for (i in 2:length(groups)) {
      expect_false(groups[i] == groups[i-1])
    }
  }
})

test_that("input validation catches all error conditions", {
  valid_df <- data.frame(cost = c(1, 2), impact = c(1, 2))

  # Non-dataframe input
  expect_error(frontier(list(cost = 1, impact = 1)), "data.frame")

  # Missing required columns
  expect_error(frontier(data.frame(a = 1, b = 2)), "'cost' and 'impact'")

  # Non-numeric columns
  expect_error(
    frontier(data.frame(cost = c("A", "B"), impact = c(1, 2))),
    "Cost and impact columns must be numeric"
  )
  expect_error(
    frontier(data.frame(cost = c(1, 2), impact = c("A", "B"))),
    "Cost and impact columns must be numeric"
  )

  # Invalid threshold
  expect_error(frontier(valid_df, threshold = -1), "positive numeric")
  expect_error(frontier(valid_df, threshold = c(1, 2)), "positive numeric")
  expect_error(frontier(valid_df, threshold = "high"), "positive numeric")

  # Invalid start_index
  expect_error(frontier(valid_df, start_index = 0), "valid row number")
  expect_error(frontier(valid_df, start_index = 3), "valid row number")
  expect_error(frontier(valid_df, start_index = "first"), "valid row number")
})

test_that("frontier handles edge cases correctly", {
  # Single row
  single_row <- data.frame(cost = 1, impact = 1)
  res_single <- frontier(single_row)
  expect_equal(nrow(res_single), 1)
  expect_equal(res_single$step, 0)

  # All identical costs (should pick highest impact)
  identical_costs <- data.frame(
    cost = c(1, 1, 1, 1),
    impact = c(1, 3, 2, 4)
  )
  res_identical <- frontier(identical_costs)
  expect_equal(max(res_identical$impact), 4)

  # All identical impacts (should pick lowest cost)
  identical_impacts <- data.frame(
    cost = c(1, 2, 3, 4),
    impact = c(5, 5, 5, 5)
  )
  res_impacts <- frontier(identical_impacts)
  expect_equal(res_impacts$cost, 1)
})

test_that("frontier handles negative values correctly", {
  df <- data.frame(
    cost = c(-5, -2, 0, 2, 5),
    impact = c(-10, 3, 2, 2, 10)
  )

  res <- frontier(df)

  # Should include cost-saving solution with positive impact
  expect_true(-2 %in% res$cost)

  # Should handle negative costs and impacts appropriately
  expect_true(all(is.finite(res$cost)))
  expect_true(all(is.finite(res$impact)))
})

test_that("step numbering is consistent and logical", {
  df <- data.frame(
    cost = c(1, 2, 3, 4, 5),
    impact = c(1, 2, 3, 4, 5)
  )

  res <- frontier(df, start_index = 3)

  # Starting solution should have step = 0
  expect_equal(res$step[res$cost == 3], 0)

  # Steps should be consecutive integers
  steps <- sort(res$step)
  expect_equal(steps, seq(min(steps), max(steps)))

  # Cost-saving steps should be negative
  cost_saving <- res[res$step < 0, ]
  if (nrow(cost_saving) > 0) {
    expect_true(all(cost_saving$cost < 3))
  }

  # Cost-increasing steps should be positive
  cost_increasing <- res[res$step > 0, ]
  if (nrow(cost_increasing) > 0) {
    expect_true(all(cost_increasing$cost > 3))
  }
})

test_that("filter functions receive correct arguments", {
  df <- data.frame(
    cost = c(1, 2, 3, 4),
    impact = c(1, 2, 3, 4),
    test_col = c("A", "B", "C", "D")
  )

  # Track what the filter function receives
  filter_calls <- list()

  test_filter <- function(candidates, current) {
    filter_calls <<- append(filter_calls, list(list(
      candidates = candidates,
      current = current
    )))
    return(candidates)
  }

  frontier(df, up_filter = test_filter, start_index = 1)

  # Filter should have been called at least once
  expect_gt(length(filter_calls), 0)

  # Each call should have proper structure
  for (call in filter_calls) {
    expect_true(is.data.frame(call$candidates))
    expect_true(is.data.frame(call$current))
    expect_equal(nrow(call$current), 1)
    expect_true(all(c("cost", "impact") %in% names(call$candidates)))
    expect_true(all(c("cost", "impact") %in% names(call$current)))
  }
})

test_that("frontier preserves additional columns", {
  df <- data.frame(
    cost = c(1, 2, 3, 4),
    impact = c(1, 2, 3, 4),
    intervention_name = c("A", "B", "C", "D"),
    category = c("Type1", "Type2", "Type1", "Type2")
  )

  res <- frontier(df)

  # Should preserve all original columns
  expect_true(all(c("intervention_name", "category") %in% names(res)))

  # Should add step column
  expect_true("step" %in% names(res))

  # Data integrity should be maintained
  for (i in 1:nrow(res)) {
    original_row <- df[df$cost == res$cost[i] & df$impact == res$impact[i], ][1, ]
    expect_equal(res$intervention_name[i], original_row$intervention_name)
    expect_equal(res$category[i], original_row$category)
  }
})
