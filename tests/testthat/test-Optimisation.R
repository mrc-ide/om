test_that("Single budget level optimisation works", {
  n <- 2
  options <- 2
  cost <- matrix(1:options, ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2

  # Unlimited budget ###########################################################
  budget <- 100
  # Maximise
  o1 <- om(z = z, cost = cost, budget = budget)
  expect_equal(sum(o1$z), sum(apply(z, 1, max)))
  # Minimise
  o2 <- om(z = z, cost = cost, budget = budget, sense = "min")
  expect_equal(sum(o2$z), sum(apply(z, 1, min)))
  ##############################################################################

  # Limited budget ###########################################################
  budget <- 2
  # Maximise
  o3 <- om(z = z, cost = cost, budget = budget)
  expect_equal(sum(o3$z), 4)
  # Minimise
  o4 <- om(z = z, cost = cost, budget = budget, sense = "min")
  expect_equal(sum(o4$z), 4)
  ##############################################################################

  # Small examples compared to brute force search ##############################
  set.seed(1)
  for(i in 1:5){
    n <- 5
    options <- 5
    cost <- matrix(0:(options - 1), ncol = options, nrow = n, byrow = TRUE) + rgamma(options * n, 1, 1)
    z <- cost * 2
    budget <- mean(cost) * n

    o5 <- om(z = z, cost = cost, budget = budget)
    b5 <- brute(z = z, cost = cost, budget = budget)
    expect_equal(sum(o5$z), sum(b5$z))
  }
  ##############################################################################

  # Unordered larger random examples with unlimited budget #####################
  set.seed(1)
  for(i in 1:5){
    n <- 50
    options <- 10
    cost <- matrix(rpois(n * options, 10), ncol = options, nrow = n, byrow = TRUE)
    z <- cost * 2
    budget <- sum(cost)
    # Maximise
    o6 <- om(z = z, cost = cost, budget = budget)
    expect_equal(sum(o6$z), sum(apply(z, 1, max)))
    # Minimise
    o7 <- om(z = z, cost = cost, budget = budget, sense = "min")
    expect_equal(sum(o7$z), sum(apply(z, 1, min)))
  }
  ##############################################################################

  # Non-linear relationship between impact and cost ############################
  n <- 10
  options <- 20
  z <- matrix(1:options, ncol = options, nrow = n, byrow = TRUE)
  cost <- exp(0.5 * z)

  # Unlimited budget
  budget <- sum(cost)
  # Maximise
  o8 <- om(z = z, cost = cost, budget = budget)
  expect_equal(sum(o8$z), sum(apply(z, 1, max)))
  # Minimise
  o9 <- om(z = z, cost = cost, budget = budget, sense = "min")
  expect_equal(sum(o9$z), sum(apply(z, 1, min)))

  # Limited budget - due to non-linearity, we expect all units to be given
  # equal share (not all money to a single unit)
  for(i in 1:options){
    budget <- sum(cost[,i])
    # Maximise
    o9 <- om(z = z, cost = cost, budget = budget)
    expect_true(all(o9$z == i))
  }
  ##############################################################################
})

test_that("Multiple budget level optimisation works", {
  n <- 2
  options <- 2
  cost <- matrix(1:options, ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2

  # Two levels, recipients = all ###############################################
  budget <- c(1, 1)
  o10 <- om(z = z, cost = cost, budget = budget)
  budget <- sum(budget)
  o11 <- om(z = z, cost = cost, budget = budget)
  # Output should == single level with same total budget
  expect_identical(o10[,c("i", "j", "z")], o11[,c("i", "j", "z")])
  ##############################################################################

  # Two levels, recipients = mixed, unlimited budget ###########################
  n <- 50
  options <- 10
  cost <- matrix(rpois(n * options, 10), ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2
  budget <- round(c(sum(cost), sum(cost)))
  recipients <- cbind(
    c(rep(1, 40), rep(0, 10)),
    c(rep(0, 30), rep(1, 20))
  )
  # Maximise
  o12 <- om(z = z, cost = cost, budget = budget, recipients = recipients)
  expect_true(sum(o12$cost) <= sum(budget))
  expect_lt(sum(o12$budget_level_1), budget[1])
  expect_lt(sum(o12$budget_level_2), budget[2])
  expect_equal(sum(o12$z), sum(apply(z, 1, max)))
  expect_equal(sum(o12$budget_level_1[recipients[,1] == 0]), 0)
  expect_equal(sum(o12$budget_level_2[recipients[,2] == 0]), 0)
  ##############################################################################

  # Two levels, recipients = mixed, limited budget #############################
  n <- 50
  options <- 10
  cost <- matrix(rpois(n * options, 10), ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2
  budget <- round(c(mean(cost) * (n/2), mean(cost) * (n/3)))
  recipients <- cbind(
    c(rep(1, 40), rep(0, 10)),
    c(rep(0, 30), rep(1, 20))
  )
  # Maximise
  o13 <- om(z = z, cost = cost, budget = budget, recipients = recipients)
  expect_true(sum(o13$cost) <= sum(budget))
  expect_lte(round(sum(o13$budget_level_1)), budget[1])
  expect_lte(round(sum(o13$budget_level_2)), budget[2])
  expect_equal(sum(o13$budget_level_1[recipients[,1] == 0]), 0)
  expect_equal(sum(o13$budget_level_2[recipients[,2] == 0]), 0)
  ##############################################################################
})

test_that("Ragged array works", {
  n <- 5
  options <- 5
  cost <- matrix(1:options, ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2

  cost[5, 4] <- NA
  z[5, 4] <- NA

  # Unlimited budget ###########################################################
  budget <- 100
  # Maximise
  o14 <- om(z = z, cost = cost, budget = budget)
  expect_equal(sum(o14$z), sum(apply(z, 1, max, na.rm = TRUE)))
  ##############################################################################

  # Unmatched missing ##########################################################
  cost[1, 1] <- NA
  expect_error(om(z = z, cost = cost, budget = budget),
               "Missing values do not match for cost and z")
  ##############################################################################
})

test_that("Input checks work", {
  n <- 5
  options <- 5
  cost <- matrix(1:options, ncol = options, nrow = n, byrow = TRUE)
  z <- cost * 2
  budget <- 100

  expect_error(om(z = 1:2, cost = cost, budget = budget),
               "z and cost inputs must both be matrices with the same dimensions")
  expect_error(om(z = z, cost = 1:2, budget = budget),
               "z and cost inputs must both be matrices with the same dimensions")
  expect_error(om(z = z[1:2,], cost = cost, budget = budget),
               "z and cost inputs must both be matrices with the same dimensions")

  expect_error(om(z = z, cost = cost, budget = "A"),
               "budget must be a finite numeric vector")
  expect_error(om(z = z, cost = cost, budget = Inf),
               "budget must be a finite numeric vector")
  expect_error(om(z = z, cost = cost, budget = z),
               "budget must be a finite numeric vector")

  expect_error(om(z = z[1,,drop = FALSE], cost = cost[1,,drop = FALSE], budget = budget),
               "minimum number of units is 2")

  budget <- c(100, 100)
  recipients <- rbind(
    c(1, 1),
    c(1, 1),
    c(1, 1),
    c(1, 0),
    c(0, 1)
  )

  expect_error(om(z = z, cost = cost, budget = budget, recipients = recipients[,1, drop = FALSE]),
               "recipients must be a matrix with n unit rows and n budget level column")
  expect_error(om(z = z, cost = cost, budget = budget, recipients = recipients[1,, drop = FALSE]),
               "recipients must be a matrix with n unit rows and n budget level column")
})

test_that("Brute fails elegantly", {
  expect_error(
    brute("A", "B", "C"),
    "z and cost inputs must both be matrices with the same dimensions"
  )
  expect_error(
    brute(matrix(1), matrix(1), "C"),
    "budget must be a numeric vector of length 1"
  )


})
