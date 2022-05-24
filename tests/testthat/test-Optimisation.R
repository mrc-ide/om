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
  expect_equal(sum(o2$z), 4)
  # Minimise
  o4 <- om(z = z, cost = cost, budget = budget, sense = "min")
  expect_equal(sum(o3$z), 4)
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

})
