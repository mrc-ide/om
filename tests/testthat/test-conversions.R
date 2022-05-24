test_that("matrix conversion works", {
  input <- matrix(1:4, nrow = 2)

  output <- matrix_to_idf(x = input)
  expect_identical(output, data.frame(i = c(1L, 1L, 2L, 2L),
                                      j = c(1L, 2L, 1L, 2L),
                                      z = c(1L, 3L, 2L, 4L)))

  output2 <- matrix_to_idf(x = input, z = "k")
  expect_identical(output2, data.frame(i = c(1L, 1L, 2L, 2L),
                                      j = c(1L, 2L, 1L, 2L),
                                      k = c(1L, 3L, 2L, 4L)))

  expect_error(matrix_to_idf(x = 1:4), "x must be a matrix")
})
