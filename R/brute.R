#' Brute force search for single budget level
#'
#' @inheritParams om
#'
#' @return Brute force solution
brute <- function(z, cost, budget){
  if(!is.matrix(z) | !is.matrix(cost)  | !all(dim(z) == dim(cost))){
    stop("z and cost inputs must both be matrices with the same dimensions")
  }
  if(!is.numeric(budget) | !is.vector(budget) | length(budget) > 1){
    stop("budget must be a numeric vector of length 1")
  }

  # Number of units
  n <- nrow(z)
  # Number of options per unit
  options <- ncol(z)
  # Number of budget levels
  budget_n <- length(budget)

  z_df <- matrix_to_idf(z)
  cost_df <- matrix_to_idf(cost, z = "cost")

  # Data frame of possible combinations
  l <- rep(list(1:options), n)
  all_comb <- expand.grid(l)
  colnames(all_comb) <- 1:n

  # Link with cost and z
  all_opts <- all_comb |>
    dplyr::mutate(index = 1:dplyr::n()) |>
    tidyr::pivot_longer(-.data$index, names_to = "i", values_to = "j", names_transform = list(i = as.integer)) |>
    dplyr::left_join(cost_df, by = c("i", "j")) |>
    dplyr::left_join(z_df, by = c("i", "j"))

  # Filter (the first) optimum solution
  solution <- all_opts |>
    dplyr::group_by(.data$index) |>
    dplyr::summarise(
      cost = sum(.data$cost),
      z = sum(.data$z)) |>
    dplyr::filter(.data$cost <= budget) |>
    dplyr::slice_max(order_by = .data$z, n = 1, with_ties = FALSE)

  # Format
  out <- dplyr::filter(all_opts, .data$index == solution$index) |>
    dplyr::select(.data$i, .data$j, .data$cost, .data$z) |>
    dplyr::arrange(.data$i, .data$j)

  return(out)
}
