#' Matrix to indexed data.frame
#'
#' Converts a matrix to data.frame with row (i) and column (j) index
#' variables.
#'
#' @param x Matrix
#' @param z Name of the data.frame column storing the matrix values
#'
#' @return A data.frame
#' @export
matrix_to_idf <- function(x, z = "z"){
  if(!is.matrix(x)){
    stop("x must be a matrix")
  }

  df <- data.frame(
    i = c(t(row(x))),
    j = c(t(col(x))),
    z = c(t(x))
  )
  colnames(df)[3] <- z

  return(df)
}

#' Wrangle allocations
#'
#' @inheritParams output
#'
#' @return Allocation proportions
allocation_output <- function(solution, budget){
  p <- i <- j <- NULL

  solution |>
    # Extract p variable indicating proportion of each budget to each unit
    ompr::get_solution(p[i, j]) |>
    dplyr::mutate(allocation = round(.data$value * budget[j], 6)) |>
    dplyr::rename("budget_level" = "j") |>
    dplyr::select(c("i", "budget_level", "allocation")) |>
    tidyr::pivot_wider(id_cols = "i", names_from = "budget_level", values_from = "allocation", names_prefix = "budget_level_")
}

#' Wrangle solution output
#'
#' @param solution Model solution
#' @param z_df data.frame of z
#' @param cost_df data.frame of cost
#' @inheritParams om
#'
#' @return Formatted output
output <- function(solution, z_df, cost_df, budget){
  x <- i <- j <- NULL

  allocation <- allocation_output(solution = solution, budget = budget)

  solution |>
    ompr::get_solution(x[i, j]) |>
    dplyr::filter(.data$value > 0) |>
    dplyr::left_join(cost_df, by = c("i", "j")) |>
    dplyr::left_join(z_df, by = c("i", "j")) |>
    dplyr::arrange(.data$i) |>
    dplyr::select(-c("variable", "value")) |>
    dplyr::left_join(allocation, by = "i")
}
