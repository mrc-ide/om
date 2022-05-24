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
