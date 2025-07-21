#' Cost-effectiveness frontier
#'
#' Identify the set of non-dominated solutions based on cost and impact.
#'
#' Solutions are filtered so that no remaining option has higher cost and
#' lower or equal impact than another. An optional `threshold` removes
#' solutions that cost more per unit impact than the specified value.
#'
#' @param x Data frame containing `cost` and `impact` columns.
#' @param threshold Optional numeric value describing the allowed
#'   cost effectiveness. When `maximise = TRUE` solutions with
#'   `impact / cost` less than this threshold are removed. When
#'   `maximise = FALSE` solutions with `cost / impact` greater than the
#'   threshold are removed.
#' @param keep_all Logical, if `TRUE` return all supplied rows with an added
#'   logical column `frontier`. Otherwise only frontier rows are returned.
#' @param maximise Logical, if `TRUE` (default) the frontier is calculated
#'   assuming higher values of `impact` are better. If `FALSE` the function
#'   identifies the lower frontier for outcomes that should be minimised.
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' df <- data.frame(cost = c(1, 2, 2, 3),
#'                  impact = c(1, 1.5, 2, 2.4))
#' frontier(df)
#' frontier(df, keep_all = TRUE)
#' frontier(df, threshold = 1)
#' frontier(df, maximise = FALSE)
frontier <- function(x, threshold = NULL, keep_all = FALSE, maximise = TRUE) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (!all(c("cost", "impact") %in% names(x))) {
    stop("x must contain 'cost' and 'impact' columns")
  }

  if (!is.logical(maximise) || length(maximise) != 1 || is.na(maximise)) {
    stop("maximise must be a single logical value")
  }

  data <- x
  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        !is.finite(threshold) || threshold <= 0) {
      stop("threshold must be a positive numeric value")
    }
    if (maximise) {
      threshold_keep <- data$impact / data$cost >= threshold
    } else {
      threshold_keep <- data$cost / data$impact <= threshold
    }
    if (keep_all) {
      data$threshold <- threshold_keep
    } else {
      data <- data[threshold_keep, , drop = FALSE]
    }
  }

  if (nrow(data) == 0) {
    out <- data
    if (keep_all) out$frontier <- logical(0)
    return(out)
  }

  if (maximise) {
    o <- order(data$cost, -data$impact)
  } else {
    o <- order(data$cost, data$impact)
  }
  sorted <- data[o, , drop = FALSE]

  if (maximise) {
    keep <- sorted$impact == cummax(sorted$impact)
  } else {
    keep <- sorted$impact == cummin(sorted$impact)
  }
  sorted$frontier <- keep

  if (keep_all) {
    out <- sorted[order(o), , drop = FALSE]
  } else {
    out <- sorted[keep, , drop = FALSE]
  }
  rownames(out) <- NULL
  return(out)
}

