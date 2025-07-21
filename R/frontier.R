#' Cost-effectiveness frontier
#'
#' Identify the set of non-dominated solutions based on cost and impact.
#'
#' Solutions are filtered so that no remaining option has higher cost and
#' lower or equal impact than another. After finding these dominant
#' solutions an optional `threshold` removes any that cost more per unit
#' impact than the specified value.
#'
#' @param x Data frame containing `cost` and `impact` columns.
#' @param threshold Optional numeric value describing the allowed
#'   cost effectiveness. After identifying the frontier, solutions with
#'   `impact / cost` below this value are discarded (or `cost / impact`
#'   above it when `maximise = FALSE`).
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

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        !is.finite(threshold) || threshold <= 0) {
      stop("threshold must be a positive numeric value")
    }
  }

  if (nrow(x) == 0) {
    out <- x
    if (!is.null(threshold) && keep_all) out$threshold <- numeric(0)
    if (keep_all) out$frontier <- logical(0)
    return(out)
  }

  impact_order <- if (maximise) -x$impact else x$impact
  o <- order(x$cost, impact_order)
  sorted <- x[o, , drop = FALSE]

  limit <- if (maximise) cummax(sorted$impact) else cummin(sorted$impact)
  keep_frontier <- sorted$impact == limit
  sorted$frontier <- keep_frontier

  if (!is.null(threshold)) {
    ratio <- if (maximise) sorted$impact / sorted$cost else sorted$cost / sorted$impact
    threshold_keep <- if (maximise) ratio >= threshold else ratio <= threshold
    sorted$threshold <- threshold_keep
  }

  if (keep_all) {
    out <- sorted[order(o), , drop = FALSE]
  } else {
    keep <- keep_frontier
    if (!is.null(threshold)) keep <- keep & threshold_keep
    out <- sorted[keep, , drop = FALSE]
  }
  rownames(out) <- NULL
  return(out)
}

