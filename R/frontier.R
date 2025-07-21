#' Cost-effectiveness frontier
#'
#' Identify the set of non-dominated solutions based on cost and impact.
#'
#' Solutions are filtered so that no remaining option has higher cost and
#' lower or equal impact than another. An optional `threshold` removes
#' solutions that cost more per unit impact than the specified value.
#'
#' @param x Data frame containing `cost` and `impact` columns.
#' @param threshold Optional numeric value of maximum cost per unit of impact.
#'   Solutions with `cost / impact` greater than this are removed before
#'   determining the frontier.
#' @param keep_all Logical, if `TRUE` return all supplied rows with an added
#'   logical column `frontier`. Otherwise only frontier rows are returned.
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
frontier <- function(x, threshold = NULL, keep_all = FALSE) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (!all(c("cost", "impact") %in% names(x))) {
    stop("x must contain 'cost' and 'impact' columns")
  }

  data <- x
  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1 || !is.finite(threshold) || threshold <= 0) {
      stop("threshold must be a positive numeric value")
    }
    data <- data[data$cost / data$impact <= threshold, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    out <- data
    if (keep_all) out$frontier <- logical(0)
    return(out)
  }

  ord <- order(data$cost, -data$impact)
  sorted <- data[ord, , drop = FALSE]

  best <- -Inf
  flag <- logical(nrow(sorted))
  for (i in seq_len(nrow(sorted))) {
    if (sorted$impact[i] > best) {
      flag[i] <- TRUE
      best <- sorted$impact[i]
    }
  }
  sorted$frontier <- flag

  if (keep_all) {
    out <- sorted[order(ord), , drop = FALSE]
  } else {
    out <- sorted[flag, , drop = FALSE]
  }
  rownames(out) <- NULL
  return(out)
}

