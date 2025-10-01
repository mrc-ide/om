#' Cost-effectiveness frontier
#'
#' Identify the set of non-dominated solutions based on cost and impact.
#'
#' Solutions are filtered so that no remaining option has higher cost and
#' lower or equal impact than another. After finding these dominant
#' solutions an optional `threshold` removes with ICER above a given threshold.
#'
#' @param x Data frame containing `cost` and `impact` columns.
#' @param threshold Optional numeric value describing the allowed marginal cost per
#'   unit of impact gained. After identifying the frontier, solutions with
#'   ICERs above this value are discarded. We assume we start with the
#'   cheapest dominant solution and iterate checking ICERs moving up in
#'   solution cost from there.
#'
#' @return A data.frame with dominant, ICER compliant solutions
#' @export
#'
frontier <- function(x, threshold = Inf) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (!all(c("cost", "impact") %in% names(x))) {
    stop("x must contain 'cost' and 'impact' columns")
  }
  if(!is.numeric(x$cost)){
    stop("Cost column must be numeric")
  }
  if(!is.numeric(x$impact)){
    stop("Impact column must be numeric")
  }

  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("threshold must be a positive numeric value")
  }

  # Dominant solutions
  o <- order(x$cost, -x$impact)
  sorted <- x[o, , drop = FALSE]
  limit <- cummax(sorted$impact)
  dominant <- sorted$impact == limit
  frontier_solutions <- sorted[dominant, ]

  # ICER threshold filtering
  icer_keep <- rep(TRUE, nrow(frontier_solutions))
  cur_cost <- frontier_solutions$cost[1]
  cur_impact <- frontier_solutions$impact[1]

  additional_solutions <- nrow(frontier_solutions) > 1
  if(additional_solutions){
    for(i in 2:nrow(frontier_solutions)){
      icer <- (frontier_solutions$cost[i] - cur_cost) / (frontier_solutions$impact[i] - cur_impact)
      icer_keep[i] <- icer <= threshold
      if(icer_keep[i]){
        cur_cost <- frontier_solutions$cost[i]
        cur_impact <- frontier_solutions$impact[i]
      }
    }
  }

  out <- frontier_solutions[icer_keep,]
  return(out)
}

