#' Cost-effectiveness frontier
#'
#' Identify the set of non-dominated solutions based on cost and impact.
#'
#' Solutions are filtered so that no remaining option has higher cost and
#' lower or equal impact than another. After finding these dominant
#' solutions an optional `threshold` removes with ICER above a given threshold.
#'
#' @param x Data frame containing `cost` and `impact` columns.
#' @param convex_hull Logical. If TRUE, restricts results to the convex
#'   cost-effectiveness frontier by removing extendedly dominated strategies
#'   (those with non-monotonic ICERs). If FALSE, returns the full set of
#'   non-dominated solutions, which may be more useful for exploring how
#'   cost-effective options change across different budget levels.
#' @param threshold Optional numeric value describing the allowed marginal cost per
#'   unit of impact gained. After identifying the frontier, solutions with
#'   ICERs above this value are discarded. We assume we start with the
#'   cheapest dominant solution and iterate checking ICERs moving up in
#'   solution cost from there.
#'
#' @return A data.frame with dominant, ICER compliant solutions
#' @export
#'
frontier <- function(x, convex_hull = FALSE, threshold = Inf, start_index = NULL, up_filter = NULL, down_filter = NULL) {
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

  x$step <- NA
  x$step[start_index] <- 0

  # Dominant solutions
  o <- order(x$cost, -x$impact)
  sorted <- x[o, , drop = FALSE]
  limit <- cummax(sorted$impact)
  dominant <- sorted$impact == limit
  frontier_solutions <- sorted[dominant, ]

  # Convex hull filtering (removing extendedly dominated strategies)
  if(convex_hull){
    n <- nrow(frontier_solutions)
    hull_keep <- rep(FALSE, n)
    i <- 1
    hull_keep[i] <- TRUE

    while (i < n) {
      icers <- (frontier_solutions$cost[(i+1):n] - frontier_solutions$cost[i]) / (frontier_solutions$impact[(i+1):n] - frontier_solutions$impact[i])
      j_rel <- which.min(icers)
      next_icer <- icers[j_rel]
      j <- (i + j_rel)
      hull_keep[j] <- TRUE
      i <- j
    }
    frontier_solutions <- frontier_solutions[hull_keep,]
  }

  # If we start from a user-specified row
  ## This may be dominated, so we may need to add it back in here:
  frontier_start_index <- 1
  if(!is.null(start_index)){
    if(!0 %in% frontier_solutions$step){
      frontier_solutions <- frontier_solutions |>
        rbind(x[!is.na(x$step), ])
    }
    o <- order(frontier_solutions$cost, -frontier_solutions$impact)
    frontier_solutions <- frontier_solutions[o, , drop = FALSE]
    frontier_start_index <- which(frontier_solutions$step == 0)
  }

  # ICER and user-function threshold filtering
  frontier_solutions$step <- 0
  ## Moving down (lower costs)
  down <- list()
  if(frontier_start_index > 1){
    current <- frontier_solutions[frontier_start_index, ]
    down_solutions <- frontier_solutions[frontier_solutions$cost < current$cost, ]

    down <- list()
    down_index <- 1
    while(nrow(down_solutions) > 0){
      print("Down")
      # If user specifies an custom filtering function
      if(!is.null(down_filter)){
        down_solutions <- down_filter(down_solutions, current)
      }
      # WTP threshold filter
      if(nrow(down_solutions) > 0){
        icers <- (down_solutions$cost - current$cost) / (down_solutions$impact - current$impact)
        icer_keep <- icers <= threshold
        down_solutions <- down_solutions[icer_keep, ]
        # Keep the next one down (stepwise, that is the most expensive)
        current <- tail(down_solutions, 1)
        current$step <- -down_index
        down[[down_index]] <- current

        down_solutions <- frontier_solutions[frontier_solutions$cost < current$cost, ]
        down_index <- down_index + 1
      }
    }
  }

  up <- list()
  if(frontier_start_index < nrow(frontier_solutions)){
    current <- frontier_solutions[frontier_start_index, ]
    up_solutions <- frontier_solutions[frontier_solutions$cost > current$cost, ]

    up <- list()
    up_index <- 1
    while(nrow(up_solutions) > 0){
      print("Up")
      # If user specifies an custom filtering function
      if(!is.null(up_filter)){
        up_solutions <- up_filter(up_solutions, current)
      }
      # WTP threshold filter
      if(nrow(up_solutions) > 0){
        icers <- (up_solutions$cost - current$cost) / (up_solutions$impact - current$impact)
        icer_keep <- icers <= threshold
        up_solutions <- up_solutions[icer_keep, ]
        # Keep the next one down (stepwise, that is the most cheap)
        current <- head(up_solutions, 1)
        current$step <- up_index
        up[[up_index]] <- current

        up_solutions <- frontier_solutions[frontier_solutions$cost > current$cost, ]
        up_index <- up_index + 1
      }
    }
  }

  frontier_solutions <- dplyr::bind_rows(down) |>
    dplyr::bind_rows(frontier_solutions[frontier_start_index, ]) |>
    dplyr::bind_rows(up) |>
    dplyr::arrange(step)

  return(frontier_solutions)
}

