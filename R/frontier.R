#' Build Cost-Effectiveness Frontier
#'
#' Constructs a cost-effectiveness frontier from a set of interventions by finding
#' the optimal sequence of solutions moving both up (more costly) and down (less costly)
#' from a specified starting point. The algorithm uses incremental cost-effectiveness
#' ratios (ICERs) and willingness-to-pay thresholds to determine acceptability.
#'
#' @param x A data.frame containing cost and impact data for interventions
#' @param threshold Numeric. Willingness-to-pay threshold for ICER acceptability.
#'   Solutions with ICERs above this threshold will be rejected. Default is Inf.
#' @param start_index Integer. Row index of the starting solution in the original
#'   data.frame. The algorithm builds the frontier from this point. Default is 1.
#' @param up_filter Function. Optional user-defined filter function for cost-increasing
#'   solutions. Should take arguments (candidates, current) and return filtered candidates.
#' @param down_filter Function. Optional user-defined filter function for cost-saving
#'   solutions. Should take arguments (candidates, current) and return filtered candidates.
#'
#' @return A data.frame containing the frontier solutions with an additional 'step'
#'   column indicating the sequence (negative values for cost-saving steps, positive
#'   for cost-increasing steps, 0 for the starting solution).
#'
#' @export
frontier <- function(x, threshold = Inf, start_index = 1, up_filter = NULL, down_filter = NULL) {

  # Input validation
  validate_inputs(x, threshold, start_index)


  x$step <- NA
  x$step[start_index] <- 0
  # Prepare data
  x <- x[order(x$cost, -x$impact), , drop = FALSE]

  # Find starting position after ordering
  start_pos <- which(x$step == 0)

  # Build frontier in both directions
  down_solutions <- build_frontier_down(x, start_pos, threshold, down_filter)
  up_solutions <- build_frontier_up(x, start_pos, threshold, up_filter)

  # Combine and return results
  frontier_solutions <- dplyr::bind_rows(
    down_solutions,
    x[start_pos, ],
    up_solutions
  ) |>
    dplyr::arrange(.data$step)

  return(frontier_solutions)
}

#' Validate Input Parameters for Frontier Function
#'
#' Performs comprehensive validation of input parameters for the frontier function,
#' checking data types, required columns, and parameter bounds.
#'
#' @param x A data.frame that should contain 'cost' and 'impact' columns
#' @param threshold Numeric value representing willingness-to-pay threshold
#' @param start_index Integer representing the starting row index
#'
#' @return NULL (function stops execution with error if validation fails)
validate_inputs <- function(x, threshold, start_index) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if (!all(c("cost", "impact") %in% names(x))) {
    stop("x must contain 'cost' and 'impact' columns")
  }
  if (!is.numeric(x$cost) || !is.numeric(x$impact)) {
    stop("Cost and impact columns must be numeric")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("threshold must be a positive numeric value")
  }
  if (!is.numeric(start_index) || start_index < 1 || start_index > nrow(x)) {
    stop("start_index must be a valid row number")
  }
}

#' Build Cost-Saving Portion of Frontier
#'
#' Constructs the cost-saving (downward) portion of the cost-effectiveness frontier
#' by iteratively selecting solutions with the best ICERs among cost-saving options.
#' Prioritizes solutions that improve impact when available, but considers all
#' cost-saving options if no impact-improving solutions exist.
#'
#' @param x A data.frame containing ordered cost and impact data
#' @param start_pos Integer. Position of the starting solution in the ordered data
#' @param threshold Numeric. Willingness-to-pay threshold for ICER acceptability
#' @param down_filter Function. Optional user-defined filter for cost-saving candidates
#'
#' @return A data.frame containing cost-saving frontier solutions with step numbers
#'   (negative values indicating cost-saving steps)
build_frontier_down <- function(x, start_pos, threshold, down_filter) {
  if (start_pos <= 1) return(data.frame())

  current <- x[start_pos, ]
  down_solutions <- list()
  step_counter <- 1

  repeat{
    # Get all cost-saving candidates
    candidates <- x[x$cost < current$cost, , drop = FALSE]
    if (nrow(candidates) == 0) break

    # Apply user filter if provided
    if (!is.null(down_filter)) {
      candidates <- down_filter(candidates, current)
      if (nrow(candidates) == 0) break
    }

    # Calculate deltas and ICERs
    delta_cost <- candidates$cost - current$cost
    delta_impact <- candidates$impact - current$impact
    icers <- delta_cost / delta_impact

    # Prefer solutions with positive impact change, but don't require it
    impact_improving <- delta_impact > 0
    if (any(impact_improving)) {
      # If we have impact-improving options, use only those
      candidates <- candidates[impact_improving, , drop = FALSE]
      icers <- icers[impact_improving]
    }
    # Otherwise, use all candidates (your original logic)

    # Choose best (highest/least negative) ICER
    best_idx <- which.max(icers)
    best_icer <- icers[best_idx]

    # Check WTP acceptability
    if (best_icer >= 0 && best_icer > threshold) break

    # Update current solution
    current <- candidates[best_idx, ]
    current$step <- -step_counter
    down_solutions[[step_counter]] <- current
    step_counter <- step_counter + 1
  }

  down_solutions <- dplyr::bind_rows(down_solutions)

  return(down_solutions)
}

#' Build Cost-Increasing Portion of Frontier
#'
#' Constructs the cost-increasing (upward) portion of the cost-effectiveness frontier
#' by iteratively selecting solutions with the lowest ICERs among options that cost
#' more and provide better impact than the current solution.
#'
#' @param x A data.frame containing ordered cost and impact data
#' @param start_pos Integer. Position of the starting solution in the ordered data
#' @param threshold Numeric. Willingness-to-pay threshold for ICER acceptability
#' @param up_filter Function. Optional user-defined filter for cost-increasing candidates
#'
#' @return A data.frame containing cost-increasing frontier solutions with step numbers
#'   (positive values indicating cost-increasing steps)
build_frontier_up <- function(x, start_pos, threshold, up_filter) {
  if (start_pos > nrow(x)) return(data.frame())

  current <- x[start_pos, ]
  up_solutions <- list()
  step_counter <- 1

  repeat{
    # Get candidates that cost more AND have better impact
    candidates <- x[x$cost >= current$cost & x$impact > current$impact, , drop = FALSE]
    if (nrow(candidates) == 0) break

    # Apply user filter if provided
    if (!is.null(up_filter)) {
      candidates <- up_filter(candidates, current)
      if (nrow(candidates) == 0) break
    }

    # Calculate ICERs
    delta_cost <- candidates$cost - current$cost
    delta_impact <- candidates$impact - current$impact
    icers <- delta_cost / delta_impact

    # Choose best (lowest) ICER
    best_idx <- which.min(icers)
    best_icer <- icers[best_idx]

    # Check WTP acceptability
    if (best_icer >= 0 && best_icer > threshold) break

    # Update current solution
    current <- candidates[best_idx, ]
    current$step <- step_counter
    up_solutions[[step_counter]] <- current
    step_counter <- step_counter + 1
  }

  up_solutions <- dplyr::bind_rows(up_solutions)

  return(up_solutions)
}
