#' Optimise budget
#'
#' @param z Matrix of impact. Rows[i] = units, cols[j] = options, fill = impact measure.
#'   If all units do not have all options, fill z with NA.
#' @param cost Matrix of cost. Rows[i] = units, cols[j] = options, fill = cost.
#'    If all units do not have all options, fill cost with NA.
#' @param budget vector of budgets
#' @param recipients Matrix of recipients. Rows[i] = units, cols[j] = budget levels,
#'   fill = binary indicator to show if Unit[i] has access to budget[j].
#' @param sense Optimisation target, can be "min" or "max"
#'
#' @import ROI.plugin.glpk ompr ompr.roi
#'
#' @return Optimsised solution
#' @export
om <- function(z, cost, budget, recipients = NULL, sense = "max"){
  if(!is.matrix(z) | !is.matrix(cost)  | !all(dim(z) == dim(cost))){
    stop("z and cost inputs must both be matrices with the same dimensions")
  }
  if(!is.numeric(budget) | !is.vector(budget) | !all(is.finite(budget))){
    stop("budget must be a finite numeric vector")
  }

  # Number of units
  n <- nrow(z)
  if(n < 2){
    stop("minimum number of units is 2")
  }
  # Number of options per unit
  options <- ncol(z)
  # Number of budget levels
  budget_n <- length(budget)

  # Check missing values match
  if(!identical(which(is.na(z)), which(is.na(cost)))){
    stop("Missing values do not match for cost and z")
  }

  # Set missing values (these should never be possible to choose)
  cost[is.na(cost)] <- sum(budget) + 1
  z[is.na(z)] <- ifelse(sense == "max", min(z, na.rm = TRUE) - 1, max(z, na.rm = TRUE) + 1)

  # If not specified assumed all budgets can be accessed by all units
  if(is.null(recipients)){
    recipients <- matrix(1, nrow = n, ncol = budget_n)
  }
  if(nrow(recipients) != n | ncol(recipients) != budget_n){
    stop("recipients must be a matrix with n unit rows and n budget level columns")
  }
  # Create the not-recipients matrix
  not_recipients <- +!recipients

  # Create the ompr model
  model <- create_model(n = n, options = options, z = z, cost = cost,
                        budget = budget, budget_n = budget_n,
                        not_recipients = not_recipients,
                        sense = sense)

  # Optimise
  solution <- optimise_model(model)

  # Post processing
  z_df <- matrix_to_idf(z)
  cost_df <- matrix_to_idf(cost, z = "cost")
  out <- output(solution = solution, z_df = z_df, cost_df = cost_df, budget = budget)

  return(out)
}

#' Create ompr model
#'
#' @param n Number of units
#' @param options Maximum number of options per unit
#' @param budget_n Number of budget levels
#' @param not_recipients Binary matrix indicating units not allowed to access a given budget level
#' @inheritParams om
#'
#' @return ompr model
create_model <- function(n, options, z, cost, budget, budget_n, not_recipients, sense){
  x <- p <- i <- j <- NULL

  model <- ompr::MIPModel() |>
    # Binary indicator of intervention package (j) choosen for each unit (i)
    ompr::add_variable(x[i, j], i = 1:n, j = 1:options, type = "binary") |>
    # Objective is to maximise, or minimise z
    ompr::set_objective(ompr::sum_over(z[i, j] * x[i, j], i = 1:n, j = 1:options), sense = sense) |>
    # Only one intervention package option per unit
    ompr::add_constraint(ompr::sum_over(x[i, j], j = 1:options) == 1, i = 1:n) |>
    # The proportion of each level of budget (j) going to each uit (i)
    ompr::add_variable(p[i, j], i = 1:n, j = 1:budget_n, type = "continuous", lb = 0, ub = 1) |>
    # The sum of proportions for each budget level must not exceed 1
    ompr::add_constraint(ompr::sum_over(p[i, j], i = 1:n) <= 1, j = 1:budget_n) |>
    # Budget can only be allcoated to specified units
    ompr::add_constraint(ompr::sum_over(p[i, j] * not_recipients[i,j], i = 1:n) == 0, j = 1:budget_n) |>
    # Total allocation of budget  = total cost for each country
    ompr::add_constraint(ompr::sum_over(p[i, j] * budget[j], j = 1:budget_n) == ompr::sum_over(cost[i, j] * x[i, j], j = 1:options), i = 1:n)
}

#' Find solution
#'
#' @param model ompr model as specified by \code{create_model()}
#'
#' @return Model solution
optimise_model <- function(model){
    ompr::solve_model(model = model, solver = ompr.roi::with_ROI(solver = "glpk"))
}
