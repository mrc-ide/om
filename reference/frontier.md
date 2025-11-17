# Build Cost-Effectiveness Frontier

Constructs a cost-effectiveness frontier from a set of interventions by
finding the optimal sequence of solutions moving both up (more costly)
and down (less costly) from a specified starting point. The algorithm
uses incremental cost-effectiveness ratios (ICERs) and
willingness-to-pay thresholds to determine acceptability.

## Usage

``` r
frontier(
  x,
  threshold = Inf,
  start_index = 1,
  up_filter = NULL,
  down_filter = NULL
)
```

## Arguments

- x:

  A data.frame containing cost and impact data for interventions

- threshold:

  Numeric. Willingness-to-pay threshold for ICER acceptability.
  Solutions with ICERs above this threshold will be rejected. Default is
  Inf.

- start_index:

  Integer. Row index of the starting solution in the x. The algorithm
  builds the frontier from this point. Default is 1.

- up_filter:

  Function. Optional user-defined filter function for cost-increasing
  solutions. Should take arguments (candidates, current) and return
  filtered candidates.

- down_filter:

  Function. Optional user-defined filter function for cost-saving
  solutions. Should take arguments (candidates, current) and return
  filtered candidates.

## Value

A data.frame containing the frontier solutions with an additional 'step'
column indicating the sequence (negative values for cost-saving steps,
positive for cost-increasing steps, 0 for the starting solution).
