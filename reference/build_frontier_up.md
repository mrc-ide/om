# Build Cost-Increasing Portion of Frontier

Constructs the cost-increasing (upward) portion of the
cost-effectiveness frontier by iteratively selecting solutions with the
lowest ICERs among options that cost more and provide better impact than
the current solution.

## Usage

``` r
build_frontier_up(x, start_pos, threshold, up_filter)
```

## Arguments

- x:

  A data.frame containing ordered cost and impact data

- start_pos:

  Integer. Position of the starting solution in the ordered data

- threshold:

  Numeric. Willingness-to-pay threshold for ICER acceptability

- up_filter:

  Function. Optional user-defined filter for cost-increasing candidates

## Value

A data.frame containing cost-increasing frontier solutions with step
numbers (positive values indicating cost-increasing steps)
