# Build Cost-Saving Portion of Frontier

Constructs the cost-saving (downward) portion of the cost-effectiveness
frontier by iteratively selecting solutions with the best ICERs among
cost-saving options. Prioritizes solutions that improve impact when
available, but considers all cost-saving options if no impact-improving
solutions exist.

## Usage

``` r
build_frontier_down(x, start_pos, threshold, down_filter)
```

## Arguments

- x:

  A data.frame containing ordered cost and impact data

- start_pos:

  Integer. Position of the starting solution in the ordered data

- threshold:

  Numeric. Willingness-to-pay threshold for ICER acceptability

- down_filter:

  Function. Optional user-defined filter for cost-saving candidates

## Value

A data.frame containing cost-saving frontier solutions with step numbers
(negative values indicating cost-saving steps)
