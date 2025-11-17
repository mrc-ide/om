# Optimise budget

Optimise budget

## Usage

``` r
om(z, cost, budget, recipients = NULL, sense = "max")
```

## Arguments

- z:

  Matrix of impact. Rows\[i\] = units, cols\[j\] = options, fill =
  impact measure. If all units do not have all options, fill z with NA.

- cost:

  Matrix of cost. Rows\[i\] = units, cols\[j\] = options, fill = cost.
  If all units do not have all options, fill cost with NA.

- budget:

  vector of budgets

- recipients:

  Matrix of recipients. Rows\[i\] = units, cols\[j\] = budget levels,
  fill = binary indicator to show if Unit\[i\] has access to
  budget\[j\].

- sense:

  Optimisation target, can be "min" or "max"

## Value

Optimsised solution
