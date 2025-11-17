# Create ompr model

Create ompr model

## Usage

``` r
create_model(n, options, z, cost, budget, budget_n, not_recipients, sense)
```

## Arguments

- n:

  Number of units

- options:

  Maximum number of options per unit

- z:

  Matrix of impact. Rows\[i\] = units, cols\[j\] = options, fill =
  impact measure. If all units do not have all options, fill z with NA.

- cost:

  Matrix of cost. Rows\[i\] = units, cols\[j\] = options, fill = cost.
  If all units do not have all options, fill cost with NA.

- budget:

  vector of budgets

- budget_n:

  Number of budget levels

- not_recipients:

  Binary matrix indicating units not allowed to access a given budget
  level

- sense:

  Optimisation target, can be "min" or "max"

## Value

ompr model
