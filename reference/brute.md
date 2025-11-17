# Brute force search for single budget level

Brute force search for single budget level

## Usage

``` r
brute(z, cost, budget)
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

## Value

Brute force solution
