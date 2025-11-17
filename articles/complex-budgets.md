# Complex budgets

``` r
library(om)
#> Loading required package: ROI.plugin.glpk
#> Loading required package: ompr
#> Loading required package: ompr.roi
```

In the introductory vignette, we saw how Omland could optimise the
national budget given the following modelled cost and impact estimates

``` r
omland_output <- data.frame(
  unit = rep(1:10, each = 5),
  option = rep(1:5, 10),
  impact = rep(0:4, 10),
  cost = rep(c(0, 5, 10, 15, 20), 10)  * rep(c(rep(1, 5), rep(5, 5)), each = 5)
)
impact_matrix <- matrix(omland_output$impact, nrow = 10, ncol =  5, byrow = TRUE)
cost_matrix <- matrix(omland_output$cost, nrow = 10, ncol =  5, byrow = TRUE)
```

Breaking news!!! A regional initiative has been set up to increase
funding in two of Omland’s sub-national units, units number 9 and 10. An
additional \$200 has been made available to Omland, but spending must be
restricted units 9 and 10.

Fortunately for Omland, om can deal with multiple budget levels. Let’s
look at how we can set this up. We now specify two budgets, the national
and regional

``` r
budget <- c(100, 200)
```

We also need to specify which sub-national units have access to which
parts of the budget. We can do this with another matrix. In the
`recipients` matrix, each row is a sub-national unit, and each column
corresponds to a level in our budget vector. A `1` indicates that a
sub-national unit may receive a share of that budget, a `0` indicates it
can not receive any of it.

``` r
recipients <- cbind(
  rep(1, 10),
  c(rep(0, 8), rep(1, 2))
)
recipients
#>       [,1] [,2]
#>  [1,]    1    0
#>  [2,]    1    0
#>  [3,]    1    0
#>  [4,]    1    0
#>  [5,]    1    0
#>  [6,]    1    0
#>  [7,]    1    0
#>  [8,]    1    0
#>  [9,]    1    1
#> [10,]    1    1
```

Here, we see that only rows 9 and 10 have access to both budget levels.
Now we can optimise again

``` r
optimised <- om(z = impact_matrix, cost = cost_matrix, budget = budget,
                recipients = recipients)
optimised
#>     i j cost z budget_level_1 budget_level_2
#> 1   1 5   20 4             20              0
#> 2   2 5   20 4             20              0
#> 3   3 5   20 4             20              0
#> 4   4 5   20 4             20              0
#> 5   5 5   20 4             20              0
#> 6   6 1    0 0              0              0
#> 7   7 1    0 0              0              0
#> 8   8 1    0 0              0              0
#> 9   9 5  100 4              0            100
#> 10 10 5  100 4              0            100
```

Now we see that the first budget level, available to all of Omland, is
shared across the first 5 sub-national units, just like in the
introductory example. However, thanks to the regional initiative, Omland
can now spend an additional \$200 in sub-national units 9 and 10,
improving the overall impact. The columns `budget_level_1` and
`budget_level_2` show us how each section of Omland’s budget is
distributed.

You can add more budget levels by modifying the `budget` and
`recipients` inputs.
