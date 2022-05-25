
<!-- README.md is generated from README.Rmd. Please edit that file -->

# om <img src="man/figures/om_hex.png" align="right" width=30% height=30% />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/om/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/om/actions)
[![Codecov test
coverage](https://codecov.io/gh/mrc-ide/om/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/om?branch=main)
<!-- badges: end -->

om can be used to find solutions to budget-optimisation challenges. This
might be finding the optimal mix of spatially-targeted intervention
packages across a country that are affordable within a specified budget
envelope. om can also deal with optimiastions where multiple budgets may
be available to difference subsets of spaital units. For example, we may
want to maximise the impact of a global-donor budget to be optimised
across multiple countries, combined with country domestic budgets.

All of the clever-stuff in the background is handled by the superb ompr
package. Please cite the ompr package when using om (`citation("ompr")`)
and be sure to check it out
[here](https://dirkschumacher.github.io/ompr/)!

To get started, take a look at the [introductory
vignette](https://mrc-ide.github.io/om/articles/Introduction.html)

Updates, reviews and improvements are encouraged via
[PRs](https://github.com/mrc-ide/om/pulls)!

Any issues or clarifications? Please post on the [issues page
here](https://github.com/mrc-ide/om/issues).

## Installation

You can install the development version of om like so:

``` r
remotes::install_github("mrc-ide/om")
```
