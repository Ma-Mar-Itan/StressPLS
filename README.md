
# stressPLS

`stressPLS` is an R package scaffold for stress-testing formative
higher-order constructs in PLS-SEM. The package is being built around a
clear separation between model specification, perturbation generation,
model estimation, robustness scoring, and reporting.

The initial package does not implement a PLS-SEM estimator. Instead, it
creates validated perturbation grids and result objects that can later
be connected to modular estimation backends.

## Installation

``` r
# install.packages("pak")
pak::pak("stressPLS")
```

## Minimal example

``` r
library(stressPLS)

dat <- data.frame(
  x1 = rnorm(20),
  x2 = rnorm(20),
  x3 = rnorm(20),
  y = rnorm(20)
)

model <- list(
  indicators = c("x1", "x2", "x3"),
  paths = list(y = c("x1", "x2", "x3"))
)

grid <- stress_indicators(dat, model)
result <- stress_pls(dat, model, grid = grid)

summarise_stress(result)
#> <stresspls_summary>
#> Scenarios: 3 
#>         status n
#>  not_estimated 3
rank_fragility(result)
#> [1] scenario_id     fragility_score rank           
#> <0 rows> (or 0-length row.names)
```

## Concept

Stress testing asks whether substantive conclusions survive reasonable
changes to indicators, higher-order construct specifications, weights,
bootstrap settings, and related assumptions. `stressPLS` will expose
those changes as explicit scenario grids, run them through a modular
backend, and report stability and fragility without fabricating
estimates when a backend is absent.
