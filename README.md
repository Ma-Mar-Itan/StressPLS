
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

## Model specification example

``` r
image <- specify_construct(
  name = "Image",
  indicators = c("img1", "img2", "img3")
)

quality <- specify_construct(
  name = "Quality",
  indicators = c("qual1", "qual2", "qual3")
)

satisfaction <- specify_construct(
  name = "Satisfaction",
  indicators = c("sat1", "sat2", "sat3"),
  mode = "reflective"
)

brand_equity <- specify_hoc(
  name = "BrandEquity",
  dimensions = c("Image", "Quality")
)

paths <- specify_paths(
  from = "BrandEquity",
  to = "Satisfaction"
)

model_spec <- specify_model(
  constructs = list(image, quality, satisfaction),
  hocs = list(brand_equity),
  paths = paths
)

construct_names(model_spec)
#> [1] "Image"        "Quality"      "Satisfaction" "BrandEquity"
required_indicators(model_spec)
#> [1] "img1"  "img2"  "img3"  "qual1" "qual2" "qual3" "sat1"  "sat2"  "sat3"
```

## Perturbation example

``` r
indicator_grid <- perturb_indicators(
  model = model_spec,
  method = "leave_one_out",
  constructs = c("Image", "Quality")
)

indicator_grid
#> <stresspls_perturbation_grid>
#> Scenarios: 6 
#> Methods: leave_one_out 
#> Affected constructs: Image, Quality 
#> Data materialized: no 
#>                    scenario_id        method construct valid
#>     leave_one_out__image__img1 leave_one_out     Image  TRUE
#>     leave_one_out__image__img2 leave_one_out     Image  TRUE
#>     leave_one_out__image__img3 leave_one_out     Image  TRUE
#>  leave_one_out__quality__qual1 leave_one_out   Quality  TRUE
#>  leave_one_out__quality__qual2 leave_one_out   Quality  TRUE
#>  leave_one_out__quality__qual3 leave_one_out   Quality  TRUE
```

## Concept

Stress testing asks whether substantive conclusions survive reasonable
changes to indicators, higher-order construct specifications, weights,
bootstrap settings, and related assumptions. `stressPLS` will expose
those changes as explicit scenario grids, run them through a modular
backend, and report stability and fragility without fabricating
estimates when a backend is absent. Future estimation backends will
consume `stresspls_model_spec` objects and `stresspls_perturbation_grid`
objects.
