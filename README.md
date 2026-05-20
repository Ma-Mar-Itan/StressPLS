
# stressPLS

`stressPLS` is an R package scaffold for stress-testing formative
higher-order constructs in PLS-SEM. The package is being built around a
clear separation between model specification, perturbation generation,
model estimation, robustness scoring, and reporting.

The package does not fabricate PLS-SEM estimates. Instead, it creates
validated model and perturbation objects, runs user-supplied estimator
backends, and standardizes the resulting diagnostics. The examples below
use a deterministic toy backend that is only for demonstrating the API.

Terminology is deliberately conservative: robustness means conclusions
remain substantively unchanged under plausible perturbations; stability
means weights, paths, signs, and predictive metrics remain consistent
across perturbation or resampling; resilience means the substantive
interpretation withstands multiple stressors; fragility means
conclusions change materially under small or plausible changes; and
predictive validity means satisfactory out-of-sample or cross-validation
performance supplied or enabled by the backend.

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

## Custom backend example

Backends are regular R functions with the signature
`function(model, data, scenario = NULL, ...)`. They should return
canonical tables such as `paths`, `weights`, `vifs`, `r2`, and
prediction metrics.

``` r
demo_data <- data.frame(
  img1 = 1:12,
  img2 = 2:13,
  img3 = 3:14,
  qual1 = 4:15,
  qual2 = 5:16,
  qual3 = 6:17,
  sat1 = 7:18,
  sat2 = 8:19,
  sat3 = 9:20,
  group = rep(c("a", "b"), each = 6)
)

toy_backend <- function(model, data, scenario = NULL, ...) {
  brand_score <- rowMeans(data[c("img1", "img2", "img3", "qual1", "qual2", "qual3")])
  sat_score <- rowMeans(data[c("sat1", "sat2")])
  path_estimate <- unname(coef(lm(sat_score ~ brand_score))[2])

  list(
    paths = data.frame(
      from = "BrandEquity",
      to = "Satisfaction",
      estimate = path_estimate,
      significant = TRUE
    ),
    weights = data.frame(
      construct = "Image",
      indicator = c("img1", "img2", "img3"),
      estimate = colMeans(data[c("img1", "img2", "img3")]) / 100
    ),
    vifs = data.frame(
      construct = "Image",
      indicator = c("img1", "img2", "img3"),
      vif = c(1.2, 1.3, 1.4)
    ),
    prediction = data.frame(outcome = "sat1", metric = "rmse", value = 1)
  )
}

backend <- as_backend(toy_backend, name = "toy-example")
baseline <- fit_baseline_model(model_spec, demo_data, backend)
fit_grid <- fit_perturbation_grid(model_spec, demo_data, indicator_grid, backend)
```

## Stability metrics example

``` r
calc_path_stability(fit_grid)
#>          from           to estimate std_error statistic p_value ci_low ci_high
#> 1 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#> 2 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#> 3 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#> 4 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#> 5 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#> 6 BrandEquity Satisfaction        1        NA        NA      NA     NA      NA
#>   significant                   scenario_id baseline_estimate
#> 1        TRUE    leave_one_out__image__img1                 1
#> 2        TRUE    leave_one_out__image__img2                 1
#> 3        TRUE    leave_one_out__image__img3                 1
#> 4        TRUE leave_one_out__quality__qual1                 1
#> 5        TRUE leave_one_out__quality__qual2                 1
#> 6        TRUE leave_one_out__quality__qual3                 1
#>   baseline_significant difference abs_difference direction_flip
#> 1                 TRUE          0              0          FALSE
#> 2                 TRUE          0              0          FALSE
#> 3                 TRUE          0              0          FALSE
#> 4                 TRUE          0              0          FALSE
#> 5                 TRUE          0              0          FALSE
#> 6                 TRUE          0              0          FALSE
#>   significance_change
#> 1               FALSE
#> 2               FALSE
#> 3               FALSE
#> 4               FALSE
#> 5               FALSE
#> 6               FALSE
calc_indicator_fragility(fit_grid)
#>                     scenario_id fragility_score mean_abs_change
#> 1    leave_one_out__image__img1               0               0
#> 2    leave_one_out__image__img2               0               0
#> 3    leave_one_out__image__img3               0               0
#> 4 leave_one_out__quality__qual1               0               0
#> 5 leave_one_out__quality__qual2               0               0
#> 6 leave_one_out__quality__qual3               0               0
#>   direction_flip_rate        method construct
#> 1                   0 leave_one_out     Image
#> 2                   0 leave_one_out     Image
#> 3                   0 leave_one_out     Image
#> 4                   0 leave_one_out   Quality
#> 5                   0 leave_one_out   Quality
#> 6                   0 leave_one_out   Quality
rank_fragility(fit_grid)
#>                     scenario_id fragility_score mean_abs_change
#> 1    leave_one_out__image__img1               0               0
#> 2    leave_one_out__image__img2               0               0
#> 3    leave_one_out__image__img3               0               0
#> 4 leave_one_out__quality__qual1               0               0
#> 5 leave_one_out__quality__qual2               0               0
#> 6 leave_one_out__quality__qual3               0               0
#>   direction_flip_rate        method construct rank
#> 1                   0 leave_one_out     Image    1
#> 2                   0 leave_one_out     Image    2
#> 3                   0 leave_one_out     Image    3
#> 4                   0 leave_one_out   Quality    4
#> 5                   0 leave_one_out   Quality    5
#> 6                   0 leave_one_out   Quality    6
```

## Bootstrap example

``` r
boot <- bootstrap_stability(model_spec, demo_data, backend, R = 5, seed = 1)
summarise_bootstrap_stability(boot)
#>           from           to        value        type           metric construct
#> 1  BrandEquity Satisfaction 1.000000e+00        path sign_consistency      <NA>
#> 2         <NA>         <NA> 1.000000e+00      weight sign_consistency     Image
#> 3         <NA>         <NA> 1.000000e+00      weight sign_consistency     Image
#> 4         <NA>         <NA> 1.000000e+00      weight sign_consistency     Image
#> 5  BrandEquity Satisfaction 7.771561e-16        path         ci_width      <NA>
#> 6         <NA>         <NA> 2.025000e-02      weight         ci_width     Image
#> 7         <NA>         <NA> 2.025000e-02      weight         ci_width     Image
#> 8         <NA>         <NA> 2.025000e-02      weight         ci_width     Image
#> 9         <NA>         <NA> 1.000000e+00 diagnostics convergence_rate      <NA>
#> 10        <NA>         <NA> 0.000000e+00 diagnostics       error_rate      <NA>
#>    indicator
#> 1       <NA>
#> 2       img1
#> 3       img2
#> 4       img3
#> 5       <NA>
#> 6       img1
#> 7       img2
#> 8       img3
#> 9       <NA>
#> 10      <NA>
```

## Collinearity stress example

``` r
col_stress <- collinearity_stress_test(
  model_spec,
  demo_data,
  backend,
  indicators = c("img1", "img2", "img3"),
  levels = c(0, 0.3)
)
make_collinearity_table(col_stress)
#>   level    scenario_id converged max_vif mean_path_abs_change
#> 1   0.0 collinearity_1      TRUE     1.4                    0
#> 2   0.3 collinearity_2      TRUE     1.4                    0
```

## Prediction validation example

``` r
cv_backend <- function(model, data, scenario = NULL, ...) {
  test <- scenario$test_data[[1]]
  list(predictions = data.frame(
    outcome = "sat1",
    observed = test$sat1,
    predicted = mean(data$sat1)
  ))
}

cv <- repeated_cv_predict(
  model_spec,
  demo_data,
  cv_backend,
  outcomes = "sat1",
  v = 3,
  repeats = 1,
  seed = 1
)
compare_prediction_metrics(cv)
#>   outcome metric     mean        sd n
#> 1    sat1    mae 3.833333 0.4018188 3
#> 2    sat1   rmse 4.144350 0.3980192 3
```

## Heterogeneity example

``` r
het <- subgroup_heterogeneity(
  model_spec,
  demo_data,
  backend,
  group = "group",
  min_n = 3
)
compare_subgroup_paths(het)
#>          from           to min_estimate max_estimate        range groups
#> 1 BrandEquity Satisfaction            1            1 2.109424e-15   a, b
```

## Simulation example

``` r
sim <- simulate_formative_hoc_data(n = 30, seed = 1)
simulation <- run_simulation_grid(data.frame(n = 30), backend, replications = 1, seed = 1)
summarise_simulation_results(simulation)
#>   design_id replications  mean_bias      rmse sign_consistency
#> 1         1            1 -0.1720763 0.1720763                1
#>   direction_flip_rate ci_width stability_index convergence_rate error_rate
#> 1                   0       NA              NA                1          0
```

## Sensitivity report object

``` r
report <- sensitivity_report(
  baseline = baseline,
  perturbations = fit_grid,
  bootstrap = boot,
  collinearity = col_stress,
  prediction = cv,
  heterogeneity = het,
  simulation = simulation,
  limitations = "Examples use a deterministic toy backend, not a PLS-SEM estimator."
)
report
#> <stresspls_sensitivity_report>
#> Sections: baseline, perturbations, bootstrap, collinearity, prediction, heterogeneity, simulation 
#> Limitations: Examples use a deterministic toy backend, not a PLS-SEM estimator.
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

## Current limitations

`stressPLS` is an experimental package. The safest supported route is a
custom backend supplied by the user. Adapter functions for `seminr`,
`cSEM`, `plspm`, and SmartPLS exports are documented skeletons unless
users provide package-specific estimator or parser functions. Simulation
and stress diagnostics are intended for robustness analysis, not as
substitutes for a validated PLS-SEM estimator.
