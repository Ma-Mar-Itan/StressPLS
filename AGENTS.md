# AGENTS.md — stressPLS

## Project goal

Build `stressPLS`, an R package for stress-testing formative higher-order constructs in PLS-SEM.

The package should help researchers evaluate whether formative higher-order construct results are robust to reasonable perturbations in indicators, construct specifications, weighting schemes, resampling choices, and model assumptions.

## Core deliverables

1. A clean R package with:
   - DESCRIPTION
   - NAMESPACE
   - R/
   - tests/testthat/
   - vignettes/
   - man/
   - README.md
   - pkgdown-ready documentation
   - GitHub Actions R-CMD-check workflow

2. Core user-facing functions:
   - `stress_pls()`
   - `stress_specifications()`
   - `stress_indicators()`
   - `stress_weights()`
   - `stress_bootstrap()`
   - `summarise_stress()`
   - `plot_stress()`
   - `rank_fragility()`
   - `sensitivity_report()`

3. S3 classes:
   - `stresspls_result`
   - `stresspls_grid`
   - `stresspls_summary`

4. Tests:
   - Unit tests for every exported function.
   - Input validation tests.
   - Reproducibility tests using fixed seeds.
   - Snapshot tests for printed summaries where useful.
   - Error-message tests for invalid model/data inputs.

5. Documentation:
   - Roxygen comments for every exported function.
   - README with installation, minimal example, and conceptual explanation.
   - Vignette explaining the methodology.
   - Vignette using a simulated dataset.
   - Function reference documentation.

## Engineering rules

- Use idiomatic R.
- Prefer explicit validation over silent coercion.
- Keep functions small and composable.
- Avoid hidden global state.
- Every stochastic function must accept `seed`.
- Use `testthat`.
- Use `roxygen2`.
- Use `ggplot2` for plotting.
- Use `tibble`, `dplyr`, and `purrr` where helpful, but avoid unnecessary dependencies.
- Do not depend on experimental packages unless clearly justified.
- Do not fake statistical results.
- If a PLS-SEM backend is needed, design the package so backends are modular.

## Statistical design principles

The package should separate:

1. Model specification
2. Perturbation generation
3. Model estimation
4. Robustness scoring
5. Reporting and visualization

The stress-testing framework should support:

- Indicator deletion
- Indicator swapping
- Construct reweighting
- Bootstrap sensitivity
- Alternative higher-order construct specifications
- Multicollinearity diagnostics
- Stability of path coefficients
- Stability of construct scores
- Stability of significance decisions
- Fragility ranking

## Definition of done

A task is done only when:

1. Code is implemented.
2. Tests pass.
3. Documentation builds.
4. Examples run.
5. `devtools::check()` or `rcmdcheck::rcmdcheck()` passes, unless blocked by environment limitations.
6. The final response summarizes changed files, tests run, and remaining limitations.

## Commands

Use these commands when appropriate:

```r
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()