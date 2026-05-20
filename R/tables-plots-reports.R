#' Make a baseline result table
#'
#' @param fit A `stresspls_fit`.
#'
#' @return A data frame.
#' @examples
#' make_baseline_table(as_stresspls_fit(list()))
#' @export
make_baseline_table <- function(fit) {
  if (!inherits(fit, "stresspls_fit")) {
    stop("`fit` must be a stresspls_fit object.", call. = FALSE)
  }
  rbind_list(list(
    add_component(fit$paths, "paths"),
    add_component(fit$weights, "weights")
  ))
}

#' Make an indicator perturbation table
#'
#' @param x A `stresspls_fit_grid` or `stresspls_perturbation_grid`.
#'
#' @return A data frame.
#' @examples
#' # See `perturb_indicators()` for construction.
#' @export
make_indicator_perturbation_table <- function(x) {
  if (inherits(x, "stresspls_perturbation_grid")) {
    return(x$scenarios)
  }
  if (inherits(x, "stresspls_fit_grid")) {
    return(calc_indicator_fragility(x))
  }
  stop("`x` must be a stresspls_fit_grid or stresspls_perturbation_grid.",
       call. = FALSE)
}

#' Make a bootstrap stability table
#'
#' @param x A `stresspls_bootstrap`.
#'
#' @return A data frame.
#' @examples
#' # See `bootstrap_stability()` for construction.
#' @export
make_bootstrap_stability_table <- function(x) {
  summarise_bootstrap_stability(x)
}

#' Make a collinearity stress table
#'
#' @param x A `stresspls_collinearity_stress`.
#'
#' @return A data frame.
#' @examples
#' # See `collinearity_stress_test()` for construction.
#' @export
make_collinearity_table <- function(x) {
  summarise_collinearity_stress(x)
}

#' Make a prediction validation table
#'
#' @param x A `stresspls_prediction_validation`.
#'
#' @return A data frame.
#' @examples
#' compare_prediction_metrics(data.frame(outcome = "y", metric = "rmse", value = 1))
#' @export
make_prediction_table <- function(x) {
  compare_prediction_metrics(x)
}

#' Make a heterogeneity table
#'
#' @param x A `stresspls_heterogeneity`.
#'
#' @return A data frame.
#' @examples
#' # See `subgroup_heterogeneity()` for construction.
#' @export
make_heterogeneity_table <- function(x) {
  rbind_list(list(
    add_component(compare_subgroup_paths(x), "paths"),
    add_component(compare_subgroup_weights(x), "weights")
  ))
}

#' Make a simulation design table
#'
#' @param x A `stresspls_simulation` object or design data frame.
#'
#' @return A data frame.
#' @examples
#' make_simulation_design_table(data.frame(n = 100))
#' @export
make_simulation_design_table <- function(x) {
  if (inherits(x, "stresspls_simulation")) {
    return(x$design)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  stop("`x` must be a stresspls_simulation object or data frame.",
       call. = FALSE)
}

#' Make a simulation results table
#'
#' @param x A `stresspls_simulation`.
#'
#' @return A data frame.
#' @examples
#' # See `run_simulation_grid()` for construction.
#' @export
make_simulation_results_table <- function(x) {
  summarise_simulation_results(x)
}

#' Plot the stressPLS workflow
#'
#' @return A `ggplot` object.
#' @examples
#' plot_stress_workflow()
#' @export
plot_stress_workflow <- function() {
  nodes <- data.frame(
    step = factor(c("Specification", "Perturbation", "Backend",
                    "Metrics", "Reporting"),
                  levels = c("Specification", "Perturbation", "Backend",
                             "Metrics", "Reporting")),
    x = 1:5,
    y = 1,
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(nodes, ggplot2::aes(x = x, y = y, label = step)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_text(vjust = -1) +
    ggplot2::geom_path() +
    ggplot2::labs(x = NULL, y = NULL, title = "stressPLS workflow") +
    ggplot2::theme_minimal()
}

#' Plot indicator stability as a heatmap
#'
#' @param x A `stresspls_fit_grid`.
#'
#' @return A `ggplot` object.
#' @examples
#' # See `fit_perturbation_grid()` for construction.
#' @export
plot_indicator_stability_heatmap <- function(x) {
  tab <- calc_weight_stability(x)
  if (nrow(tab) == 0L) {
    tab <- data.frame(indicator = character(), scenario_id = character(),
                      abs_difference = numeric())
  }
  ggplot2::ggplot(tab, ggplot2::aes(x = scenario_id, y = indicator,
                                    fill = abs_difference)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Scenario", y = "Indicator", fill = "|Change|") +
    ggplot2::theme_minimal()
}

#' Plot path estimate distributions
#'
#' @param x A `stresspls_fit_grid` or data frame from `extract_paths()`.
#'
#' @return A `ggplot` object.
#' @examples
#' plot_path_distribution(data.frame(from = "A", to = "B", estimate = 0.1))
#' @export
plot_path_distribution <- function(x) {
  paths <- if (inherits(x, "stresspls_fit_grid")) extract_paths(x) else x
  if (!is.data.frame(paths)) stop("`x` must provide path estimates.", call. = FALSE)
  paths$path <- paste(paths$from, paths$to, sep = " -> ")
  ggplot2::ggplot(paths, ggplot2::aes(x = estimate)) +
    ggplot2::geom_histogram(bins = 20) +
    ggplot2::facet_wrap(stats::as.formula("~ path"), scales = "free") +
    ggplot2::labs(x = "Estimate", y = "Count") +
    ggplot2::theme_minimal()
}

#' Plot a VIF stress curve
#'
#' @param x A `stresspls_collinearity_stress`.
#'
#' @return A `ggplot` object.
#' @examples
#' # See `collinearity_stress_test()` for construction.
#' @export
plot_vif_stress_curve <- function(x) {
  tab <- summarise_collinearity_stress(x)
  ggplot2::ggplot(tab, ggplot2::aes(x = level, y = max_vif)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Collinearity strength", y = "Maximum VIF") +
    ggplot2::theme_minimal()
}

#' Plot prediction metric comparisons
#'
#' @param x A `stresspls_prediction_validation` or metric table.
#'
#' @return A `ggplot` object.
#' @examples
#' plot_prediction_comparison(data.frame(outcome = "y", metric = "rmse", value = 1))
#' @export
plot_prediction_comparison <- function(x) {
  metrics <- if (inherits(x, "stresspls_prediction_validation")) x$metrics else x
  ggplot2::ggplot(metrics, ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(stats::as.formula("~ outcome"), scales = "free_y") +
    ggplot2::labs(x = "Metric", y = "Value") +
    ggplot2::theme_minimal()
}

#' Plot a compact robustness dashboard
#'
#' @param x A `stresspls_fit_grid`.
#'
#' @return A `ggplot` object.
#' @examples
#' # See `fit_perturbation_grid()` for construction.
#' @export
plot_robustness_dashboard <- function(x) {
  tab <- calc_indicator_fragility(x)
  ggplot2::ggplot(tab, ggplot2::aes(x = scenario_id, y = fragility_score)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Scenario", y = "Fragility score") +
    ggplot2::theme_minimal()
}

#' Plot simulation results
#'
#' @param x A `stresspls_simulation`.
#'
#' @return A `ggplot` object.
#' @examples
#' # See `run_simulation_grid()` for construction.
#' @export
plot_simulation_results <- function(x) {
  tab <- summarise_simulation_results(x)
  ggplot2::ggplot(tab, ggplot2::aes(x = factor(design_id), y = rmse)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Design", y = "RMSE") +
    ggplot2::theme_minimal()
}

#' Create a structured sensitivity report
#'
#' @param x Optional legacy `stresspls_result` or `stresspls_summary`.
#' @param baseline Optional baseline `stresspls_fit`.
#' @param perturbations Optional perturbation fit grid.
#' @param bootstrap Optional bootstrap object.
#' @param collinearity Optional collinearity stress object.
#' @param prediction Optional prediction validation object.
#' @param heterogeneity Optional heterogeneity object.
#' @param simulation Optional simulation object.
#' @param warnings Character warnings.
#' @param limitations Character limitations.
#'
#' @return A report object. Legacy result input returns the original text report.
#' @examples
#' sensitivity_report(limitations = "No estimator backend was run.")
#' @export
sensitivity_report <- function(x = NULL, baseline = NULL, perturbations = NULL,
                               bootstrap = NULL, collinearity = NULL,
                               prediction = NULL, heterogeneity = NULL,
                               simulation = NULL, warnings = character(),
                               limitations = character()) {
  if (inherits(x, "stresspls_result") || inherits(x, "stresspls_summary")) {
    return(legacy_sensitivity_report(x))
  }
  report <- list(
    baseline = if (!is.null(baseline)) make_baseline_table(baseline) else NULL,
    perturbations = if (!is.null(perturbations)) {
      make_indicator_perturbation_table(perturbations)
    } else NULL,
    bootstrap = if (!is.null(bootstrap)) {
      make_bootstrap_stability_table(bootstrap)
    } else NULL,
    collinearity = if (!is.null(collinearity)) {
      make_collinearity_table(collinearity)
    } else NULL,
    prediction = if (!is.null(prediction)) make_prediction_table(prediction) else NULL,
    heterogeneity = if (!is.null(heterogeneity)) {
      make_heterogeneity_table(heterogeneity)
    } else NULL,
    simulation = if (!is.null(simulation)) {
      make_simulation_results_table(simulation)
    } else NULL,
    warnings = as.character(warnings),
    limitations = as.character(limitations),
    created_at = Sys.time()
  )
  structure(report, class = "stresspls_sensitivity_report")
}

legacy_sensitivity_report <- function(x) {
  if (inherits(x, "stresspls_result")) {
    x <- summarise_stress(x)
  }
  total <- sum(x$status_counts$n)
  lines <- c(
    "stressPLS sensitivity report",
    paste0("Scenarios: ", total),
    paste0(x$status_counts$status, ": ", x$status_counts$n)
  )
  class(lines) <- c("stresspls_report", class(lines))
  lines
}

add_component <- function(x, component) {
  if (!is.data.frame(x)) {
    x <- data.frame()
  }
  x$component <- rep(component, nrow(x))
  x
}
