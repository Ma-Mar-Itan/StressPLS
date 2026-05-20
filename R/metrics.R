#' Calculate sign consistency
#'
#' @param x Numeric estimates or a `stresspls_fit_grid`.
#' @param type Estimate type for fit grids: `"paths"` or `"weights"`.
#'
#' @return A numeric value for numeric input, otherwise a tidy data frame.
#' @examples
#' calc_sign_consistency(c(-1, -0.5, 0.2))
#' @export
calc_sign_consistency <- function(x, type = c("paths", "weights")) {
  if (is.numeric(x)) {
    return(sign_consistency_scalar(x))
  }
  type <- match.arg(type)
  grouped_metric(x, type, function(z) sign_consistency_scalar(z))
}

#' Calculate direction flip rate
#'
#' @param x Numeric estimates or a `stresspls_fit_grid`.
#' @param theta_hat Baseline estimate for numeric input.
#' @param type Estimate type for fit grids.
#'
#' @return A numeric value for numeric input, otherwise a tidy data frame.
#' @examples
#' calc_direction_flip_rate(c(-1, 0.5, 0.2), theta_hat = 0.4)
#' @export
calc_direction_flip_rate <- function(x, theta_hat = NULL,
                                     type = c("paths", "weights")) {
  if (is.numeric(x)) {
    if (is.null(theta_hat) || length(theta_hat) != 1L || is.na(theta_hat)) {
      stop("`theta_hat` must be a single non-missing baseline estimate.",
           call. = FALSE)
    }
    return(mean(sign(x) != sign(theta_hat), na.rm = TRUE))
  }
  type <- match.arg(type)
  stability <- if (type == "paths") calc_path_stability(x) else
    calc_weight_stability(x)
  aggregate_metric(stability, "direction_flip", type)
}

#' Calculate confidence interval width
#'
#' @param x Numeric estimates or a `stresspls_fit_grid`.
#' @param type Estimate type for fit grids.
#'
#' @return A numeric value for numeric input, otherwise a tidy data frame.
#' @examples
#' calc_ci_width(c(0.1, 0.2, 0.4, 0.5))
#' @export
calc_ci_width <- function(x, type = c("paths", "weights")) {
  if (is.numeric(x)) {
    return(ci_width_scalar(x))
  }
  type <- match.arg(type)
  grouped_metric(x, type, function(z) ci_width_scalar(z))
}

#' Calculate scaled confidence interval width
#'
#' @param x Numeric estimates or a `stresspls_fit_grid`.
#' @param theta_hat Baseline estimate for numeric input.
#' @param type Estimate type for fit grids.
#'
#' @return A numeric value for numeric input, otherwise a tidy data frame.
#' @examples
#' calc_scaled_ci_width(c(0.1, 0.2, 0.4), theta_hat = 0.2)
#' @export
calc_scaled_ci_width <- function(x, theta_hat = NULL,
                                 type = c("paths", "weights")) {
  if (is.numeric(x)) {
    if (is.null(theta_hat) || length(theta_hat) != 1L || is.na(theta_hat)) {
      stop("`theta_hat` must be a single non-missing baseline estimate.",
           call. = FALSE)
    }
    width <- ci_width_scalar(x)
    if (is.na(width) || theta_hat == 0) {
      return(NA_real_)
    }
    return(width / abs(theta_hat))
  }
  type <- match.arg(type)
  estimates <- estimates_with_keys(x, type)
  baseline <- baseline_estimates(x, type)
  metric_by_key(estimates, baseline, function(z, b) {
    calc_scaled_ci_width(z, theta_hat = b)
  }, "scaled_ci_width", type)
}

#' Calculate a stability index
#'
#' @param x Numeric estimates or a `stresspls_fit_grid`.
#' @param type Estimate type for fit grids.
#'
#' @return A numeric value for numeric input, otherwise a tidy data frame.
#' @examples
#' calc_stability_index(c(0.1, 0.2, 0.3))
#' @export
calc_stability_index <- function(x, type = c("paths", "weights")) {
  if (is.numeric(x)) {
    return(stability_index_scalar(x))
  }
  type <- match.arg(type)
  grouped_metric(x, type, function(z) stability_index_scalar(z))
}

#' Calculate indicator perturbation fragility
#'
#' @param x A `stresspls_fit_grid`.
#' @param baseline Optional baseline `stresspls_fit`.
#'
#' @return A data frame with one row per scenario.
#' @examples
#' # See `fit_perturbation_grid()` for fit-grid construction.
#' @export
calc_indicator_fragility <- function(x, baseline = NULL) {
  if (!inherits(x, "stresspls_fit_grid")) {
    stop("`x` must be a stresspls_fit_grid object.", call. = FALSE)
  }
  path_stability <- calc_path_stability(x, baseline = baseline)
  weight_stability <- calc_weight_stability(x, baseline = baseline)
  pieces <- list()
  if (nrow(path_stability) > 0L) {
    pieces[[length(pieces) + 1L]] <- data.frame(
      scenario_id = path_stability$scenario_id,
      component = "paths",
      abs_change = abs(path_stability$difference),
      direction_flip = path_stability$direction_flip,
      stringsAsFactors = FALSE
    )
  }
  if (nrow(weight_stability) > 0L) {
    pieces[[length(pieces) + 1L]] <- data.frame(
      scenario_id = weight_stability$scenario_id,
      component = "weights",
      abs_change = abs(weight_stability$difference),
      direction_flip = weight_stability$direction_flip,
      stringsAsFactors = FALSE
    )
  }
  changes <- rbind_list(pieces)
  if (nrow(changes) == 0L) {
    return(empty_fragility_table())
  }
  scenarios <- unique(changes$scenario_id)
  out <- do.call(rbind, lapply(scenarios, function(id) {
    rows <- changes[changes$scenario_id == id, , drop = FALSE]
    data.frame(
      scenario_id = id,
      fragility_score = mean(rows$abs_change, na.rm = TRUE) +
        mean(rows$direction_flip, na.rm = TRUE),
      mean_abs_change = mean(rows$abs_change, na.rm = TRUE),
      direction_flip_rate = mean(rows$direction_flip, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out <- merge(out, x$grid$scenarios[, c("scenario_id", "method", "construct")],
               by = "scenario_id", all.x = TRUE, sort = FALSE)
  out
}

#' Calculate structural path stability
#'
#' @param x A `stresspls_fit_grid`.
#' @param baseline Optional baseline `stresspls_fit`.
#'
#' @return A tidy data frame comparing scenario paths to baseline paths.
#' @examples
#' # See `fit_perturbation_grid()` for fit-grid construction.
#' @export
calc_path_stability <- function(x, baseline = NULL) {
  stability_table(x, baseline, "paths", c("from", "to"))
}

#' Calculate formative weight stability
#'
#' @param x A `stresspls_fit_grid`.
#' @param baseline Optional baseline `stresspls_fit`.
#'
#' @return A tidy data frame comparing scenario weights to baseline weights.
#' @examples
#' # See `fit_perturbation_grid()` for fit-grid construction.
#' @export
calc_weight_stability <- function(x, baseline = NULL) {
  stability_table(x, baseline, "weights", c("construct", "indicator"))
}

sign_consistency_scalar <- function(theta) {
  theta <- theta[!is.na(theta) & theta != 0]
  if (length(theta) == 0L) {
    return(NA_real_)
  }
  max(mean(theta > 0), mean(theta < 0))
}

ci_width_scalar <- function(theta) {
  theta <- theta[!is.na(theta)]
  if (length(theta) < 2L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(theta, 0.975, names = FALSE) -
    stats::quantile(theta, 0.025, names = FALSE))
}

stability_index_scalar <- function(theta) {
  theta <- abs(theta[!is.na(theta)])
  if (length(theta) < 2L || mean(theta) == 0) {
    return(NA_real_)
  }
  1 / (1 + stats::sd(theta) / mean(theta))
}

stability_table <- function(x, baseline = NULL, table, keys) {
  if (!inherits(x, "stresspls_fit_grid")) {
    stop("`x` must be a stresspls_fit_grid object.", call. = FALSE)
  }
  if (is.null(baseline)) {
    baseline <- x$baseline
  }
  if (is.null(baseline)) {
    stop("A baseline fit is required for stability metrics.", call. = FALSE)
  }
  base <- baseline[[table]]
  scen <- extract_fit_table(x, table)
  if (nrow(base) == 0L || nrow(scen) == 0L) {
    return(data.frame())
  }
  names(base)[names(base) == "estimate"] <- "baseline_estimate"
  keep_base <- c(keys, "baseline_estimate", "significant")
  base <- base[, intersect(keep_base, names(base)), drop = FALSE]
  names(base)[names(base) == "significant"] <- "baseline_significant"
  merged <- merge(scen, base, by = keys, all.x = TRUE, sort = FALSE)
  merged$difference <- merged$estimate - merged$baseline_estimate
  merged$abs_difference <- abs(merged$difference)
  merged$direction_flip <- sign(merged$estimate) != sign(merged$baseline_estimate)
  merged$significance_change <- if ("significant" %in% names(merged) &&
                                    "baseline_significant" %in% names(merged)) {
    merged$significant != merged$baseline_significant
  } else {
    NA
  }
  merged
}

estimates_with_keys <- function(x, type) {
  table <- if (type == "paths") "paths" else "weights"
  extract_fit_table(x, table)
}

baseline_estimates <- function(x, type) {
  table <- if (type == "paths") "paths" else "weights"
  if (is.null(x$baseline)) {
    stop("A baseline fit is required.", call. = FALSE)
  }
  x$baseline[[table]]
}

grouped_metric <- function(x, type, fun) {
  estimates <- estimates_with_keys(x, type)
  key_cols <- if (type == "paths") c("from", "to") else c("construct", "indicator")
  if (nrow(estimates) == 0L) {
    return(data.frame())
  }
  split_key <- key_interaction(estimates, key_cols)
  out <- lapply(split(estimates, split_key), function(rows) {
    data.frame(rows[1, key_cols, drop = FALSE],
               value = fun(rows$estimate),
               stringsAsFactors = FALSE)
  })
  rbind_list(out)
}

metric_by_key <- function(estimates, baseline, fun, metric_name, type) {
  key_cols <- if (type == "paths") c("from", "to") else c("construct", "indicator")
  if (nrow(estimates) == 0L || nrow(baseline) == 0L) {
    return(data.frame())
  }
  out <- lapply(split(estimates, key_interaction(estimates, key_cols)),
                function(rows) {
    base_row <- merge(rows[1, key_cols, drop = FALSE], baseline,
                      by = key_cols, all.x = TRUE)
    data.frame(rows[1, key_cols, drop = FALSE],
               metric = metric_name,
               value = fun(rows$estimate, base_row$estimate[[1]]),
               stringsAsFactors = FALSE)
  })
  rbind_list(out)
}

aggregate_metric <- function(stability, column, type) {
  if (nrow(stability) == 0L) {
    return(data.frame())
  }
  key_cols <- if (type == "paths") c("from", "to") else c("construct", "indicator")
  out <- lapply(split(stability, key_interaction(stability, key_cols)),
                function(rows) {
    data.frame(rows[1, key_cols, drop = FALSE],
               value = mean(rows[[column]], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  rbind_list(out)
}

key_interaction <- function(data, key_cols) {
  do.call(interaction, c(data[key_cols], list(drop = TRUE, sep = "\r")))
}
