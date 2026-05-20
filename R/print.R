#' @export
print.stresspls_grid <- function(x, ...) {
  cat("<stresspls_grid>\n")
  cat("Scenarios:", nrow(x$scenarios), "\n")
  if (!is.null(x$metadata$type)) {
    cat("Type:", x$metadata$type, "\n")
  }
  print(utils::head(x$scenarios), row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_result <- function(x, ...) {
  cat("<stresspls_result>\n")
  cat("Scenarios:", nrow(x$results), "\n")
  backend <- if (is.null(x$backend)) "none" else x$backend
  cat("Backend:", backend, "\n")
  print(utils::head(x$results), row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_summary <- function(x, ...) {
  cat("<stresspls_summary>\n")
  cat("Scenarios:", sum(x$status_counts$n), "\n")
  print(x$status_counts, row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_construct_spec <- function(x, ...) {
  cat("<stresspls_construct_spec>\n")
  cat("Name:", x$name, "\n")
  cat("Mode:", x$mode, "\n")
  cat("Indicators:", paste(x$indicators, collapse = ", "), "\n")
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_hoc_spec <- function(x, ...) {
  cat("<stresspls_hoc_spec>\n")
  cat("Name:", x$name, "\n")
  cat("Mode:", x$mode, "\n")
  cat("Approach:", x$approach, "\n")
  cat("Dimensions:", paste(x$dimensions, collapse = ", "), "\n")
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_path_spec <- function(x, ...) {
  cat("<stresspls_path_spec>\n")
  cat("Paths:", nrow(x$paths), "\n")
  print(x$paths, row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_model_spec <- function(x, ...) {
  cat("<stresspls_model_spec>\n")
  cat("Constructs:", length(x$constructs), "\n")
  cat("Higher-order constructs:", length(x$hocs), "\n")
  cat("Paths:", nrow(x$paths$paths), "\n")
  if (length(construct_names(x)) > 0L) {
    cat("Names:", paste(construct_names(x), collapse = ", "), "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_perturbation_grid <- function(x, ...) {
  cat("<stresspls_perturbation_grid>\n")
  cat("Scenarios:", nrow(x$scenarios), "\n")
  cat("Methods:", paste(x$methods, collapse = ", "), "\n")
  affected <- unique(x$scenarios$construct)
  affected <- affected[nzchar(affected)]
  cat("Affected constructs:", if (length(affected) == 0L) {
    "none"
  } else {
    paste(affected, collapse = ", ")
  }, "\n")
  materialized <- isTRUE(x$metadata$data_materialized)
  cat("Data materialized:", if (materialized) "yes" else "no", "\n")
  if (nrow(x$scenarios) > 0L) {
    preview <- x$scenarios[, c(
      "scenario_id",
      "method",
      "construct",
      "valid"
    ), drop = FALSE]
    print(utils::head(preview), row.names = FALSE)
  }
  invisible(x)
}

#' @export
print.stresspls_backend <- function(x, ...) {
  cat("<stresspls_backend>\n")
  cat("Name:", x$name, "\n")
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_fit <- function(x, ...) {
  cat("<stresspls_fit>\n")
  cat("Scenario:", scenario_id_from_fit(x), "\n")
  cat("Converged:", if (isTRUE(x$diagnostics$converged)) "yes" else "no", "\n")
  cat("Paths:", nrow(x$paths), "\n")
  cat("Weights:", nrow(x$weights), "\n")
  if (length(x$diagnostics$errors) > 0L) {
    cat("Errors:", paste(x$diagnostics$errors, collapse = "; "), "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_fit_grid <- function(x, ...) {
  cat("<stresspls_fit_grid>\n")
  cat("Scenarios:", length(x$fits), "\n")
  cat("Backend:", x$backend$name, "\n")
  cat("Converged:", sum(x$scenario_index$converged), "/", nrow(x$scenario_index), "\n")
  if (nrow(x$scenario_index) > 0L) {
    print(utils::head(x$scenario_index), row.names = FALSE)
  }
  invisible(x)
}

#' @export
print.stresspls_bootstrap <- function(x, ...) {
  cat("<stresspls_bootstrap>\n")
  cat("Replications:", nrow(x$index), "\n")
  cat("Converged:", sum(x$index$converged), "/", nrow(x$index), "\n")
  invisible(x)
}

#' @export
print.stresspls_collinearity_recipe <- function(x, ...) {
  cat("<stresspls_collinearity_recipe>\n")
  cat("Indicators:", paste(x$recipe$indicators, collapse = ", "), "\n")
  cat("Strength:", x$recipe$strength, "\n")
  invisible(x)
}

#' @export
print.stresspls_collinearity_stress <- function(x, ...) {
  cat("<stresspls_collinearity_stress>\n")
  cat("Levels:", paste(x$levels, collapse = ", "), "\n")
  cat("Indicators:", paste(x$indicators, collapse = ", "), "\n")
  cat("Converged:", sum(x$index$converged), "/", nrow(x$index), "\n")
  invisible(x)
}

#' @export
print.stresspls_prediction_validation <- function(x, ...) {
  cat("<stresspls_prediction_validation>\n")
  cat("Folds:", nrow(x$fold_index), "\n")
  cat("Outcomes:", paste(x$outcomes, collapse = ", "), "\n")
  if (nrow(x$metrics) > 0L) {
    print(utils::head(compare_prediction_metrics(x)), row.names = FALSE)
  }
  invisible(x)
}

#' @export
print.stresspls_heterogeneity <- function(x, ...) {
  cat("<stresspls_heterogeneity>\n")
  cat("Group:", x$group, "\n")
  cat("Subgroups:", paste(x$index$group, collapse = ", "), "\n")
  cat("Converged:", sum(x$index$converged), "/", nrow(x$index), "\n")
  invisible(x)
}

#' @export
print.stresspls_simulated_data <- function(x, ...) {
  cat("<stresspls_simulated_data>\n")
  cat("Rows:", nrow(x$data), "\n")
  cat("Columns:", ncol(x$data), "\n")
  invisible(x)
}

#' @export
print.stresspls_simulation <- function(x, ...) {
  cat("<stresspls_simulation>\n")
  cat("Design rows:", nrow(x$design), "\n")
  cat("Replications:", nrow(x$results), "\n")
  invisible(x)
}

#' @export
print.stresspls_sensitivity_report <- function(x, ...) {
  cat("<stresspls_sensitivity_report>\n")
  sections <- setdiff(names(x), c("warnings", "limitations", "created_at"))
  present <- sections[vapply(x[sections], Negate(is.null), logical(1))]
  cat("Sections:", if (length(present) == 0L) "none" else paste(present, collapse = ", "), "\n")
  if (length(x$warnings) > 0L) {
    cat("Warnings:", paste(x$warnings, collapse = "; "), "\n")
  }
  if (length(x$limitations) > 0L) {
    cat("Limitations:", paste(x$limitations, collapse = "; "), "\n")
  }
  invisible(x)
}
