#' Create a stressPLS backend
#'
#' `as_backend()` wraps an estimator function in a stable backend object. The
#' wrapped function must accept at least `model` and `data`; it should also
#' accept `scenario` or `...`.
#'
#' @param backend Function or existing `stresspls_backend`.
#' @param name Optional backend name.
#' @param description Optional backend description.
#'
#' @return A `stresspls_backend` object.
#' @examples
#' toy_backend <- function(model, data, scenario = NULL, ...) {
#'   list(paths = data.frame(from = "A", to = "B", estimate = 0.1))
#' }
#' as_backend(toy_backend, name = "toy")
#' @export
as_backend <- function(backend, name = NULL, description = NULL) {
  if (inherits(backend, "stresspls_backend")) {
    return(validate_backend(backend))
  }
  if (!is.function(backend)) {
    stop("`backend` must be a function or stresspls_backend object.",
         call. = FALSE)
  }
  formals_names <- names(formals(backend))
  if (!all(c("model", "data") %in% formals_names)) {
    stop("`backend` must have formal arguments `model` and `data`.",
         call. = FALSE)
  }
  if (!("scenario" %in% formals_names) && !("..." %in% formals_names)) {
    stop("`backend` must accept `scenario` or `...`.", call. = FALSE)
  }
  if (is.null(name)) {
    name <- "custom"
  }
  name <- validate_single_string(name, "name")
  if (!is.null(description)) {
    description <- validate_single_string(description, "description")
  }

  structure(
    list(
      fun = backend,
      name = name,
      description = description
    ),
    class = "stresspls_backend"
  )
}

#' Validate a stressPLS backend
#'
#' @param backend A backend function or `stresspls_backend` object.
#'
#' @return The validated `stresspls_backend`, invisibly.
#' @examples
#' toy_backend <- function(model, data, scenario = NULL, ...) list()
#' validate_backend(as_backend(toy_backend))
#' @export
validate_backend <- function(backend) {
  if (!inherits(backend, "stresspls_backend")) {
    backend <- as_backend(backend)
  }
  if (!is.function(backend$fun)) {
    stop("`backend$fun` must be a function.", call. = FALSE)
  }
  invisible(backend)
}

#' Fit a baseline model through a backend
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame containing required indicators.
#' @param backend Backend function or `stresspls_backend`.
#' @param ... Extra arguments passed to the backend.
#'
#' @return A `stresspls_fit` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:4, img2 = 2:5)
#' toy_backend <- function(model, data, scenario = NULL, ...) {
#'   list(weights = data.frame(construct = "Image", indicator = "img1",
#'                             estimate = mean(data$img1)))
#' }
#' fit_baseline_model(model, dat, toy_backend)
#' @export
fit_baseline_model <- function(model, data, backend, ...) {
  validate_model_spec(model, data = data)
  backend <- as_backend(backend)
  validate_backend(backend)
  output <- backend$fun(model = model, data = data, scenario = NULL, ...)
  as_stresspls_fit(
    output,
    model = model,
    data = data,
    scenario = NULL,
    backend = backend
  )
}

#' Fit all valid scenarios in a perturbation grid
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame containing required indicators.
#' @param grid A `stresspls_perturbation_grid`.
#' @param backend Backend function or `stresspls_backend`.
#' @param continue_on_error If `TRUE`, scenario-level backend errors are stored
#'   as failed fit diagnostics. If `FALSE`, the first error is raised.
#' @param fit_baseline If `TRUE`, store a baseline fit on the returned object.
#' @param ... Extra arguments passed to the backend.
#'
#' @return A `stresspls_fit_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2", "img3"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:4, img2 = 2:5, img3 = 3:6)
#' grid <- leave_one_indicator_out(model)
#' toy_backend <- function(model, data, scenario = NULL, ...) {
#'   list(weights = data.frame(construct = "Image", indicator = "img1",
#'                             estimate = mean(data$img1)))
#' }
#' fit_perturbation_grid(model, dat, grid, toy_backend)
#' @export
fit_perturbation_grid <- function(model, data, grid, backend,
                                  continue_on_error = TRUE,
                                  fit_baseline = TRUE, ...) {
  validate_model_spec(model, data = data)
  if (!inherits(grid, "stresspls_perturbation_grid")) {
    stop("`grid` must be a stresspls_perturbation_grid object.", call. = FALSE)
  }
  backend <- as_backend(backend)
  validate_backend(backend)

  baseline <- NULL
  if (isTRUE(fit_baseline)) {
    baseline <- fit_baseline_model(model, data, backend, ...)
  }

  scenarios <- grid$scenarios
  fits <- vector("list", nrow(scenarios))
  for (i in seq_len(nrow(scenarios))) {
    scenario <- scenarios[i, , drop = FALSE]
    scenario_data <- scenario$data[[1]]
    if (is.null(scenario_data)) {
      scenario_data <- data
    }
    if (!isTRUE(scenario$valid[[1]])) {
      fits[[i]] <- error_fit(
        model = model,
        data = scenario_data,
        scenario = scenario,
        backend = backend,
        error = scenario$reason_if_invalid[[1]]
      )
      next
    }

    fits[[i]] <- tryCatch(
      {
        output <- backend$fun(
          model = model,
          data = scenario_data,
          scenario = scenario,
          ...
        )
        as_stresspls_fit(
          output,
          model = model,
          data = scenario_data,
          scenario = scenario,
          backend = backend
        )
      },
      error = function(e) {
        if (!isTRUE(continue_on_error)) {
          stop(e)
        }
        error_fit(
          model = model,
          data = scenario_data,
          scenario = scenario,
          backend = backend,
          error = conditionMessage(e)
        )
      }
    )
  }

  structure(
    list(
      model = model,
      data = data,
      grid = grid,
      baseline = baseline,
      fits = fits,
      backend = backend,
      continue_on_error = continue_on_error,
      scenario_index = fit_grid_index(scenarios, fits)
    ),
    class = "stresspls_fit_grid"
  )
}

#' Create a standardized stressPLS fit
#'
#' @param x Backend output or existing `stresspls_fit`.
#' @param model Optional model specification.
#' @param data Optional data used for fitting.
#' @param scenario Optional scenario metadata.
#' @param backend Optional backend object.
#' @param metadata Additional metadata.
#'
#' @return A `stresspls_fit` object.
#' @examples
#' as_stresspls_fit(list(paths = data.frame(from = "A", to = "B",
#'                                          estimate = 0.2)))
#' @export
as_stresspls_fit <- function(x, model = NULL, data = NULL, scenario = NULL,
                             backend = NULL, metadata = list()) {
  if (inherits(x, "stresspls_fit")) {
    return(x)
  }
  output <- standardize_backend_output(x)
  output$model <- model
  output$data <- data
  output$scenario <- scenario
  output$backend <- backend
  output$metadata <- utils::modifyList(output$metadata, metadata)
  structure(output, class = "stresspls_fit")
}

#' Standardize backend output
#'
#' @param x Backend output list or `stresspls_fit`.
#'
#' @return A list with canonical output tables.
#' @examples
#' standardize_backend_output(list(paths = data.frame(from = "A", to = "B",
#'                                                    estimate = 0.2)))
#' @export
standardize_backend_output <- function(x) {
  if (inherits(x, "stresspls_fit")) {
    x <- unclass(x)
  }
  if (!is.list(x)) {
    stop("Backend output must be a list or stresspls_fit object.",
         call. = FALSE)
  }
  diagnostics <- x[["diagnostics", exact = TRUE]]
  if (is.null(diagnostics)) {
    diagnostics <- list(converged = TRUE, warnings = character(),
                        errors = character())
  }
  diagnostics$converged <- isTRUE(diagnostics$converged)
  diagnostics$warnings <- as.character(diagnostics$warnings %||% character())
  diagnostics$errors <- as.character(diagnostics$errors %||% character())

  list(
    paths = standardize_table(x[["paths", exact = TRUE]], canonical_paths()),
    weights = standardize_table(x[["weights", exact = TRUE]], canonical_weights()),
    vifs = standardize_table(x[["vifs", exact = TRUE]], canonical_vifs()),
    r2 = standardize_table(x[["r2", exact = TRUE]], canonical_r2()),
    prediction = standardize_table(x[["prediction", exact = TRUE]],
                                   canonical_prediction()),
    predictions = standardize_table(x[["predictions", exact = TRUE]],
                                    canonical_predictions()),
    diagnostics = diagnostics,
    metadata = x[["metadata", exact = TRUE]] %||% list()
  )
}

#' Extract structural path estimates
#'
#' @param x A `stresspls_fit` or `stresspls_fit_grid`.
#'
#' @return A data frame of path estimates.
#' @examples
#' fit <- as_stresspls_fit(list(paths = data.frame(from = "A", to = "B",
#'                                                 estimate = 0.2)))
#' extract_paths(fit)
#' @export
extract_paths <- function(x) {
  extract_fit_table(x, "paths")
}

#' Extract formative weight estimates
#'
#' @param x A `stresspls_fit` or `stresspls_fit_grid`.
#'
#' @return A data frame of weight estimates.
#' @examples
#' fit <- as_stresspls_fit(list(weights = data.frame(construct = "A",
#'                                                   indicator = "a1",
#'                                                   estimate = 0.2)))
#' extract_weights(fit)
#' @export
extract_weights <- function(x) {
  extract_fit_table(x, "weights")
}

#' Extract VIF diagnostics
#'
#' @param x A `stresspls_fit` or `stresspls_fit_grid`.
#'
#' @return A data frame of VIF diagnostics.
#' @examples
#' fit <- as_stresspls_fit(list(vifs = data.frame(construct = "A",
#'                                                indicator = "a1", vif = 1)))
#' extract_vifs(fit)
#' @export
extract_vifs <- function(x) {
  extract_fit_table(x, "vifs")
}

#' Extract R-squared values
#'
#' @param x A `stresspls_fit` or `stresspls_fit_grid`.
#'
#' @return A data frame of R-squared values.
#' @examples
#' fit <- as_stresspls_fit(list(r2 = data.frame(construct = "Y", r2 = 0.4)))
#' extract_r2(fit)
#' @export
extract_r2 <- function(x) {
  extract_fit_table(x, "r2")
}

#' Extract prediction metrics
#'
#' @param x A `stresspls_fit` or `stresspls_fit_grid`.
#'
#' @return A data frame of prediction metrics.
#' @examples
#' fit <- as_stresspls_fit(list(prediction = data.frame(outcome = "Y",
#'                                                      metric = "rmse",
#'                                                      value = 1)))
#' extract_prediction_metrics(fit)
#' @export
extract_prediction_metrics <- function(x) {
  extract_fit_table(x, "prediction")
}

canonical_paths <- function() {
  data.frame(
    from = character(), to = character(), estimate = numeric(),
    std_error = numeric(), statistic = numeric(), p_value = numeric(),
    ci_low = numeric(), ci_high = numeric(), significant = logical(),
    stringsAsFactors = FALSE
  )
}

canonical_weights <- function() {
  data.frame(
    construct = character(), indicator = character(), estimate = numeric(),
    std_error = numeric(), statistic = numeric(), p_value = numeric(),
    ci_low = numeric(), ci_high = numeric(), significant = logical(),
    stringsAsFactors = FALSE
  )
}

canonical_vifs <- function() {
  data.frame(
    construct = character(), indicator = character(), vif = numeric(),
    stringsAsFactors = FALSE
  )
}

canonical_r2 <- function() {
  data.frame(construct = character(), r2 = numeric(), stringsAsFactors = FALSE)
}

canonical_prediction <- function() {
  data.frame(
    outcome = character(), metric = character(), value = numeric(),
    stringsAsFactors = FALSE
  )
}

canonical_predictions <- function() {
  data.frame(
    outcome = character(), observed = numeric(), predicted = numeric(),
    stringsAsFactors = FALSE
  )
}

standardize_table <- function(x, template) {
  if (is.null(x)) {
    return(template)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (name in names(template)) {
    if (!name %in% names(x)) {
      x[[name]] <- template[[name]][NA_integer_]
    }
  }
  x <- x[, names(template), drop = FALSE]
  for (name in names(template)) {
    if (is.numeric(template[[name]])) {
      x[[name]] <- as.numeric(x[[name]])
    } else if (is.logical(template[[name]])) {
      x[[name]] <- as.logical(x[[name]])
    } else {
      x[[name]] <- as.character(x[[name]])
    }
  }
  x
}

extract_fit_table <- function(x, table) {
  if (inherits(x, "stresspls_fit")) {
    out <- x[[table]]
    if (is.null(out)) {
      out <- standardize_backend_output(list())[[table]]
    }
    scenario_id <- scenario_id_from_fit(x)
    if (!is.null(scenario_id) && nrow(out) > 0L) {
      out$scenario_id <- scenario_id
    }
    return(out)
  }
  if (inherits(x, "stresspls_fit_grid")) {
    pieces <- lapply(x$fits, extract_fit_table, table = table)
    out <- rbind_list(pieces)
    return(out)
  }
  stop("`x` must be a stresspls_fit or stresspls_fit_grid object.",
       call. = FALSE)
}

scenario_id_from_fit <- function(fit) {
  if (is.null(fit$scenario)) {
    return("baseline")
  }
  fit$scenario$scenario_id[[1]] %||% "scenario"
}

fit_grid_index <- function(scenarios, fits) {
  data.frame(
    scenario_id = scenarios$scenario_id,
    method = scenarios$method,
    construct = scenarios$construct,
    converged = vapply(fits, function(fit) isTRUE(fit$diagnostics$converged),
                       logical(1)),
    error = vapply(fits, function(fit) {
      paste(fit$diagnostics$errors, collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

error_fit <- function(model, data, scenario, backend, error) {
  as_stresspls_fit(
    list(
      diagnostics = list(
        converged = FALSE,
        warnings = character(),
        errors = as.character(error)
      )
    ),
    model = model,
    data = data,
    scenario = scenario,
    backend = backend
  )
}

rbind_list <- function(x) {
  x <- x[vapply(x, is.data.frame, logical(1))]
  if (length(x) == 0L) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(df) {
    missing <- setdiff(columns, names(df))
    for (column in missing) {
      df[[column]] <- rep(NA, nrow(df))
    }
    df[, columns, drop = FALSE]
  })
  out <- do.call(rbind, x)
  row.names(out) <- NULL
  out
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
