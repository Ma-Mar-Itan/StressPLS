#' Run a stressPLS analysis
#'
#' `stress_pls()` combines data, a model specification, a perturbation grid, and
#' an optional estimation backend. The scaffold intentionally does not implement
#' a PLS-SEM estimator. If `backend = NULL`, scenarios are returned with status
#' `"not_estimated"` rather than fabricated statistical results.
#'
#' @param data A data frame containing the analysis data.
#' @param model A list describing the model specification.
#' @param grid Optional `stresspls_grid`. If omitted, an empty base grid is used.
#' @param backend Optional function with arguments `data`, `model`, `scenario`,
#'   and `seed`. It must return a data frame.
#' @param seed Optional random seed passed to backend calls.
#'
#' @return A `stresspls_result` object.
#' @examples
#' dat <- data.frame(x1 = 1:4, x2 = 2:5, y = 3:6)
#' model <- list(indicators = c("x1", "x2"))
#' grid <- stress_indicators(dat, model)
#' stress_pls(dat, model, grid = grid)
#' @export
stress_pls <- function(data, model, grid = NULL, backend = NULL, seed = NULL) {
  validate_data(data)
  validate_model(model)
  validate_seed(seed)

  if (is.null(grid)) {
    grid <- new_stresspls_grid(
      scenarios = data.frame(
        scenario_id = "baseline",
        perturbation = "baseline",
        stringsAsFactors = FALSE
      ),
      call = match.call(),
      metadata = list(type = "baseline", seed = seed)
    )
  } else {
    validate_grid(grid)
  }

  scenarios <- grid$scenarios
  if (is.null(backend)) {
    results <- data.frame(
      scenario_id = scenarios$scenario_id,
      perturbation = scenarios$perturbation,
      status = "not_estimated",
      stringsAsFactors = FALSE
    )
  } else {
    if (!is.function(backend)) {
      stop("`backend` must be a function or `NULL`.", call. = FALSE)
    }
    results <- run_backend(data, model, scenarios, backend, seed)
  }

  new_stresspls_result(
    data = data,
    model = model,
    grid = grid,
    results = results,
    call = match.call(),
    backend = if (is.null(backend)) NULL else "custom"
  )
}

run_backend <- function(data, model, scenarios, backend, seed) {
  pieces <- vector("list", nrow(scenarios))
  for (i in seq_len(nrow(scenarios))) {
    scenario <- scenarios[i, , drop = FALSE]
    scenario_seed <- if (is.null(seed)) NULL else seed + i - 1L
    value <- backend(
      data = data,
      model = model,
      scenario = scenario,
      seed = scenario_seed
    )
    if (!is.data.frame(value)) {
      stop("`backend` must return a data frame for every scenario.",
           call. = FALSE)
    }
    if (!"scenario_id" %in% names(value)) {
      value$scenario_id <- scenario$scenario_id
    }
    if (!"status" %in% names(value)) {
      value$status <- "estimated"
    }
    pieces[[i]] <- value
  }
  results <- do.call(rbind, pieces)
  row.names(results) <- NULL
  results
}
