#' Create a stressPLS perturbation grid
#'
#' @param scenarios A data frame with one row per generated perturbation.
#' @param call The matched call that generated the grid.
#' @param metadata A named list of metadata about the perturbation generator.
#'
#' @return An object of class `stresspls_grid`.
#' @keywords internal
new_stresspls_grid <- function(scenarios, call = NULL, metadata = list()) {
  if (!is.data.frame(scenarios)) {
    stop("`scenarios` must be a data frame.", call. = FALSE)
  }
  if (!is.list(metadata) || is.null(names(metadata))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }

  structure(
    list(
      scenarios = scenarios,
      call = call,
      metadata = metadata
    ),
    class = "stresspls_grid"
  )
}

#' Create a stressPLS result object
#'
#' @param data Original analysis data.
#' @param model Model specification.
#' @param grid A `stresspls_grid` object.
#' @param results A data frame of estimation results.
#' @param call The matched call.
#' @param backend Name of the backend or `NULL`.
#'
#' @return An object of class `stresspls_result`.
#' @keywords internal
new_stresspls_result <- function(data, model, grid, results, call = NULL,
                                 backend = NULL) {
  if (!inherits(grid, "stresspls_grid")) {
    stop("`grid` must be a stresspls_grid object.", call. = FALSE)
  }
  if (!is.data.frame(results)) {
    stop("`results` must be a data frame.", call. = FALSE)
  }

  structure(
    list(
      data = data,
      model = model,
      grid = grid,
      results = results,
      call = call,
      backend = backend
    ),
    class = "stresspls_result"
  )
}

#' Create a stressPLS summary object
#'
#' @param result A `stresspls_result` object.
#' @param status_counts A data frame of status counts.
#'
#' @return An object of class `stresspls_summary`.
#' @keywords internal
new_stresspls_summary <- function(result, status_counts) {
  if (!inherits(result, "stresspls_result")) {
    stop("`result` must be a stresspls_result object.", call. = FALSE)
  }
  if (!is.data.frame(status_counts)) {
    stop("`status_counts` must be a data frame.", call. = FALSE)
  }

  structure(
    list(
      result = result,
      status_counts = status_counts
    ),
    class = "stresspls_summary"
  )
}
