#' Generate indicator perturbations
#'
#' `stress_indicators()` creates an initial perturbation grid for indicator
#' deletion or swapping scenarios. It does not estimate a PLS-SEM model.
#'
#' @param data A data frame containing the analysis data.
#' @param model A list describing the model specification.
#' @param indicators Character vector of indicator names. If `NULL`, the
#'   function looks for `model$indicators`.
#' @param swaps Optional two-column data frame or matrix defining indicator
#'   swaps. The first column is the original indicator and the second column is
#'   the replacement indicator.
#' @param seed Optional random seed. Included for reproducibility as perturbation
#'   generation grows.
#'
#' @return A `stresspls_grid` object.
#' @examples
#' dat <- data.frame(x1 = 1:3, x2 = 2:4, y = 3:5)
#' model <- list(indicators = c("x1", "x2"))
#' stress_indicators(dat, model)
#' @export
stress_indicators <- function(data, model, indicators = NULL, swaps = NULL,
                              seed = NULL) {
  validate_data(data)
  validate_model(model)
  validate_seed(seed)

  if (is.null(indicators)) {
    indicators <- model$indicators
  }
  indicators <- as_non_empty_character(indicators, "indicators")

  missing_indicators <- setdiff(indicators, names(data))
  if (length(missing_indicators) > 0L) {
    stop(
      "`indicators` must all be columns in `data`; missing: ",
      paste(missing_indicators, collapse = ", "),
      call. = FALSE
    )
  }

  scenarios <- data.frame(
    scenario_id = paste0("indicator_drop_", seq_along(indicators)),
    perturbation = "indicator_deletion",
    indicator = indicators,
    replacement = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!is.null(swaps)) {
    swaps <- as.data.frame(swaps, stringsAsFactors = FALSE)
    if (ncol(swaps) != 2L || nrow(swaps) == 0L) {
      stop("`swaps` must have two columns and at least one row.",
           call. = FALSE)
    }
    names(swaps) <- c("indicator", "replacement")
    swap_values <- unlist(swaps, use.names = FALSE)
    missing_swaps <- setdiff(swap_values, names(data))
    if (length(missing_swaps) > 0L) {
      stop(
        "`swaps` must reference columns in `data`; missing: ",
        paste(missing_swaps, collapse = ", "),
        call. = FALSE
      )
    }

    swap_scenarios <- data.frame(
      scenario_id = paste0("indicator_swap_", seq_len(nrow(swaps))),
      perturbation = "indicator_swap",
      indicator = swaps$indicator,
      replacement = swaps$replacement,
      stringsAsFactors = FALSE
    )
    scenarios <- rbind(scenarios, swap_scenarios)
  }

  new_stresspls_grid(
    scenarios = scenarios,
    call = match.call(),
    metadata = list(type = "indicators", seed = seed)
  )
}
