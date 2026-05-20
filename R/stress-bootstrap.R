#' Generate bootstrap stress-test scenarios
#'
#' `stress_bootstrap()` creates a seeded bootstrap scenario grid. It records the
#' bootstrap replication seeds and leaves estimation to `stress_pls()`.
#'
#' @param data A data frame containing the analysis data.
#' @param model A list describing the model specification.
#' @param R Number of bootstrap replications.
#' @param seed Optional random seed.
#'
#' @return A `stresspls_grid` object.
#' @examples
#' dat <- data.frame(x1 = 1:5, x2 = 2:6, y = 3:7)
#' model <- list(indicators = c("x1", "x2"))
#' stress_bootstrap(dat, model, R = 2, seed = 1)
#' @export
stress_bootstrap <- function(data, model, R = 100L, seed = NULL) {
  validate_data(data)
  validate_model(model)
  validate_seed(seed)

  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R < 1L ||
      R != as.integer(R)) {
    stop("`R` must be a positive whole number.", call. = FALSE)
  }

  scenario_seeds <- with_seed(seed, {
    sample.int(.Machine$integer.max, size = as.integer(R))
  })
  scenarios <- data.frame(
    scenario_id = paste0("bootstrap_", seq_len(as.integer(R))),
    perturbation = "bootstrap_resample",
    bootstrap_id = seq_len(as.integer(R)),
    seed = scenario_seeds,
    stringsAsFactors = FALSE
  )

  new_stresspls_grid(
    scenarios = scenarios,
    call = match.call(),
    metadata = list(type = "bootstrap", seed = seed, R = as.integer(R))
  )
}
