#' Generate alternative model specification scenarios
#'
#' `stress_specifications()` creates a grid from named alternative model
#' specifications. The objects are stored as list-column entries so that a
#' backend can estimate them later.
#'
#' @param specifications A non-empty list of alternative model specifications.
#' @param seed Optional random seed. Included for reproducibility as randomized
#'   specification perturbations are added.
#'
#' @return A `stresspls_grid` object.
#' @examples
#' stress_specifications(list(repeated = list(type = "repeated")))
#' @export
stress_specifications <- function(specifications, seed = NULL) {
  if (!is.list(specifications) || length(specifications) == 0L) {
    stop("`specifications` must be a non-empty list.", call. = FALSE)
  }
  validate_seed(seed)

  spec_names <- names(specifications)
  if (is.null(spec_names) || any(spec_names == "")) {
    spec_names <- paste0("specification_", seq_along(specifications))
  }

  scenarios <- data.frame(
    scenario_id = paste0("specification_", seq_along(specifications)),
    perturbation = "alternative_specification",
    specification_name = spec_names,
    stringsAsFactors = FALSE
  )
  scenarios$specification <- I(unname(specifications))

  new_stresspls_grid(
    scenarios = scenarios,
    call = match.call(),
    metadata = list(type = "specifications", seed = seed)
  )
}
