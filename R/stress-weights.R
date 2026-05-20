#' Generate construct weight perturbations
#'
#' `stress_weights()` creates a perturbation grid for alternative formative
#' indicator weights. The initial scaffold supports equal weights and seeded
#' jitter around supplied weights.
#'
#' @param weights Named numeric vector of baseline weights.
#' @param schemes Character vector of schemes to generate. Supported values are
#'   `"equal"` and `"jitter"`.
#' @param jitter_size Maximum absolute jitter applied to each baseline weight
#'   when `"jitter"` is requested.
#' @param seed Optional random seed.
#'
#' @return A `stresspls_grid` object.
#' @examples
#' stress_weights(c(x1 = 0.4, x2 = 0.6), seed = 1)
#' @export
stress_weights <- function(weights, schemes = c("equal", "jitter"),
                           jitter_size = 0.05, seed = NULL) {
  validate_weights(weights)
  schemes <- as_non_empty_character(schemes, "schemes")
  validate_seed(seed)

  unsupported <- setdiff(schemes, c("equal", "jitter"))
  if (length(unsupported) > 0L) {
    stop("Unsupported `schemes`: ", paste(unsupported, collapse = ", "),
         call. = FALSE)
  }
  if (!is.numeric(jitter_size) || length(jitter_size) != 1L ||
      is.na(jitter_size) || jitter_size < 0) {
    stop("`jitter_size` must be a single non-negative number.",
         call. = FALSE)
  }

  scenarios <- with_seed(seed, {
    pieces <- list()
    if ("equal" %in% schemes) {
      equal_weights <- rep(1 / length(weights), length(weights))
      pieces[[length(pieces) + 1L]] <- data.frame(
        scenario_id = "weights_equal",
        perturbation = "construct_reweighting",
        scheme = "equal",
        weight_name = names(weights),
        weight = equal_weights,
        stringsAsFactors = FALSE
      )
    }
    if ("jitter" %in% schemes) {
      jittered <- weights + stats::runif(
        length(weights),
        min = -jitter_size,
        max = jitter_size
      )
      pieces[[length(pieces) + 1L]] <- data.frame(
        scenario_id = "weights_jitter",
        perturbation = "construct_reweighting",
        scheme = "jitter",
        weight_name = names(weights),
        weight = jittered,
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, pieces)
  })

  row.names(scenarios) <- NULL
  new_stresspls_grid(
    scenarios = scenarios,
    call = match.call(),
    metadata = list(type = "weights", seed = seed)
  )
}
