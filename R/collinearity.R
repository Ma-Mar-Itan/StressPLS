#' Inflate collinearity among selected indicators
#'
#' This function approximates collinearity pressure by blending selected numeric
#' indicators with a shared standardized component. Exact target correlations
#' are not guaranteed for arbitrary data.
#'
#' @param data Data frame.
#' @param indicators Character vector of numeric indicator columns.
#' @param target_correlation Optional target pairwise correlation.
#' @param strength Optional blend strength in `[0, 1)`.
#'
#' @return A `stresspls_collinearity_recipe` with transformed data and recipe.
#' @examples
#' dat <- data.frame(x1 = 1:5, x2 = 2:6)
#' inflate_collinearity(dat, c("x1", "x2"), strength = 0.5)
#' @export
inflate_collinearity <- function(data, indicators, target_correlation = NULL,
                                 strength = NULL) {
  validate_data(data)
  indicators <- validate_name_vector(indicators, "indicators")
  missing <- setdiff(indicators, names(data))
  if (length(missing) > 0L) {
    stop("`data` is missing indicators: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (length(indicators) < 2L) {
    stop("At least two `indicators` are required.", call. = FALSE)
  }
  if (!all(vapply(data[indicators], is.numeric, logical(1)))) {
    stop("All selected `indicators` must be numeric.", call. = FALSE)
  }
  if (is.null(strength)) {
    strength <- if (is.null(target_correlation)) 0.5 else target_correlation
  }
  if (!is.numeric(strength) || length(strength) != 1L || is.na(strength) ||
      strength < 0 || strength >= 1) {
    stop("`strength` must be a single number in [0, 1).", call. = FALSE)
  }
  out <- data
  z <- scale(data[indicators])
  common <- rowMeans(z)
  common <- as.numeric(scale(common))
  for (indicator in indicators) {
    original_mean <- mean(data[[indicator]], na.rm = TRUE)
    original_sd <- stats::sd(data[[indicator]], na.rm = TRUE)
    blended <- sqrt(strength) * common + sqrt(1 - strength) *
      as.numeric(scale(data[[indicator]]))
    out[[indicator]] <- original_mean + original_sd * as.numeric(scale(blended))
  }
  structure(
    list(
      data = out,
      recipe = list(
        indicators = indicators,
        target_correlation = target_correlation,
        strength = strength,
        approximation = TRUE
      )
    ),
    class = "stresspls_collinearity_recipe"
  )
}

#' Run a collinearity stress test
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame.
#' @param backend Backend function or object.
#' @param indicators Numeric indicators to perturb.
#' @param levels Collinearity strengths in `[0, 1)`.
#' @param continue_on_error Store scenario errors instead of stopping.
#' @param ... Extra backend arguments.
#'
#' @return A `stresspls_collinearity_stress` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:6, img2 = 2:7)
#' toy <- function(model, data, scenario = NULL, ...) {
#'   list(vifs = data.frame(construct = "Image", indicator = "img1", vif = 1))
#' }
#' collinearity_stress_test(model, dat, toy, c("img1", "img2"), levels = c(0, .2))
#' @export
collinearity_stress_test <- function(model, data, backend, indicators,
                                     levels = c(0, 0.25, 0.5, 0.75),
                                     continue_on_error = TRUE, ...) {
  validate_model_spec(model, data = data)
  backend <- as_backend(backend)
  if (!is.numeric(levels) || anyNA(levels) || any(levels < 0 | levels >= 1)) {
    stop("`levels` must be numeric values in [0, 1).", call. = FALSE)
  }
  baseline <- fit_baseline_model(model, data, backend, ...)
  fits <- vector("list", length(levels))
  recipes <- vector("list", length(levels))
  for (i in seq_along(levels)) {
    transformed <- if (levels[[i]] == 0) {
      structure(list(data = data, recipe = list(strength = 0)),
                class = "stresspls_collinearity_recipe")
    } else {
      inflate_collinearity(data, indicators, strength = levels[[i]])
    }
    recipes[[i]] <- transformed$recipe
    scenario <- data.frame(
      scenario_id = paste0("collinearity_", i),
      method = "collinearity",
      construct = "",
      level = levels[[i]],
      stringsAsFactors = FALSE
    )
    fits[[i]] <- tryCatch(
      as_stresspls_fit(
        backend$fun(model = model, data = transformed$data,
                    scenario = scenario, ...),
        model = model,
        data = transformed$data,
        scenario = scenario,
        backend = backend
      ),
      error = function(e) {
        if (!continue_on_error) stop(e)
        error_fit(model, transformed$data, scenario, backend,
                  conditionMessage(e))
      }
    )
  }
  index <- data.frame(
    level = levels,
    scenario_id = paste0("collinearity_", seq_along(levels)),
    converged = vapply(fits, function(fit) isTRUE(fit$diagnostics$converged),
                       logical(1)),
    stringsAsFactors = FALSE
  )
  structure(
    list(model = model, data = data, baseline = baseline, fits = fits,
         levels = levels, indicators = indicators, recipes = recipes,
         index = index, backend = backend),
    class = "stresspls_collinearity_stress"
  )
}

#' Summarise a collinearity stress test
#'
#' @param x A `stresspls_collinearity_stress` object.
#'
#' @return A tidy data frame.
#' @examples
#' # See `collinearity_stress_test()` for construction.
#' @export
summarise_collinearity_stress <- function(x) {
  if (!inherits(x, "stresspls_collinearity_stress")) {
    stop("`x` must be a stresspls_collinearity_stress object.", call. = FALSE)
  }
  grid <- structure(
    list(fits = x$fits, baseline = x$baseline, scenario_index = x$index),
    class = "stresspls_fit_grid"
  )
  vifs <- extract_vifs(grid)
  max_vif <- if (nrow(vifs) > 0L &&
                 all(c("vif", "scenario_id") %in% names(vifs))) {
    tapply(vifs$vif, vifs$scenario_id, max, na.rm = TRUE)
  } else {
    stats::setNames(numeric(), character())
  }
  path_stability <- calc_path_stability(grid)
  path_change <- if (nrow(path_stability) > 0L &&
                     all(c("difference", "scenario_id") %in%
                         names(path_stability))) {
    tapply(abs(path_stability$difference),
           path_stability$scenario_id, mean, na.rm = TRUE)
  } else {
    stats::setNames(numeric(), character())
  }
  out <- x$index
  out$max_vif <- unname(max_vif[out$scenario_id])
  out$mean_path_abs_change <- unname(path_change[out$scenario_id])
  out
}
