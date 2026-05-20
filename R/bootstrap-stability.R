#' Bootstrap stability diagnostics
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame.
#' @param backend Backend function or `stresspls_backend`.
#' @param R Number of bootstrap replications.
#' @param seed Optional random seed.
#' @param strata Optional column name for stratified resampling.
#' @param continue_on_error Store failed fits instead of stopping.
#' @param ... Extra arguments passed to the backend.
#'
#' @return A `stresspls_bootstrap` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:6, img2 = 2:7)
#' toy <- function(model, data, scenario = NULL, ...) {
#'   list(weights = data.frame(construct = "Image", indicator = "img1",
#'                             estimate = mean(data$img1)))
#' }
#' bootstrap_stability(model, dat, toy, R = 2, seed = 1)
#' @export
bootstrap_stability <- function(model, data, backend, R = 500, seed = NULL,
                                strata = NULL, continue_on_error = TRUE, ...) {
  validate_model_spec(model, data = data)
  backend <- as_backend(backend)
  R <- validate_positive_whole_number(R, "R")
  validate_seed(seed)
  if (!is.null(strata) && (!is.character(strata) || length(strata) != 1L ||
                           !strata %in% names(data))) {
    stop("`strata` must be a single column name in `data` or `NULL`.",
         call. = FALSE)
  }
  baseline <- fit_baseline_model(model, data, backend, ...)
  seeds <- with_seed(seed, sample.int(.Machine$integer.max, R))
  fits <- vector("list", R)
  index <- data.frame(
    replicate = seq_len(R),
    seed = seeds,
    converged = FALSE,
    error = "",
    stringsAsFactors = FALSE
  )
  for (i in seq_len(R)) {
    rows <- bootstrap_indices(data, seed = seeds[[i]], strata = strata)
    boot_data <- data[rows, , drop = FALSE]
    scenario <- data.frame(
      scenario_id = paste0("bootstrap_", i),
      method = "bootstrap",
      construct = "",
      stringsAsFactors = FALSE
    )
    fits[[i]] <- tryCatch(
      as_stresspls_fit(
        backend$fun(model = model, data = boot_data, scenario = scenario, ...),
        model = model,
        data = boot_data,
        scenario = scenario,
        backend = backend
      ),
      error = function(e) {
        if (!continue_on_error) {
          stop(e)
        }
        error_fit(model, boot_data, scenario, backend, conditionMessage(e))
      }
    )
    index$converged[[i]] <- isTRUE(fits[[i]]$diagnostics$converged)
    index$error[[i]] <- paste(fits[[i]]$diagnostics$errors, collapse = "; ")
  }
  structure(
    list(
      model = model,
      data = data,
      baseline = baseline,
      fits = fits,
      index = index,
      backend = backend,
      seed = seed,
      strata = strata
    ),
    class = "stresspls_bootstrap"
  )
}

#' Summarise bootstrap stability diagnostics
#'
#' @param x A `stresspls_bootstrap` object.
#'
#' @return A tidy data frame of bootstrap stability metrics.
#' @examples
#' # See `bootstrap_stability()` for construction.
#' @export
summarise_bootstrap_stability <- function(x) {
  if (!inherits(x, "stresspls_bootstrap")) {
    stop("`x` must be a stresspls_bootstrap object.", call. = FALSE)
  }
  grid <- structure(
    list(
      fits = x$fits,
      baseline = x$baseline,
      scenario_index = data.frame(
        scenario_id = x$index$replicate,
        converged = x$index$converged
      )
    ),
    class = "stresspls_fit_grid"
  )
  path_sign <- calc_sign_consistency(grid, type = "paths")
  if (nrow(path_sign) > 0L) {
    path_sign$type <- "path"
    path_sign$metric <- "sign_consistency"
  }
  weight_sign <- calc_sign_consistency(grid, type = "weights")
  if (nrow(weight_sign) > 0L) {
    weight_sign$type <- "weight"
    weight_sign$metric <- "sign_consistency"
  }
  path_ci <- calc_ci_width(grid, type = "paths")
  if (nrow(path_ci) > 0L) {
    path_ci$type <- "path"
    path_ci$metric <- "ci_width"
  }
  weight_ci <- calc_ci_width(grid, type = "weights")
  if (nrow(weight_ci) > 0L) {
    weight_ci$type <- "weight"
    weight_ci$metric <- "ci_width"
  }
  out <- rbind_list(list(path_sign, weight_sign, path_ci, weight_ci))
  diagnostics <- data.frame(
    type = "diagnostics",
    metric = c("convergence_rate", "error_rate"),
    value = c(mean(x$index$converged), mean(!x$index$converged)),
    stringsAsFactors = FALSE
  )
  rbind_list(list(out, diagnostics))
}

bootstrap_indices <- function(data, seed, strata) {
  with_seed(seed, {
    if (is.null(strata)) {
      sample.int(nrow(data), nrow(data), replace = TRUE)
    } else {
      split_rows <- split(seq_len(nrow(data)), data[[strata]])
      unlist(lapply(split_rows, function(idx) {
        sample(idx, length(idx), replace = TRUE)
      }), use.names = FALSE)
    }
  })
}
