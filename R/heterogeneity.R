#' Observed subgroup heterogeneity diagnostics
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame.
#' @param backend Backend function or object.
#' @param group Column name defining observed subgroups.
#' @param min_n Minimum subgroup sample size.
#' @param continue_on_error Store subgroup errors instead of stopping.
#' @param ... Extra backend arguments.
#'
#' @return A `stresspls_heterogeneity` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:8, img2 = 2:9, g = rep(c("a", "b"), each = 4))
#' toy <- function(model, data, scenario = NULL, ...) {
#'   list(weights = data.frame(construct = "Image", indicator = "img1",
#'                             estimate = mean(data$img1)))
#' }
#' subgroup_heterogeneity(model, dat, toy, group = "g", min_n = 2)
#' @export
subgroup_heterogeneity <- function(model, data, backend, group, min_n = 30,
                                   continue_on_error = TRUE, ...) {
  validate_model_spec(model, data = data)
  backend <- as_backend(backend)
  group <- validate_single_string(group, "group")
  if (!group %in% names(data)) {
    stop("`group` must be a column in `data`.", call. = FALSE)
  }
  min_n <- validate_positive_whole_number(min_n, "min_n")
  groups <- split(seq_len(nrow(data)), data[[group]])
  small <- names(groups)[vapply(groups, length, integer(1)) < min_n]
  if (length(small) > 0L) {
    stop("Subgroups below `min_n`: ", paste(small, collapse = ", "),
         call. = FALSE)
  }
  fits <- vector("list", length(groups))
  names(fits) <- names(groups)
  index <- data.frame(
    group = names(groups),
    n = vapply(groups, length, integer(1)),
    converged = FALSE,
    error = "",
    stringsAsFactors = FALSE
  )
  for (i in seq_along(groups)) {
    scenario <- data.frame(
      scenario_id = paste0("subgroup_", names(groups)[[i]]),
      method = "subgroup",
      construct = "",
      group = names(groups)[[i]],
      stringsAsFactors = FALSE
    )
    subgroup_data <- data[groups[[i]], , drop = FALSE]
    fits[[i]] <- tryCatch(
      as_stresspls_fit(
        backend$fun(model = model, data = subgroup_data,
                    scenario = scenario, ...),
        model = model, data = subgroup_data, scenario = scenario,
        backend = backend
      ),
      error = function(e) {
        if (!continue_on_error) stop(e)
        error_fit(model, subgroup_data, scenario, backend, conditionMessage(e))
      }
    )
    index$converged[[i]] <- isTRUE(fits[[i]]$diagnostics$converged)
    index$error[[i]] <- paste(fits[[i]]$diagnostics$errors, collapse = "; ")
  }
  structure(
    list(model = model, data = data, group = group, fits = fits,
         index = index, backend = backend),
    class = "stresspls_heterogeneity"
  )
}

#' Compare subgroup path estimates
#'
#' @param x A `stresspls_heterogeneity` object.
#'
#' @return A tidy comparison table.
#' @examples
#' # See `subgroup_heterogeneity()` for construction.
#' @export
compare_subgroup_paths <- function(x) {
  compare_subgroup_table(x, "paths", c("from", "to"))
}

#' Compare subgroup weight estimates
#'
#' @param x A `stresspls_heterogeneity` object.
#'
#' @return A tidy comparison table.
#' @examples
#' # See `subgroup_heterogeneity()` for construction.
#' @export
compare_subgroup_weights <- function(x) {
  compare_subgroup_table(x, "weights", c("construct", "indicator"))
}

compare_subgroup_table <- function(x, table, keys) {
  if (!inherits(x, "stresspls_heterogeneity")) {
    stop("`x` must be a stresspls_heterogeneity object.", call. = FALSE)
  }
  rows <- list()
  for (group in names(x$fits)) {
    values <- x$fits[[group]][[table]]
    if (nrow(values) == 0L) next
    values$group <- group
    rows[[length(rows) + 1L]] <- values
  }
  estimates <- rbind_list(rows)
  if (nrow(estimates) == 0L) return(data.frame())
  out <- lapply(split(estimates, key_interaction(estimates, keys)),
                function(rows) {
    data.frame(
      rows[1, keys, drop = FALSE],
      min_estimate = min(rows$estimate, na.rm = TRUE),
      max_estimate = max(rows$estimate, na.rm = TRUE),
      range = diff(range(rows$estimate, na.rm = TRUE)),
      groups = paste(rows$group, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  rbind_list(out)
}
