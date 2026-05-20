#' Repeated cross-validation prediction diagnostics
#'
#' @param model A `stresspls_model_spec`.
#' @param data Data frame.
#' @param backend Backend function or object.
#' @param outcomes Outcome columns to evaluate.
#' @param v Number of folds.
#' @param repeats Number of repetitions.
#' @param seed Optional random seed.
#' @param metrics Metrics to compute from predictions.
#' @param continue_on_error Store fold errors instead of stopping.
#' @param ... Extra backend arguments.
#'
#' @return A `stresspls_prediction_validation` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' dat <- data.frame(img1 = 1:8, img2 = 2:9, y = 3:10)
#' toy <- function(model, data, scenario = NULL, ...) {
#'   test <- scenario$test_data[[1]]
#'   list(predictions = data.frame(outcome = "y", observed = test$y,
#'                                 predicted = mean(data$y)))
#' }
#' repeated_cv_predict(model, dat, toy, outcomes = "y", v = 2, repeats = 1)
#' @export
repeated_cv_predict <- function(model, data, backend, outcomes, v = 5,
                                repeats = 1, seed = NULL,
                                metrics = c("rmse", "mae"),
                                continue_on_error = TRUE, ...) {
  validate_model_spec(model, data = data)
  backend <- as_backend(backend)
  outcomes <- validate_name_vector(outcomes, "outcomes")
  missing <- setdiff(outcomes, names(data))
  if (length(missing) > 0L) {
    stop("`data` is missing outcomes: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  v <- validate_positive_whole_number(v, "v")
  repeats <- validate_positive_whole_number(repeats, "repeats")
  if (v > nrow(data)) {
    stop("`v` must not exceed the number of rows in `data`.", call. = FALSE)
  }
  metrics <- match.arg(metrics, c("rmse", "mae"), several.ok = TRUE)
  fold_index <- make_cv_folds(nrow(data), v, repeats, seed)
  fits <- vector("list", nrow(fold_index))
  metric_rows <- list()
  for (i in seq_len(nrow(fold_index))) {
    test_idx <- fold_index$test_index[[i]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    scenario <- data.frame(
      scenario_id = paste0("cv_", fold_index$repeat_id[[i]], "_",
                           fold_index$fold[[i]]),
      method = "cross_validation",
      construct = "",
      repeat_id = fold_index$repeat_id[[i]],
      fold = fold_index$fold[[i]],
      stringsAsFactors = FALSE
    )
    scenario$test_data <- I(list(data[test_idx, , drop = FALSE]))
    fits[[i]] <- tryCatch(
      as_stresspls_fit(
        backend$fun(model = model, data = data[train_idx, , drop = FALSE],
                    scenario = scenario, ...),
        model = model,
        data = data[train_idx, , drop = FALSE],
        scenario = scenario,
        backend = backend
      ),
      error = function(e) {
        if (!continue_on_error) stop(e)
        error_fit(model, data[train_idx, , drop = FALSE], scenario, backend,
                  conditionMessage(e))
      }
    )
    fold_metrics <- fold_prediction_metrics(fits[[i]], outcomes, metrics)
    if (nrow(fold_metrics) == 0L && isTRUE(fits[[i]]$diagnostics$converged)) {
      msg <- "Backend did not return predictions or prediction metrics."
      if (!continue_on_error) {
        stop(msg, call. = FALSE)
      }
      fits[[i]]$diagnostics$converged <- FALSE
      fits[[i]]$diagnostics$errors <- c(fits[[i]]$diagnostics$errors, msg)
    }
    if (nrow(fold_metrics) > 0L) {
      fold_metrics$repeat_id <- fold_index$repeat_id[[i]]
      fold_metrics$fold <- fold_index$fold[[i]]
      metric_rows[[length(metric_rows) + 1L]] <- fold_metrics
    }
  }
  structure(
    list(model = model, data = data, fits = fits, fold_index = fold_index,
         metrics = rbind_list(metric_rows), backend = backend,
         outcomes = outcomes, seed = seed),
    class = "stresspls_prediction_validation"
  )
}

#' Compare prediction metrics
#'
#' @param x A `stresspls_prediction_validation` object or prediction metric data
#'   frame.
#'
#' @return A tidy summary table.
#' @examples
#' compare_prediction_metrics(data.frame(outcome = "y", metric = "rmse",
#'                                       value = c(1, 2)))
#' @export
compare_prediction_metrics <- function(x) {
  metrics <- if (inherits(x, "stresspls_prediction_validation")) x$metrics else x
  if (!is.data.frame(metrics) || !all(c("outcome", "metric", "value") %in%
                                      names(metrics))) {
    stop("`x` must contain `outcome`, `metric`, and `value` columns.",
         call. = FALSE)
  }
  if (nrow(metrics) == 0L) {
    return(data.frame())
  }
  split_key <- interaction(metrics$outcome, metrics$metric, drop = TRUE)
  out <- lapply(split(metrics, split_key), function(rows) {
    data.frame(
      outcome = rows$outcome[[1]],
      metric = rows$metric[[1]],
      mean = mean(rows$value, na.rm = TRUE),
      sd = stats::sd(rows$value, na.rm = TRUE),
      n = sum(!is.na(rows$value)),
      stringsAsFactors = FALSE
    )
  })
  rbind_list(out)
}

make_cv_folds <- function(n, v, repeats, seed) {
  seeds <- with_seed(seed, sample.int(.Machine$integer.max, repeats))
  rows <- list()
  for (r in seq_len(repeats)) {
    assignment <- with_seed(seeds[[r]], sample(rep(seq_len(v), length.out = n)))
    for (fold in seq_len(v)) {
      rows[[length(rows) + 1L]] <- list(
        repeat_id = r,
        fold = fold,
        test_index = which(assignment == fold)
      )
    }
  }
  data.frame(
    repeat_id = vapply(rows, `[[`, integer(1), "repeat_id"),
    fold = vapply(rows, `[[`, integer(1), "fold"),
    test_index = I(lapply(rows, `[[`, "test_index")),
    stringsAsFactors = FALSE
  )
}

fold_prediction_metrics <- function(fit, outcomes, metrics) {
  if (nrow(fit$prediction) > 0L) {
    return(fit$prediction[fit$prediction$outcome %in% outcomes &
                            fit$prediction$metric %in% metrics, ,
                          drop = FALSE])
  }
  preds <- fit$predictions
  if (nrow(preds) == 0L) {
    return(canonical_prediction())
  }
  rows <- list()
  for (outcome in outcomes) {
    p <- preds[preds$outcome == outcome, , drop = FALSE]
    if (nrow(p) == 0L) next
    err <- p$observed - p$predicted
    if ("rmse" %in% metrics) {
      rows[[length(rows) + 1L]] <- data.frame(
        outcome = outcome, metric = "rmse",
        value = sqrt(mean(err^2, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
    }
    if ("mae" %in% metrics) {
      rows[[length(rows) + 1L]] <- data.frame(
        outcome = outcome, metric = "mae",
        value = mean(abs(err), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  rbind_list(rows)
}
