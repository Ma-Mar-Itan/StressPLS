#' Create a backend skeleton for seminr
#'
#' This is a skeleton adapter, not a tested built-in `seminr` integration.
#' Supply `estimator` to perform estimation and return canonical stressPLS
#' backend output.
#'
#' @param estimator Optional function using `seminr`.
#'
#' @return A `stresspls_backend`.
#' @examples
#' backend_from_seminr()
#' @export
backend_from_seminr <- function(estimator = NULL) {
  optional_backend("seminr", estimator)
}

#' Create a backend skeleton for cSEM
#'
#' This is a skeleton adapter, not a tested built-in `cSEM` integration.
#' Supply `estimator` to perform estimation and return canonical stressPLS
#' backend output.
#'
#' @param estimator Optional function using `cSEM`.
#'
#' @return A `stresspls_backend`.
#' @examples
#' backend_from_csem()
#' @export
backend_from_csem <- function(estimator = NULL) {
  optional_backend("cSEM", estimator)
}

#' Create a backend skeleton for plspm
#'
#' This is a skeleton adapter, not a tested built-in `plspm` integration.
#' Supply `estimator` to perform estimation and return canonical stressPLS
#' backend output.
#'
#' @param estimator Optional function using `plspm`.
#'
#' @return A `stresspls_backend`.
#' @examples
#' backend_from_plspm()
#' @export
backend_from_plspm <- function(estimator = NULL) {
  optional_backend("plspm", estimator)
}

#' Create a backend skeleton for SmartPLS exports
#'
#' This is a skeleton adapter, not a tested built-in SmartPLS export parser.
#' Supply `parser` to read export artifacts and return canonical stressPLS
#' backend output.
#'
#' @param parser Optional function that reads SmartPLS export artifacts and
#'   returns canonical backend output.
#'
#' @return A `stresspls_backend`.
#' @examples
#' backend_from_smartpls_export()
#' @export
backend_from_smartpls_export <- function(parser = NULL) {
  fun <- function(model, data, scenario = NULL, ...) {
    if (is.null(parser)) {
      stop(
        "SmartPLS export support requires a `parser` that returns canonical ",
        "stressPLS backend output.",
        call. = FALSE
      )
    }
    parser(model = model, data = data, scenario = scenario, ...)
  }
  as_backend(fun, name = "smartpls_export",
             description = "Skeleton backend for SmartPLS export parsers.")
}

optional_backend <- function(package, estimator) {
  fun <- function(model, data, scenario = NULL, ...) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(
        "Optional package `", package, "` is not installed. ",
        "Install it or supply a custom backend via `as_backend()`.",
        call. = FALSE
      )
    }
    if (is.null(estimator)) {
      stop(
        "`", package, "` backend integration is a documented skeleton. ",
        "Supply `estimator` that returns canonical stressPLS backend output.",
        call. = FALSE
      )
    }
    estimator(model = model, data = data, scenario = scenario, ...)
  }
  as_backend(fun, name = tolower(package),
             description = paste("Optional", package, "backend skeleton."))
}
