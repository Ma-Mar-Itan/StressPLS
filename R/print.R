#' @export
print.stresspls_grid <- function(x, ...) {
  cat("<stresspls_grid>\n")
  cat("Scenarios:", nrow(x$scenarios), "\n")
  if (!is.null(x$metadata$type)) {
    cat("Type:", x$metadata$type, "\n")
  }
  print(utils::head(x$scenarios), row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_result <- function(x, ...) {
  cat("<stresspls_result>\n")
  cat("Scenarios:", nrow(x$results), "\n")
  backend <- if (is.null(x$backend)) "none" else x$backend
  cat("Backend:", backend, "\n")
  print(utils::head(x$results), row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_summary <- function(x, ...) {
  cat("<stresspls_summary>\n")
  cat("Scenarios:", sum(x$status_counts$n), "\n")
  print(x$status_counts, row.names = FALSE)
  invisible(x)
}
