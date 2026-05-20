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

#' @export
print.stresspls_construct_spec <- function(x, ...) {
  cat("<stresspls_construct_spec>\n")
  cat("Name:", x$name, "\n")
  cat("Mode:", x$mode, "\n")
  cat("Indicators:", paste(x$indicators, collapse = ", "), "\n")
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_hoc_spec <- function(x, ...) {
  cat("<stresspls_hoc_spec>\n")
  cat("Name:", x$name, "\n")
  cat("Mode:", x$mode, "\n")
  cat("Approach:", x$approach, "\n")
  cat("Dimensions:", paste(x$dimensions, collapse = ", "), "\n")
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  invisible(x)
}

#' @export
print.stresspls_path_spec <- function(x, ...) {
  cat("<stresspls_path_spec>\n")
  cat("Paths:", nrow(x$paths), "\n")
  print(x$paths, row.names = FALSE)
  invisible(x)
}

#' @export
print.stresspls_model_spec <- function(x, ...) {
  cat("<stresspls_model_spec>\n")
  cat("Constructs:", length(x$constructs), "\n")
  cat("Higher-order constructs:", length(x$hocs), "\n")
  cat("Paths:", nrow(x$paths$paths), "\n")
  if (length(construct_names(x)) > 0L) {
    cat("Names:", paste(construct_names(x), collapse = ", "), "\n")
  }
  invisible(x)
}
