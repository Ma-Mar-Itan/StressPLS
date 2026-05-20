#' Summarise a stressPLS result
#'
#' @param x A `stresspls_result` or `stresspls_fit_grid` object.
#'
#' @return A `stresspls_summary` object.
#' @examples
#' dat <- data.frame(x1 = 1:4, y = 2:5)
#' model <- list(indicators = "x1")
#' result <- stress_pls(dat, model)
#' summarise_stress(result)
#' @export
summarise_stress <- function(x) {
  if (inherits(x, "stresspls_fit_grid")) {
    status <- ifelse(x$scenario_index$converged, "estimated", "error")
    counts <- as.data.frame(table(status), stringsAsFactors = FALSE)
    names(counts) <- c("status", "n")
    counts$n <- as.integer(counts$n)
    return(structure(
      list(result = x, status_counts = counts),
      class = "stresspls_summary"
    ))
  }
  if (!inherits(x, "stresspls_result")) {
    stop("`x` must be a stresspls_result or stresspls_fit_grid object.",
         call. = FALSE)
  }
  status <- x$results$status
  if (is.null(status)) {
    status <- rep("unknown", nrow(x$results))
  }
  counts <- as.data.frame(table(status), stringsAsFactors = FALSE)
  names(counts) <- c("status", "n")
  counts$n <- as.integer(counts$n)
  new_stresspls_summary(x, counts)
}
