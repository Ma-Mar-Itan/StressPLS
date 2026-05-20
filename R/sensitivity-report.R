#' Create a text sensitivity report
#'
#' `sensitivity_report()` returns a compact text report for a stressPLS result
#' or summary. Rich report formats will be added after the methodology and
#' backend interfaces stabilize.
#'
#' @param x A `stresspls_result` or `stresspls_summary` object.
#'
#' @return A character vector containing report lines.
#' @examples
#' dat <- data.frame(x1 = 1:4, y = 2:5)
#' model <- list(indicators = "x1")
#' result <- stress_pls(dat, model)
#' sensitivity_report(result)
#' @export
sensitivity_report <- function(x) {
  if (inherits(x, "stresspls_result")) {
    x <- summarise_stress(x)
  }
  if (!inherits(x, "stresspls_summary")) {
    stop("`x` must be a stresspls_result or stresspls_summary object.",
         call. = FALSE)
  }

  total <- sum(x$status_counts$n)
  lines <- c(
    "stressPLS sensitivity report",
    paste0("Scenarios: ", total),
    paste0(
      x$status_counts$status,
      ": ",
      x$status_counts$n
    )
  )
  class(lines) <- c("stresspls_report", class(lines))
  lines
}
