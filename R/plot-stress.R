#' Plot stressPLS summaries
#'
#' @param x A `stresspls_result` or `stresspls_summary` object.
#' @param ... Reserved for future methods.
#'
#' @return A `ggplot` object.
#' @examples
#' dat <- data.frame(x1 = 1:4, y = 2:5)
#' model <- list(indicators = "x1")
#' result <- stress_pls(dat, model)
#' plot_stress(result)
#' @export
#' @importFrom ggplot2 aes geom_col ggplot labs theme_minimal
plot_stress <- function(x, ...) {
  UseMethod("plot_stress")
}

#' @export
plot_stress.stresspls_result <- function(x, ...) {
  plot_stress(summarise_stress(x), ...)
}

#' @export
plot_stress.stresspls_summary <- function(x, ...) {
  ggplot2::ggplot(x$status_counts, ggplot2::aes(x = status, y = n)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Scenario status",
      y = "Count",
      title = "stressPLS scenario status"
    ) +
    ggplot2::theme_minimal()
}

#' @export
plot.stresspls_result <- function(x, ...) {
  plot_stress(x, ...)
}

#' @export
plot.stresspls_summary <- function(x, ...) {
  plot_stress(x, ...)
}
