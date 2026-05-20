#' Rank stress-test scenarios by fragility
#'
#' `rank_fragility()` ranks scenarios when a backend has supplied a numeric
#' `fragility_score` column. Without that column, it returns an empty ranking
#' instead of inventing scores.
#'
#' @param x A `stresspls_result`, `stresspls_summary`, or
#'   `stresspls_fit_grid` object.
#'
#' @return A data frame with `scenario_id`, `fragility_score`, and `rank`.
#' @examples
#' dat <- data.frame(x1 = 1:4, y = 2:5)
#' model <- list(indicators = "x1")
#' result <- stress_pls(dat, model)
#' rank_fragility(result)
#' @export
rank_fragility <- function(x) {
  if (inherits(x, "stresspls_fit_grid")) {
    ranked <- calc_indicator_fragility(x)
    if (nrow(ranked) == 0L) {
      return(ranked)
    }
    ranked <- ranked[order(ranked$fragility_score, decreasing = TRUE), ,
                     drop = FALSE]
    ranked$rank <- seq_len(nrow(ranked))
    return(ranked)
  }
  if (inherits(x, "stresspls_summary")) {
    x <- x$result
  }
  if (!inherits(x, "stresspls_result")) {
    stop("`x` must be a stresspls_result or stresspls_summary object.",
         call. = FALSE)
  }

  results <- x$results
  if (!"fragility_score" %in% names(results)) {
    out <- empty_fragility_table()
    attr(out, "note") <- "No `fragility_score` column is available."
    return(out)
  }
  if (!is.numeric(results$fragility_score)) {
    stop("`fragility_score` must be numeric.", call. = FALSE)
  }

  ranked <- results[!is.na(results$fragility_score), , drop = FALSE]
  ranked <- ranked[order(ranked$fragility_score, decreasing = TRUE), ,
                   drop = FALSE]
  data.frame(
    scenario_id = ranked$scenario_id,
    fragility_score = ranked$fragility_score,
    rank = seq_len(nrow(ranked)),
    stringsAsFactors = FALSE
  )
}
