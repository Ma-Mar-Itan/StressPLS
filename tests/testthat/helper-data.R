example_data <- function() {
  data.frame(
    x1 = c(1, 2, 3, 4),
    x2 = c(2, 3, 4, 5),
    x3 = c(3, 4, 5, 6),
    y = c(4, 5, 6, 7)
  )
}

example_model <- function() {
  list(
    indicators = c("x1", "x2", "x3"),
    paths = list(y = c("x1", "x2", "x3"))
  )
}
