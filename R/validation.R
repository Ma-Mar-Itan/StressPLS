validate_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  invisible(data)
}

validate_model <- function(model) {
  if (!is.list(model)) {
    stop("`model` must be a list describing the PLS-SEM specification.",
         call. = FALSE)
  }
  if (length(model) == 0L) {
    stop("`model` must not be empty.", call. = FALSE)
  }
  invisible(model)
}

validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(invisible(seed))
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be a single non-missing number or `NULL`.",
         call. = FALSE)
  }
  invisible(seed)
}

with_seed <- function(seed, expr) {
  validate_seed(seed)
  if (is.null(seed)) {
    return(force(expr))
  }

  old_seed <- NULL
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  set.seed(seed)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  force(expr)
}

validate_grid <- function(grid) {
  if (!inherits(grid, "stresspls_grid")) {
    stop("`grid` must be a stresspls_grid object.", call. = FALSE)
  }
  invisible(grid)
}

validate_weights <- function(weights) {
  if (!is.numeric(weights) || length(weights) == 0L || anyNA(weights)) {
    stop("`weights` must be a non-empty numeric vector without missing values.",
         call. = FALSE)
  }
  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop("`weights` must be a named numeric vector.", call. = FALSE)
  }
  invisible(weights)
}

as_non_empty_character <- function(x, name) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || any(x == "")) {
    stop(sprintf("`%s` must be a non-empty character vector.", name),
         call. = FALSE)
  }
  x
}

empty_fragility_table <- function() {
  data.frame(
    scenario_id = character(),
    fragility_score = numeric(),
    rank = integer(),
    stringsAsFactors = FALSE
  )
}
