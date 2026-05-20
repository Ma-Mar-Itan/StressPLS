#' Define a lower-order construct
#'
#' `specify_construct()` declares a manifest-variable construct for a PLS-SEM
#' model specification. It stores the construct only; it does not estimate any
#' model parameters.
#'
#' @param name Single construct name.
#' @param indicators Character vector of manifest indicator names.
#' @param mode Measurement mode. Defaults to `"formative"`.
#' @param description Optional single character description.
#'
#' @return An object of class `stresspls_construct_spec`.
#' @examples
#' image <- specify_construct(
#'   name = "Image",
#'   indicators = c("img1", "img2", "img3")
#' )
#' image
#' @export
specify_construct <- function(name, indicators, mode = "formative",
                              description = NULL) {
  name <- validate_spec_name(name, "name")
  indicators <- validate_indicator_vector(indicators)
  mode <- validate_single_string(mode, "mode")
  description <- validate_optional_description(description)

  structure(
    list(
      name = name,
      indicators = indicators,
      mode = mode,
      description = description
    ),
    class = "stresspls_construct_spec"
  )
}

#' Define a higher-order construct
#'
#' `specify_hoc()` declares a higher-order construct from named lower-order
#' dimensions. It stores the specification only; estimation backends will consume
#' these objects in later development.
#'
#' @param name Single higher-order construct name.
#' @param dimensions Character vector naming lower-order construct dimensions.
#' @param mode Measurement mode. Defaults to `"formative"`.
#' @param approach Higher-order construct approach. Defaults to `"two_stage"`.
#' @param description Optional single character description.
#'
#' @return An object of class `stresspls_hoc_spec`.
#' @examples
#' brand_equity <- specify_hoc(
#'   name = "BrandEquity",
#'   dimensions = c("Image", "Quality")
#' )
#' brand_equity
#' @export
specify_hoc <- function(name, dimensions, mode = "formative",
                        approach = "two_stage", description = NULL) {
  name <- validate_spec_name(name, "name")
  dimensions <- validate_name_vector(dimensions, "dimensions")
  mode <- validate_single_string(mode, "mode")
  approach <- validate_single_string(approach, "approach")
  description <- validate_optional_description(description)

  structure(
    list(
      name = name,
      dimensions = dimensions,
      mode = mode,
      approach = approach,
      description = description
    ),
    class = "stresspls_hoc_spec"
  )
}

#' Define structural paths
#'
#' `specify_paths()` declares directed structural paths. Paths can be supplied as
#' a two-column data frame with columns `from` and `to`, or as paired `from` and
#' `to` character vectors.
#'
#' @param paths Optional two-column data frame with `from` and `to` columns.
#' @param from Optional character vector of source construct names.
#' @param to Optional character vector of target construct names.
#'
#' @return An object of class `stresspls_path_spec`.
#' @examples
#' specify_paths(from = "BrandEquity", to = "Satisfaction")
#'
#' path_df <- data.frame(from = "BrandEquity", to = "Satisfaction")
#' specify_paths(path_df)
#' @export
specify_paths <- function(paths = NULL, from = NULL, to = NULL) {
  if (!is.null(paths)) {
    if (!is.null(from) || !is.null(to)) {
      stop("Supply either `paths` or paired `from` and `to`, not both.",
           call. = FALSE)
    }
    path_data <- validate_path_data_frame(paths)
  } else {
    from <- validate_name_vector(from, "from")
    to <- validate_name_vector(to, "to")
    if (length(from) != length(to)) {
      stop("`from` and `to` must have the same length.", call. = FALSE)
    }
    path_data <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  }

  structure(
    list(paths = path_data),
    class = "stresspls_path_spec"
  )
}

#' Combine stressPLS model specification components
#'
#' `specify_model()` combines lower-order constructs, higher-order constructs,
#' and structural paths into a `stresspls_model_spec`. The returned object is
#' validated immediately.
#'
#' @param constructs List of `stresspls_construct_spec` objects.
#' @param hocs Optional list of `stresspls_hoc_spec` objects.
#' @param paths Optional `stresspls_path_spec` object.
#'
#' @return An object of class `stresspls_model_spec`.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' quality <- specify_construct("Quality", c("qual1", "qual2"))
#' satisfaction <- specify_construct("Satisfaction", c("sat1", "sat2"),
#'   mode = "reflective"
#' )
#' brand_equity <- specify_hoc("BrandEquity", c("Image", "Quality"))
#' paths <- specify_paths(from = "BrandEquity", to = "Satisfaction")
#'
#' model <- specify_model(
#'   constructs = list(image, quality, satisfaction),
#'   hocs = list(brand_equity),
#'   paths = paths
#' )
#' model
#' @export
specify_model <- function(constructs, hocs = list(), paths = NULL) {
  if (!is.list(constructs) || length(constructs) == 0L) {
    stop("`constructs` must be a non-empty list of construct specifications.",
         call. = FALSE)
  }
  if (!all(vapply(constructs, inherits, logical(1),
                  what = "stresspls_construct_spec"))) {
    stop("Every item in `constructs` must be a stresspls_construct_spec object.",
         call. = FALSE)
  }
  if (!is.list(hocs)) {
    stop("`hocs` must be a list of higher-order construct specifications.",
         call. = FALSE)
  }
  if (length(hocs) > 0L &&
      !all(vapply(hocs, inherits, logical(1), what = "stresspls_hoc_spec"))) {
    stop("Every item in `hocs` must be a stresspls_hoc_spec object.",
         call. = FALSE)
  }
  if (is.null(paths)) {
    paths <- structure(
      list(paths = data.frame(
        from = character(),
        to = character(),
        stringsAsFactors = FALSE
      )),
      class = "stresspls_path_spec"
    )
  }
  if (!inherits(paths, "stresspls_path_spec")) {
    stop("`paths` must be a stresspls_path_spec object.", call. = FALSE)
  }

  model <- structure(
    list(
      constructs = constructs,
      hocs = hocs,
      paths = paths
    ),
    class = "stresspls_model_spec"
  )
  validate_model_spec(model)
  model
}

#' Validate a stressPLS model specification
#'
#' `validate_model_spec()` checks the internal consistency of a
#' `stresspls_model_spec` and, optionally, whether all manifest indicators are
#' present in a supplied data frame.
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to check required indicators.
#'
#' @return The input `model`, invisibly, when validation succeeds.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' satisfaction <- specify_construct("Satisfaction", c("sat1", "sat2"))
#' paths <- specify_paths(from = "Image", to = "Satisfaction")
#' model <- specify_model(list(image, satisfaction), paths = paths)
#' validate_model_spec(model)
#'
#' dat <- data.frame(img1 = 1:3, img2 = 2:4, sat1 = 3:5, sat2 = 4:6)
#' validate_model_spec(model, data = dat)
#' @export
validate_model_spec <- function(model, data = NULL) {
  if (!inherits(model, "stresspls_model_spec")) {
    stop("`model` must be a stresspls_model_spec object.", call. = FALSE)
  }

  constructs <- model$constructs
  hocs <- model$hocs
  paths <- model$paths

  if (!is.list(constructs) || length(constructs) == 0L) {
    stop("`model$constructs` must be a non-empty list.", call. = FALSE)
  }
  if (!all(vapply(constructs, inherits, logical(1),
                  what = "stresspls_construct_spec"))) {
    stop("Every construct must be a stresspls_construct_spec object.",
         call. = FALSE)
  }
  if (!is.list(hocs)) {
    stop("`model$hocs` must be a list.", call. = FALSE)
  }
  if (length(hocs) > 0L &&
      !all(vapply(hocs, inherits, logical(1), what = "stresspls_hoc_spec"))) {
    stop("Every HOC must be a stresspls_hoc_spec object.", call. = FALSE)
  }
  if (!inherits(paths, "stresspls_path_spec")) {
    stop("`model$paths` must be a stresspls_path_spec object.", call. = FALSE)
  }

  lower_names <- vapply(constructs, `[[`, character(1), "name")
  hoc_names <- vapply(hocs, `[[`, character(1), "name")
  all_names <- c(lower_names, hoc_names)
  duplicate_names <- unique(all_names[duplicated(all_names)])
  if (length(duplicate_names) > 0L) {
    stop(
      "Construct names must be unique; duplicates: ",
      paste(duplicate_names, collapse = ", "),
      call. = FALSE
    )
  }

  validate_construct_indicators(constructs)
  validate_hoc_dimensions(hocs, lower_names)
  validate_structural_paths(paths$paths, all_names)

  if (!is.null(data)) {
    validate_data(data)
    missing_indicators <- setdiff(required_indicators(model), names(data))
    if (length(missing_indicators) > 0L) {
      stop(
        "`data` is missing required indicators: ",
        paste(missing_indicators, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(model)
}

#' Return required manifest indicators
#'
#' `required_indicators()` returns the unique manifest variables required by all
#' lower-order constructs in a model specification.
#'
#' @param model A `stresspls_model_spec` object.
#'
#' @return A character vector of indicator names.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' required_indicators(model)
#' @export
required_indicators <- function(model) {
  if (!inherits(model, "stresspls_model_spec")) {
    stop("`model` must be a stresspls_model_spec object.", call. = FALSE)
  }
  unique(unlist(lapply(model$constructs, `[[`, "indicators"),
                use.names = FALSE))
}

#' Return construct names
#'
#' `construct_names()` returns lower-order and higher-order construct names from
#' a model specification.
#'
#' @param model A `stresspls_model_spec` object.
#'
#' @return A character vector of construct names.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' quality <- specify_construct("Quality", c("qual1", "qual2"))
#' brand_equity <- specify_hoc("BrandEquity", c("Image", "Quality"))
#' model <- specify_model(list(image, quality), hocs = list(brand_equity))
#' construct_names(model)
#' @export
construct_names <- function(model) {
  if (!inherits(model, "stresspls_model_spec")) {
    stop("`model` must be a stresspls_model_spec object.", call. = FALSE)
  }
  c(
    vapply(model$constructs, `[[`, character(1), "name"),
    vapply(model$hocs, `[[`, character(1), "name")
  )
}

validate_single_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || x == "") {
    stop(sprintf("`%s` must be a single non-empty string.", name),
         call. = FALSE)
  }
  x
}

validate_spec_name <- function(x, name) {
  x <- validate_single_string(x, name)
  if (grepl("\\s", x)) {
    stop(sprintf("`%s` must not contain whitespace.", name), call. = FALSE)
  }
  x
}

validate_optional_description <- function(description) {
  if (is.null(description)) {
    return(NULL)
  }
  validate_single_string(description, "description")
}

validate_name_vector <- function(x, name) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || any(x == "")) {
    stop(sprintf("`%s` must be a non-empty character vector.", name),
         call. = FALSE)
  }
  if (any(grepl("\\s", x))) {
    stop(sprintf("`%s` must not contain whitespace.", name), call. = FALSE)
  }
  x
}

validate_indicator_vector <- function(indicators) {
  if (!is.character(indicators) || anyNA(indicators) ||
      any(indicators == "")) {
    stop("`indicators` must be a character vector without missing or empty values.",
         call. = FALSE)
  }
  if (length(indicators) == 0L) {
    stop("`indicators` must contain at least one indicator.", call. = FALSE)
  }
  indicators
}

validate_path_data_frame <- function(paths) {
  paths <- as.data.frame(paths, stringsAsFactors = FALSE)
  if (!all(c("from", "to") %in% names(paths))) {
    stop("`paths` must contain `from` and `to` columns.", call. = FALSE)
  }
  path_data <- paths[, c("from", "to"), drop = FALSE]
  path_data$from <- validate_name_vector(path_data$from, "from")
  path_data$to <- validate_name_vector(path_data$to, "to")
  path_data
}

validate_construct_indicators <- function(constructs) {
  for (construct in constructs) {
    validate_spec_name(construct$name, "construct name")
    validate_indicator_vector(construct$indicators)
  }
  invisible(constructs)
}

validate_hoc_dimensions <- function(hocs, lower_names) {
  for (hoc in hocs) {
    validate_spec_name(hoc$name, "HOC name")
    validate_name_vector(hoc$dimensions, "dimensions")
    missing_dimensions <- setdiff(hoc$dimensions, lower_names)
    if (length(missing_dimensions) > 0L) {
      stop(
        "HOC `", hoc$name, "` references missing lower-order constructs: ",
        paste(missing_dimensions, collapse = ", "),
        call. = FALSE
      )
    }
  }
  invisible(hocs)
}

validate_structural_paths <- function(paths, valid_names) {
  if (!is.data.frame(paths) || !all(c("from", "to") %in% names(paths))) {
    stop("Structural paths must be stored as a data frame with `from` and `to`.",
         call. = FALSE)
  }
  if (nrow(paths) == 0L) {
    return(invisible(paths))
  }
  validate_name_vector(paths$from, "from")
  validate_name_vector(paths$to, "to")

  missing_from <- setdiff(paths$from, valid_names)
  missing_to <- setdiff(paths$to, valid_names)
  missing_refs <- unique(c(missing_from, missing_to))
  if (length(missing_refs) > 0L) {
    stop(
      "Structural paths reference missing constructs: ",
      paste(missing_refs, collapse = ", "),
      call. = FALSE
    )
  }

  path_keys <- paste(paths$from, paths$to, sep = " -> ")
  duplicate_paths <- unique(path_keys[duplicated(path_keys)])
  if (length(duplicate_paths) > 0L) {
    stop(
      "Duplicate structural paths are not allowed: ",
      paste(duplicate_paths, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(paths$from == paths$to)) {
    stop("Structural paths must not point from a construct to itself.",
         call. = FALSE)
  }
  if (has_directed_cycle(paths)) {
    stop("Structural paths contain a directed cycle.", call. = FALSE)
  }

  invisible(paths)
}

has_directed_cycle <- function(paths) {
  nodes <- unique(c(paths$from, paths$to))
  visiting <- rep(FALSE, length(nodes))
  names(visiting) <- nodes
  visited <- rep(FALSE, length(nodes))
  names(visited) <- nodes
  adjacency <- vector("list", length(nodes))
  names(adjacency) <- nodes
  for (node in nodes) {
    adjacency[[node]] <- paths$to[paths$from == node]
  }

  visit <- function(node) {
    if (isTRUE(visiting[[node]])) {
      return(TRUE)
    }
    if (isTRUE(visited[[node]])) {
      return(FALSE)
    }
    visiting[[node]] <<- TRUE
    for (next_node in adjacency[[node]]) {
      if (visit(next_node)) {
        return(TRUE)
      }
    }
    visiting[[node]] <<- FALSE
    visited[[node]] <<- TRUE
    FALSE
  }

  for (node in nodes) {
    if (visit(node)) {
      return(TRUE)
    }
  }
  FALSE
}
