#' Generate indicator perturbation scenarios
#'
#' `perturb_indicators()` dispatches to one or more indicator perturbation
#' generators and returns a `stresspls_perturbation_grid`. It does not estimate
#' PLS-SEM models.
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to validate required indicators and,
#'   for noise injection, optionally materialize transformed data.
#' @param method Perturbation method. One or more of `"leave_one_out"`,
#'   `"random_removal"`, `"combinatorial_deletion"`, `"replacement"`, and
#'   `"noise_injection"`.
#' @param constructs Optional character vector of lower-order constructs to
#'   perturb. If `NULL`, all lower-order constructs are considered.
#' @param n_remove Number of indicators to remove for deletion methods.
#' @param n_scenarios Number of random scenarios to generate.
#' @param replacement_pool Candidate replacement indicators for replacement
#'   scenarios. Can be a character vector or a named list by construct.
#' @param noise_sd Positive numeric standard deviation for noise injection.
#' @param seed Optional random seed for stochastic methods.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2", "img3"))
#' quality <- specify_construct("Quality", c("qual1", "qual2", "qual3"))
#' model <- specify_model(list(image, quality))
#'
#' perturb_indicators(model, method = "leave_one_out", constructs = "Image")
#' @export
perturb_indicators <- function(model, data = NULL,
                               method = c(
                                 "leave_one_out",
                                 "random_removal",
                                 "combinatorial_deletion",
                                 "replacement",
                                 "noise_injection"
                               ),
                               constructs = NULL, n_remove = 1,
                               n_scenarios = NULL, replacement_pool = NULL,
                               noise_sd = NULL, seed = NULL) {
  method <- match.arg(method, several.ok = TRUE)
  grids <- vector("list", length(method))

  for (i in seq_along(method)) {
    grids[[i]] <- switch(
      method[[i]],
      leave_one_out = leave_one_indicator_out(
        model = model,
        data = data,
        constructs = constructs,
        seed = seed
      ),
      random_removal = random_indicator_removal(
        model = model,
        data = data,
        constructs = constructs,
        n_remove = n_remove,
        n_scenarios = n_scenarios,
        seed = seed
      ),
      combinatorial_deletion = combinatorial_indicator_deletion(
        model = model,
        data = data,
        constructs = constructs,
        n_remove = n_remove,
        seed = seed
      ),
      replacement = indicator_replacement(
        model = model,
        data = data,
        constructs = constructs,
        replacement_pool = replacement_pool,
        seed = seed
      ),
      noise_injection = noise_injection(
        model = model,
        data = data,
        constructs = constructs,
        noise_sd = noise_sd,
        seed = seed
      )
    )
  }

  scenarios <- do.call(rbind_perturbation_scenarios,
                       lapply(grids, `[[`, "scenarios"))
  as_perturbation_grid(
    model = model,
    scenarios = scenarios,
    methods = method,
    seed = seed,
    metadata = list(
      constructs = selected_construct_names(model, constructs),
      data_materialized = any(vapply(
        grids,
        function(grid) isTRUE(grid$metadata$data_materialized),
        logical(1)
      ))
    )
  )
}

#' Generate leave-one-indicator-out scenarios
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to validate required indicators.
#' @param constructs Optional lower-order constructs to perturb.
#' @param seed Optional seed stored on the returned grid.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' leave_one_indicator_out(model)
#' @export
leave_one_indicator_out <- function(model, data = NULL, constructs = NULL,
                                    seed = NULL) {
  targets <- prepare_perturbation_targets(model, data, constructs)
  rows <- list()

  for (construct in targets) {
    indicators <- construct$indicators
    if (length(indicators) <= 1L) {
      next
    }
    for (indicator in indicators) {
      rows[[length(rows) + 1L]] <- make_perturbation_scenario(
        scenario_id = make_scenario_id(
          "leave_one_out",
          construct$name,
          indicator
        ),
        method = "leave_one_out",
        construct = construct$name,
        removed_indicators = indicator,
        description = paste0(
          "Remove indicator `", indicator, "` from construct `",
          construct$name, "`."
        )
      )
    }
  }

  scenarios <- scenario_rows_to_data_frame(rows)
  as_perturbation_grid(
    model = model,
    scenarios = scenarios,
    methods = "leave_one_out",
    seed = seed,
    metadata = list(
      constructs = vapply(targets, `[[`, character(1), "name"),
      data_materialized = FALSE
    )
  )
}

#' Generate random indicator-removal scenarios
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to validate required indicators.
#' @param constructs Optional lower-order constructs to perturb.
#' @param n_remove Number of indicators to remove per scenario.
#' @param n_scenarios Number of scenarios to sample. Defaults to the number of
#'   valid removal combinations, capped at 10.
#' @param seed Optional random seed.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2", "img3"))
#' model <- specify_model(list(image))
#' random_indicator_removal(model, n_remove = 1, n_scenarios = 2, seed = 1)
#' @export
random_indicator_removal <- function(model, data = NULL, constructs = NULL,
                                     n_remove = 1, n_scenarios = NULL,
                                     seed = NULL) {
  targets <- prepare_perturbation_targets(model, data, constructs)
  n_remove <- validate_positive_whole_number(n_remove, "n_remove")
  if (!is.null(n_scenarios)) {
    n_scenarios <- validate_positive_whole_number(n_scenarios, "n_scenarios")
  }

  candidates <- removal_candidates(targets, n_remove, exact = TRUE)
  if (nrow(candidates) == 0L) {
    scenarios <- empty_perturbation_scenarios()
  } else {
    if (is.null(n_scenarios)) {
      n_scenarios <- min(10L, nrow(candidates))
    }
    selected <- with_seed(seed, {
      candidates[sample.int(
        nrow(candidates),
        size = n_scenarios,
        replace = n_scenarios > nrow(candidates)
      ), , drop = FALSE]
    })

    rows <- vector("list", nrow(selected))
    for (i in seq_len(nrow(selected))) {
      removed <- selected$removed_indicators[[i]]
      rows[[i]] <- make_perturbation_scenario(
        scenario_id = make_scenario_id(
          "random_removal",
          sprintf("%03d", i),
          selected$construct[[i]],
          paste(removed, collapse = "_")
        ),
        method = "random_removal",
        construct = selected$construct[[i]],
        removed_indicators = removed,
        description = paste0(
          "Randomly remove ",
          paste(removed, collapse = ", "),
          " from construct `",
          selected$construct[[i]],
          "`."
        )
      )
    }
    scenarios <- scenario_rows_to_data_frame(rows)
  }

  as_perturbation_grid(
    model = model,
    scenarios = scenarios,
    methods = "random_removal",
    seed = seed,
    metadata = list(
      constructs = vapply(targets, `[[`, character(1), "name"),
      data_materialized = FALSE,
      n_remove = n_remove,
      n_scenarios = n_scenarios
    )
  )
}

#' Generate combinatorial indicator-deletion scenarios
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to validate required indicators.
#' @param constructs Optional lower-order constructs to perturb.
#' @param n_remove Maximum number of indicators to remove. All valid deletion
#'   sizes from one to `n_remove` are generated.
#' @param seed Optional seed stored on the returned grid.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2", "img3"))
#' model <- specify_model(list(image))
#' combinatorial_indicator_deletion(model, n_remove = 2)
#' @export
combinatorial_indicator_deletion <- function(model, data = NULL,
                                             constructs = NULL, n_remove = 1,
                                             seed = NULL) {
  targets <- prepare_perturbation_targets(model, data, constructs)
  n_remove <- validate_positive_whole_number(n_remove, "n_remove")
  candidates <- removal_candidates(targets, n_remove, exact = FALSE)

  rows <- vector("list", nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    removed <- candidates$removed_indicators[[i]]
    rows[[i]] <- make_perturbation_scenario(
      scenario_id = make_scenario_id(
        "combinatorial_deletion",
        candidates$construct[[i]],
        paste(removed, collapse = "_")
      ),
      method = "combinatorial_deletion",
      construct = candidates$construct[[i]],
      removed_indicators = removed,
      description = paste0(
        "Remove ",
        paste(removed, collapse = ", "),
        " from construct `",
        candidates$construct[[i]],
        "`."
      )
    )
  }

  as_perturbation_grid(
    model = model,
    scenarios = scenario_rows_to_data_frame(rows),
    methods = "combinatorial_deletion",
    seed = seed,
    metadata = list(
      constructs = vapply(targets, `[[`, character(1), "name"),
      data_materialized = FALSE,
      n_remove = n_remove
    )
  )
}

#' Generate indicator-replacement scenarios
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame used to validate required indicators and
#'   replacement candidates.
#' @param constructs Optional lower-order constructs to perturb.
#' @param replacement_pool Candidate replacement indicators. Can be a character
#'   vector applied to all constructs or a named list by construct.
#' @param seed Optional seed stored on the returned grid.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' indicator_replacement(model, replacement_pool = c("img_alt1", "img_alt2"))
#' @export
indicator_replacement <- function(model, data = NULL, constructs = NULL,
                                  replacement_pool = NULL, seed = NULL) {
  targets <- prepare_perturbation_targets(model, data, constructs)
  pool <- validate_replacement_pool(replacement_pool, targets, data)
  rows <- list()

  for (construct in targets) {
    candidates <- pool[[construct$name]]
    disallowed <- intersect(candidates, construct$indicators)
    if (length(disallowed) > 0L) {
      stop(
        "`replacement_pool` contains indicators already used in construct `",
        construct$name,
        "`: ",
        paste(disallowed, collapse = ", "),
        call. = FALSE
      )
    }
    for (indicator in construct$indicators) {
      for (candidate in candidates) {
        replacement <- stats::setNames(candidate, indicator)
        rows[[length(rows) + 1L]] <- make_perturbation_scenario(
          scenario_id = make_scenario_id(
            "replacement",
            construct$name,
            indicator,
            candidate
          ),
          method = "replacement",
          construct = construct$name,
          added_indicators = candidate,
          replaced_indicators = replacement,
          description = paste0(
            "Replace `",
            indicator,
            "` with `",
            candidate,
            "` in construct `",
            construct$name,
            "`."
          )
        )
      }
    }
  }

  as_perturbation_grid(
    model = model,
    scenarios = scenario_rows_to_data_frame(rows),
    methods = "replacement",
    seed = seed,
    metadata = list(
      constructs = vapply(targets, `[[`, character(1), "name"),
      data_materialized = FALSE
    )
  )
}

#' Generate noise-injection scenarios
#'
#' `noise_injection()` stores reproducible noise recipes for indicators. When
#' `data` is supplied, transformed scenario data are also materialized by adding
#' Gaussian noise to one indicator per scenario.
#'
#' @param model A `stresspls_model_spec` object.
#' @param data Optional data frame. If supplied, all required indicators must be
#'   present and numeric indicators are transformed for each scenario.
#' @param constructs Optional lower-order constructs to perturb.
#' @param noise_sd Positive numeric noise standard deviation.
#' @param seed Optional random seed.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' noise_injection(model, noise_sd = 0.1, seed = 1)
#' @export
noise_injection <- function(model, data = NULL, constructs = NULL,
                            noise_sd = NULL, seed = NULL) {
  targets <- prepare_perturbation_targets(model, data, constructs)
  noise_sd <- validate_noise_sd(noise_sd)
  indicators <- unlist(lapply(targets, `[[`, "indicators"), use.names = FALSE)
  scenario_seeds <- with_seed(seed, {
    sample.int(.Machine$integer.max, length(indicators))
  })

  rows <- vector("list", length(indicators))
  index <- 1L
  for (construct in targets) {
    for (indicator in construct$indicators) {
      noise_spec <- list(
        indicator = indicator,
        distribution = "normal",
        mean = 0,
        sd = noise_sd,
        seed = scenario_seeds[[index]]
      )
      scenario_data <- NULL
      if (!is.null(data)) {
        if (!is.numeric(data[[indicator]])) {
          stop(
            "Noise injection requires numeric data for indicator `",
            indicator,
            "`.",
            call. = FALSE
          )
        }
        scenario_data <- data
        scenario_data[[indicator]] <- with_seed(scenario_seeds[[index]], {
          data[[indicator]] + stats::rnorm(nrow(data), mean = 0, sd = noise_sd)
        })
      }
      rows[[index]] <- make_perturbation_scenario(
        scenario_id = make_scenario_id(
          "noise_injection",
          construct$name,
          indicator
        ),
        method = "noise_injection",
        construct = construct$name,
        noise_spec = noise_spec,
        data = scenario_data,
        description = paste0(
          "Inject normal noise into `",
          indicator,
          "` for construct `",
          construct$name,
          "`."
        )
      )
      index <- index + 1L
    }
  }

  as_perturbation_grid(
    model = model,
    scenarios = scenario_rows_to_data_frame(rows),
    methods = "noise_injection",
    seed = seed,
    metadata = list(
      constructs = vapply(targets, `[[`, character(1), "name"),
      data_materialized = !is.null(data),
      noise_sd = noise_sd
    )
  )
}

#' Construct a perturbation grid
#'
#' @param model A `stresspls_model_spec` object.
#' @param scenarios Scenario data frame.
#' @param methods Character vector of perturbation methods represented.
#' @param seed Optional seed used to generate stochastic scenarios.
#' @param created_at Creation time. Defaults to `Sys.time()`.
#' @param metadata Named list of additional grid metadata.
#'
#' @return A `stresspls_perturbation_grid` object.
#' @examples
#' image <- specify_construct("Image", c("img1", "img2"))
#' model <- specify_model(list(image))
#' scenarios <- data.frame(
#'   scenario_id = character(),
#'   method = character(),
#'   construct = character()
#' )
#' as_perturbation_grid(model, scenarios)
#' @export
as_perturbation_grid <- function(model, scenarios, methods = character(),
                                 seed = NULL, created_at = Sys.time(),
                                 metadata = list()) {
  if (!inherits(model, "stresspls_model_spec")) {
    stop("`model` must be a stresspls_model_spec object.", call. = FALSE)
  }
  validate_model_spec(model)
  validate_seed(seed)
  if (!is.data.frame(scenarios)) {
    stop("`scenarios` must be a data frame.", call. = FALSE)
  }
  scenarios <- ensure_perturbation_columns(scenarios)
  if (anyDuplicated(scenarios$scenario_id)) {
    stop("`scenario_id` values must be unique.", call. = FALSE)
  }
  if (!is.character(methods)) {
    stop("`methods` must be a character vector.", call. = FALSE)
  }
  if (!is.list(metadata) ||
      (length(metadata) > 0L && is.null(names(metadata)))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }

  structure(
    list(
      model = model,
      scenarios = scenarios,
      methods = unique(methods),
      seed = seed,
      created_at = created_at,
      metadata = metadata
    ),
    class = "stresspls_perturbation_grid"
  )
}

prepare_perturbation_targets <- function(model, data, constructs) {
  if (!inherits(model, "stresspls_model_spec")) {
    stop("`model` must be a stresspls_model_spec object.", call. = FALSE)
  }
  validate_model_spec(model, data = data)
  names <- selected_construct_names(model, constructs)
  model$constructs[vapply(model$constructs, function(construct) {
    construct$name %in% names
  }, logical(1))]
}

selected_construct_names <- function(model, constructs) {
  lower_names <- vapply(model$constructs, `[[`, character(1), "name")
  if (is.null(constructs)) {
    return(lower_names)
  }
  constructs <- validate_name_vector(constructs, "constructs")
  unknown <- setdiff(constructs, lower_names)
  if (length(unknown) > 0L) {
    stop("Unknown constructs: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  constructs
}

validate_positive_whole_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1L ||
      x != as.integer(x)) {
    stop(sprintf("`%s` must be a positive whole number.", name),
         call. = FALSE)
  }
  as.integer(x)
}

validate_noise_sd <- function(noise_sd) {
  if (!is.numeric(noise_sd) || length(noise_sd) != 1L || is.na(noise_sd) ||
      noise_sd <= 0) {
    stop("`noise_sd` must be a single positive number.", call. = FALSE)
  }
  noise_sd
}

validate_replacement_pool <- function(replacement_pool, targets, data) {
  if (is.null(replacement_pool)) {
    stop("`replacement_pool` is required for replacement scenarios.",
         call. = FALSE)
  }

  target_names <- vapply(targets, `[[`, character(1), "name")
  if (is.character(replacement_pool)) {
    pool <- rep(list(validate_name_vector(replacement_pool, "replacement_pool")),
                length(target_names))
    names(pool) <- target_names
  } else if (is.list(replacement_pool) && !is.null(names(replacement_pool))) {
    missing_pools <- setdiff(target_names, names(replacement_pool))
    if (length(missing_pools) > 0L) {
      stop(
        "`replacement_pool` is missing entries for constructs: ",
        paste(missing_pools, collapse = ", "),
        call. = FALSE
      )
    }
    pool <- replacement_pool[target_names]
    pool <- lapply(pool, validate_name_vector, name = "replacement_pool")
  } else {
    stop(
      "`replacement_pool` must be a character vector or a named list.",
      call. = FALSE
    )
  }

  if (!is.null(data)) {
    missing_candidates <- setdiff(unique(unlist(pool, use.names = FALSE)),
                                  names(data))
    if (length(missing_candidates) > 0L) {
      stop(
        "`data` is missing replacement indicators: ",
        paste(missing_candidates, collapse = ", "),
        call. = FALSE
      )
    }
  }
  pool
}

removal_candidates <- function(targets, n_remove, exact) {
  rows <- list()
  for (construct in targets) {
    max_remove <- length(construct$indicators) - 1L
    if (max_remove < 1L) {
      next
    }
    sizes <- if (exact) n_remove else seq_len(min(n_remove, max_remove))
    sizes <- sizes[sizes <= max_remove]
    for (size in sizes) {
      combinations <- utils::combn(construct$indicators, size, simplify = FALSE)
      for (removed in combinations) {
        rows[[length(rows) + 1L]] <- list(
          construct = construct$name,
          removed_indicators = removed
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      construct = character(),
      removed_indicators = I(list()),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    construct = vapply(rows, `[[`, character(1), "construct"),
    removed_indicators = I(lapply(rows, `[[`, "removed_indicators")),
    stringsAsFactors = FALSE
  )
}

make_perturbation_scenario <- function(scenario_id, method, construct,
                                       removed_indicators = character(),
                                       added_indicators = character(),
                                       replaced_indicators = character(),
                                       noise_spec = NULL, description = "",
                                       valid = TRUE,
                                       reason_if_invalid = NA_character_,
                                       data = NULL) {
  list(
    scenario_id = scenario_id,
    method = method,
    construct = construct,
    removed_indicators = removed_indicators,
    added_indicators = added_indicators,
    replaced_indicators = replaced_indicators,
    noise_spec = noise_spec,
    description = description,
    valid = valid,
    reason_if_invalid = reason_if_invalid,
    data = data
  )
}

scenario_rows_to_data_frame <- function(rows) {
  if (length(rows) == 0L) {
    return(empty_perturbation_scenarios())
  }
  data.frame(
    scenario_id = vapply(rows, `[[`, character(1), "scenario_id"),
    method = vapply(rows, `[[`, character(1), "method"),
    construct = vapply(rows, `[[`, character(1), "construct"),
    removed_indicators = I(lapply(rows, `[[`, "removed_indicators")),
    added_indicators = I(lapply(rows, `[[`, "added_indicators")),
    replaced_indicators = I(lapply(rows, `[[`, "replaced_indicators")),
    noise_spec = I(lapply(rows, `[[`, "noise_spec")),
    description = vapply(rows, `[[`, character(1), "description"),
    valid = vapply(rows, `[[`, logical(1), "valid"),
    reason_if_invalid = vapply(rows, `[[`, character(1), "reason_if_invalid"),
    data = I(lapply(rows, `[[`, "data")),
    stringsAsFactors = FALSE
  )
}

empty_perturbation_scenarios <- function() {
  data.frame(
    scenario_id = character(),
    method = character(),
    construct = character(),
    removed_indicators = I(list()),
    added_indicators = I(list()),
    replaced_indicators = I(list()),
    noise_spec = I(list()),
    description = character(),
    valid = logical(),
    reason_if_invalid = character(),
    data = I(list()),
    stringsAsFactors = FALSE
  )
}

ensure_perturbation_columns <- function(scenarios) {
  required <- names(empty_perturbation_scenarios())
  for (column in setdiff(required, names(scenarios))) {
    if (column %in% c(
      "removed_indicators",
      "added_indicators",
      "replaced_indicators",
      "noise_spec",
      "data"
    )) {
      scenarios[[column]] <- I(rep(list(NULL), nrow(scenarios)))
    } else if (column == "valid") {
      scenarios[[column]] <- rep(TRUE, nrow(scenarios))
    } else if (column == "reason_if_invalid") {
      scenarios[[column]] <- rep(NA_character_, nrow(scenarios))
    } else {
      scenarios[[column]] <- rep("", nrow(scenarios))
    }
  }
  scenarios[, required, drop = FALSE]
}

rbind_perturbation_scenarios <- function(...) {
  scenarios <- list(...)
  scenarios <- lapply(scenarios, ensure_perturbation_columns)
  if (length(scenarios) == 0L) {
    return(empty_perturbation_scenarios())
  }
  out <- do.call(rbind, scenarios)
  row.names(out) <- NULL
  out
}

make_scenario_id <- function(...) {
  pieces <- c(...)
  pieces <- gsub("[^A-Za-z0-9]+", "_", pieces)
  pieces <- gsub("^_+|_+$", "", pieces)
  tolower(paste(pieces[nzchar(pieces)], collapse = "__"))
}
