#' Simulate formative higher-order construct data
#'
#' @param n Sample size.
#' @param indicators_per_construct Number of indicators per formative dimension.
#' @param weight_strength One of `"weak"`, `"moderate"`, or `"strong"`.
#' @param collinearity One of `"low"`, `"moderate"`, `"high"`, or `"severe"`.
#' @param structural_path_strength One of `"weak"`, `"moderate"`, or
#'   `"strong"`.
#' @param measurement_noise One of `"low"`, `"moderate"`, or `"high"`.
#' @param misspecification Misspecification type.
#' @param heterogeneity Heterogeneity level.
#' @param seed Optional random seed.
#'
#' @return A `stresspls_simulated_data` object containing data, model, and true
#'   parameters.
#' @examples
#' simulate_formative_hoc_data(n = 20, indicators_per_construct = 3, seed = 1)
#' @export
simulate_formative_hoc_data <- function(
    n = 100,
    indicators_per_construct = 3,
    weight_strength = c("weak", "moderate", "strong"),
    collinearity = c("low", "moderate", "high", "severe"),
    structural_path_strength = c("weak", "moderate", "strong"),
    measurement_noise = c("low", "moderate", "high"),
    misspecification = c(
      "none",
      "irrelevant_indicator",
      "contaminated_indicator",
      "omitted_key_indicator"
    ),
    heterogeneity = c("none", "mild", "strong"),
    seed = NULL) {
  n <- validate_positive_whole_number(n, "n")
  indicators_per_construct <- validate_positive_whole_number(
    indicators_per_construct,
    "indicators_per_construct"
  )
  weight_strength <- match.arg(weight_strength)
  collinearity <- match.arg(collinearity)
  structural_path_strength <- match.arg(structural_path_strength)
  measurement_noise <- match.arg(measurement_noise)
  misspecification <- match.arg(misspecification)
  heterogeneity <- match.arg(heterogeneity)
  validate_seed(seed)

  pars <- list(
    weight = c(weak = 0.25, moderate = 0.50, strong = 0.80)[[weight_strength]],
    rho = c(low = 0.10, moderate = 0.35, high = 0.60, severe = 0.80)[[collinearity]],
    path = c(weak = 0.20, moderate = 0.45, strong = 0.70)[[structural_path_strength]],
    noise = c(low = 0.25, moderate = 0.60, high = 1.00)[[measurement_noise]]
  )

  sim <- with_seed(seed, {
    common <- stats::rnorm(n)
    image_latent <- pars$rho * common + sqrt(1 - pars$rho^2) * stats::rnorm(n)
    quality_latent <- pars$rho * common + sqrt(1 - pars$rho^2) * stats::rnorm(n)
    make_indicators <- function(prefix, latent) {
      out <- replicate(indicators_per_construct,
                       pars$weight * latent + pars$noise * stats::rnorm(n))
      colnames(out) <- paste0(prefix, seq_len(indicators_per_construct))
      as.data.frame(out)
    }
    image <- make_indicators("img", image_latent)
    quality <- make_indicators("qual", quality_latent)
    hoc <- rowMeans(cbind(image, quality))
    path <- pars$path
    if (heterogeneity == "mild") path <- path + rep(c(-0.1, 0.1), length.out = n)
    if (heterogeneity == "strong") path <- path + rep(c(-0.25, 0.25), length.out = n)
    satisfaction_latent <- path * hoc + pars$noise * stats::rnorm(n)
    sat <- make_indicators("sat", satisfaction_latent)
    dat <- cbind(image, quality, sat)
    if (misspecification == "irrelevant_indicator") {
      dat$irrelevant1 <- stats::rnorm(n)
    }
    if (misspecification == "contaminated_indicator") {
      dat$img1 <- dat$img1 + stats::rnorm(n, sd = pars$noise * 2)
    }
    if (misspecification == "omitted_key_indicator") {
      dat[[paste0("qual", indicators_per_construct)]] <- NULL
    }
    dat
  })

  image_names <- grep("^img", names(sim), value = TRUE)
  quality_names <- grep("^qual", names(sim), value = TRUE)
  sat_names <- grep("^sat", names(sim), value = TRUE)
  image <- specify_construct("Image", image_names)
  quality <- specify_construct("Quality", quality_names)
  satisfaction <- specify_construct("Satisfaction", sat_names,
                                    mode = "reflective")
  brand <- specify_hoc("BrandEquity", c("Image", "Quality"))
  paths <- specify_paths(from = "BrandEquity", to = "Satisfaction")
  model <- specify_model(list(image, quality, satisfaction), list(brand), paths)

  structure(
    list(
      data = sim,
      model = model,
      true_parameters = data.frame(
        parameter = c("weight", "path", "collinearity", "noise"),
        value = c(pars$weight, pars$path, pars$rho, pars$noise),
        stringsAsFactors = FALSE
      ),
      factors = list(
        n = n,
        indicators_per_construct = indicators_per_construct,
        weight_strength = weight_strength,
        collinearity = collinearity,
        structural_path_strength = structural_path_strength,
        measurement_noise = measurement_noise,
        misspecification = misspecification,
        heterogeneity = heterogeneity,
        seed = seed
      )
    ),
    class = "stresspls_simulated_data"
  )
}

#' Run a simulation design grid
#'
#' @param design Optional data frame of simulation factors.
#' @param backend Backend function or object.
#' @param replications Number of replications per design row.
#' @param seed Optional random seed.
#' @param continue_on_error Store replication errors instead of stopping.
#' @param ... Extra arguments passed to `simulate_formative_hoc_data()`.
#'
#' @return A `stresspls_simulation` object.
#' @examples
#' toy <- function(model, data, scenario = NULL, ...) {
#'   list(paths = data.frame(from = "BrandEquity", to = "Satisfaction",
#'                           estimate = 0.4))
#' }
#' run_simulation_grid(data.frame(n = 20), toy, replications = 1, seed = 1)
#' @export
run_simulation_grid <- function(design = NULL, backend, replications = 1,
                                seed = NULL, continue_on_error = TRUE, ...) {
  backend <- as_backend(backend)
  replications <- validate_positive_whole_number(replications, "replications")
  validate_seed(seed)
  if (is.null(design)) {
    design <- data.frame(n = 100, indicators_per_construct = 3,
                         stringsAsFactors = FALSE)
  }
  if (!is.data.frame(design) || nrow(design) == 0L) {
    stop("`design` must be a non-empty data frame.", call. = FALSE)
  }
  seeds <- with_seed(seed, sample.int(.Machine$integer.max,
                                      nrow(design) * replications))
  rows <- list()
  fits <- list()
  k <- 1L
  for (i in seq_len(nrow(design))) {
    for (r in seq_len(replications)) {
      args <- c(as.list(design[i, , drop = FALSE]), list(seed = seeds[[k]]), list(...))
      sim <- do.call(simulate_formative_hoc_data, args)
      fit <- tryCatch(
        fit_baseline_model(sim$model, sim$data, backend),
        error = function(e) {
          if (!continue_on_error) stop(e)
          error_fit(sim$model, sim$data, NULL, backend, conditionMessage(e))
        }
      )
      fits[[k]] <- fit
      paths <- extract_paths(fit)
      estimate <- paths$estimate[paths$from == "BrandEquity" &
                                   paths$to == "Satisfaction"][1]
      true_path <- sim$true_parameters$value[sim$true_parameters$parameter == "path"]
      rows[[k]] <- data.frame(
        design_id = i,
        replication = r,
        seed = seeds[[k]],
        estimate = estimate,
        true_value = true_path,
        bias = estimate - true_path,
        squared_error = (estimate - true_path)^2,
        converged = isTRUE(fit$diagnostics$converged),
        stringsAsFactors = FALSE
      )
      rows[[k]] <- cbind(rows[[k]], design[i, , drop = FALSE])
      k <- k + 1L
    }
  }
  structure(
    list(design = design, results = rbind_list(rows), fits = fits,
         backend = backend, seed = seed),
    class = "stresspls_simulation"
  )
}

#' Summarise simulation results
#'
#' @param x A `stresspls_simulation` object.
#'
#' @return A tidy data frame of simulation performance.
#' @examples
#' # See `run_simulation_grid()` for construction.
#' @export
summarise_simulation_results <- function(x) {
  if (!inherits(x, "stresspls_simulation")) {
    stop("`x` must be a stresspls_simulation object.", call. = FALSE)
  }
  if (nrow(x$results) == 0L) return(data.frame())
  out <- lapply(split(x$results, x$results$design_id), function(rows) {
    data.frame(
      design_id = rows$design_id[[1]],
      replications = nrow(rows),
      mean_bias = mean(rows$bias, na.rm = TRUE),
      rmse = sqrt(mean(rows$squared_error, na.rm = TRUE)),
      sign_consistency = calc_sign_consistency(rows$estimate),
      direction_flip_rate = mean(sign(rows$estimate) != sign(rows$true_value),
                                 na.rm = TRUE),
      ci_width = calc_ci_width(rows$estimate),
      stability_index = calc_stability_index(rows$estimate),
      convergence_rate = mean(rows$converged),
      error_rate = mean(!rows$converged),
      stringsAsFactors = FALSE
    )
  })
  rbind_list(out)
}
