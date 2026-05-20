test_that("backend abstraction validates and fits baseline", {
  backend <- as_backend(toy_backend, name = "toy")
  fit <- fit_baseline_model(toy_model(), toy_data(), backend)

  expect_s3_class(backend, "stresspls_backend")
  expect_s3_class(fit, "stresspls_fit")
  expect_silent(validate_backend(backend))
  expect_equal(nrow(extract_paths(fit)), 1)
  expect_gt(nrow(extract_weights(fit)), 0)
  expect_gt(nrow(extract_vifs(fit)), 0)
  expect_equal(nrow(extract_r2(fit)), 1)
  expect_equal(nrow(extract_prediction_metrics(fit)), 1)
})

test_that("backend output is standardized", {
  fit <- as_stresspls_fit(list(paths = data.frame(from = "A", to = "B",
                                                  estimate = 0.2)))
  expect_named(fit$paths, names(canonical_paths()))
  expect_equal(nrow(fit$weights), 0)
})

test_that("fit_perturbation_grid stores scenario errors cleanly", {
  model <- toy_model()
  data <- toy_data()
  grid <- leave_one_indicator_out(model, constructs = "Image")
  fit_grid <- fit_perturbation_grid(model, data, grid, toy_bad_backend,
                                    continue_on_error = TRUE,
                                    fit_baseline = FALSE)

  expect_s3_class(fit_grid, "stresspls_fit_grid")
  expect_false(any(fit_grid$scenario_index$converged))
  expect_true(all(grepl("toy failure", fit_grid$scenario_index$error)))
})

test_that("metric functions work on numeric estimates and fit grids", {
  expect_equal(calc_sign_consistency(c(1, 2, -1)), 2 / 3)
  expect_equal(calc_direction_flip_rate(c(1, -1), theta_hat = 1), 0.5)
  expect_gt(calc_ci_width(c(1, 2, 3)), 0)
  expect_gt(calc_scaled_ci_width(c(1, 2, 3), theta_hat = 2), 0)
  expect_gt(calc_stability_index(c(1, 2, 3)), 0)

  model <- toy_model()
  data <- toy_data()
  grid <- leave_one_indicator_out(model, constructs = "Image")
  fit_grid <- fit_perturbation_grid(model, data, grid, toy_backend)

  expect_s3_class(fit_grid, "stresspls_fit_grid")
  expect_gt(nrow(calc_path_stability(fit_grid)), 0)
  expect_gt(nrow(calc_weight_stability(fit_grid)), 0)
  expect_gt(nrow(calc_indicator_fragility(fit_grid)), 0)
  expect_gt(nrow(rank_fragility(fit_grid)), 0)
  expect_s3_class(summarise_stress(fit_grid), "stresspls_summary")
})

test_that("print methods for backend and fit objects do not error", {
  backend <- as_backend(toy_backend, name = "toy")
  fit <- fit_baseline_model(toy_model(), toy_data(), backend)
  grid <- fit_perturbation_grid(
    toy_model(),
    toy_data(),
    leave_one_indicator_out(toy_model(), constructs = "Image"),
    backend
  )

  expect_output(print(backend), "stresspls_backend")
  expect_output(print(fit), "stresspls_fit")
  expect_output(print(grid), "stresspls_fit_grid")
})
