test_that("existing backend objects remain stable after validation", {
  backend <- as_backend(toy_backend, name = "toy")
  same_backend <- as_backend(backend)

  expect_s3_class(same_backend, "stresspls_backend")
  expect_identical(same_backend$name, "toy")
})

test_that("standardized backend output fills missing columns for all rows", {
  fit <- as_stresspls_fit(list(
    paths = data.frame(
      from = c("A", "B"),
      to = c("C", "D"),
      estimate = c(0.1, 0.2)
    )
  ))

  expect_equal(nrow(fit$paths), 2)
  expect_true(all(is.na(fit$paths$std_error)))
  expect_true(all(is.na(fit$paths$significant)))
})

test_that("malformed and empty prediction outputs are handled consistently", {
  no_pred_backend <- function(model, data, scenario = NULL, ...) list()
  pred <- repeated_cv_predict(
    toy_model(),
    toy_data(),
    no_pred_backend,
    outcomes = "sat1",
    v = 2,
    continue_on_error = TRUE
  )

  summary <- compare_prediction_metrics(pred)
  expect_named(summary, c("outcome", "metric", "mean", "sd", "n"))
  expect_equal(nrow(summary), 0)
  expect_s3_class(make_prediction_table(pred), "data.frame")
  expect_s3_class(plot_prediction_comparison(pred), "ggplot")
  expect_error(compare_prediction_metrics(data.frame(value = 1)),
               "`outcome`, `metric`, and `value`", fixed = TRUE)
})

test_that("collinearity summaries tolerate missing backend VIFs and paths", {
  thin_backend <- function(model, data, scenario = NULL, ...) list()
  stress <- collinearity_stress_test(
    toy_model(),
    toy_data(),
    thin_backend,
    indicators = c("img1", "img2"),
    levels = c(0, 0.2)
  )

  summary <- summarise_collinearity_stress(stress)
  expect_equal(nrow(summary), 2)
  expect_true(all(is.na(summary$max_vif)))
  expect_true(all(is.na(summary$mean_path_abs_change)))
  expect_s3_class(plot_vif_stress_curve(stress), "ggplot")
})

test_that("table and plot helpers reject malformed path tables clearly", {
  expect_error(plot_path_distribution(data.frame(estimate = 0.1)),
               "Path estimates must contain", fixed = TRUE)
  expect_s3_class(make_baseline_table(as_stresspls_fit(list())), "data.frame")
})

test_that("sensitivity reports expose stable sections for empty inputs", {
  report <- sensitivity_report(limitations = "Audit fixture.")

  expect_s3_class(report, "stresspls_sensitivity_report")
  expect_named(
    report,
    c("baseline", "perturbations", "bootstrap", "collinearity", "prediction",
      "heterogeneity", "simulation", "warnings", "limitations", "created_at")
  )
})
