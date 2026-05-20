test_that("table, plot, and report helpers return documented objects", {
  model <- toy_model()
  data <- toy_data()
  baseline <- fit_baseline_model(model, data, toy_backend)
  grid <- leave_one_indicator_out(model, constructs = "Image")
  fit_grid <- fit_perturbation_grid(model, data, grid, toy_backend)
  boot <- bootstrap_stability(model, data, toy_backend, R = 2, seed = 1)
  coll <- collinearity_stress_test(model, data, toy_backend,
                                   indicators = c("img1", "img2"),
                                   levels = c(0, 0.2))
  pred <- repeated_cv_predict(model, data, toy_backend, outcomes = "sat1",
                              v = 2, repeats = 1)
  het <- subgroup_heterogeneity(model, data, toy_backend, group = "group",
                                min_n = 2)
  sim <- run_simulation_grid(data.frame(n = 20), toy_backend, replications = 1)

  expect_s3_class(make_baseline_table(baseline), "data.frame")
  expect_s3_class(make_indicator_perturbation_table(fit_grid), "data.frame")
  expect_s3_class(make_bootstrap_stability_table(boot), "data.frame")
  expect_s3_class(make_collinearity_table(coll), "data.frame")
  expect_s3_class(make_prediction_table(pred), "data.frame")
  expect_s3_class(make_heterogeneity_table(het), "data.frame")
  expect_s3_class(make_simulation_design_table(sim), "data.frame")
  expect_s3_class(make_simulation_results_table(sim), "data.frame")

  expect_s3_class(plot_stress_workflow(), "ggplot")
  expect_s3_class(plot_indicator_stability_heatmap(fit_grid), "ggplot")
  expect_s3_class(plot_path_distribution(fit_grid), "ggplot")
  expect_s3_class(plot_vif_stress_curve(coll), "ggplot")
  expect_s3_class(plot_prediction_comparison(pred), "ggplot")
  expect_s3_class(plot_robustness_dashboard(fit_grid), "ggplot")
  expect_s3_class(plot_simulation_results(sim), "ggplot")

  report <- sensitivity_report(
    baseline = baseline,
    perturbations = fit_grid,
    bootstrap = boot,
    collinearity = coll,
    prediction = pred,
    heterogeneity = het,
    simulation = sim,
    limitations = "Toy backend only."
  )
  expect_s3_class(report, "stresspls_sensitivity_report")
  expect_output(print(report), "stresspls_sensitivity_report")
})

test_that("optional adapter skeletons are stable without optional packages", {
  expect_s3_class(backend_from_seminr(), "stresspls_backend")
  expect_s3_class(backend_from_csem(), "stresspls_backend")
  expect_s3_class(backend_from_plspm(), "stresspls_backend")
  expect_s3_class(backend_from_smartpls_export(), "stresspls_backend")
})
