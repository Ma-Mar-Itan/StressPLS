test_that("bootstrap stability is reproducible and summarised", {
  first <- bootstrap_stability(toy_model(), toy_data(), toy_backend, R = 3,
                               seed = 10)
  second <- bootstrap_stability(toy_model(), toy_data(), toy_backend, R = 3,
                                seed = 10)

  expect_s3_class(first, "stresspls_bootstrap")
  expect_equal(first$index$seed, second$index$seed)
  expect_gt(nrow(summarise_bootstrap_stability(first)), 0)
  expect_output(print(first), "stresspls_bootstrap")
})

test_that("collinearity stress returns recipes and summaries", {
  recipe <- inflate_collinearity(toy_data(), c("img1", "img2"), strength = 0.5)
  stress <- collinearity_stress_test(
    toy_model(),
    toy_data(),
    toy_backend,
    indicators = c("img1", "img2"),
    levels = c(0, 0.2)
  )

  expect_s3_class(recipe, "stresspls_collinearity_recipe")
  expect_s3_class(stress, "stresspls_collinearity_stress")
  expect_equal(nrow(summarise_collinearity_stress(stress)), 2)
  expect_output(print(stress), "stresspls_collinearity_stress")
})

test_that("prediction validation handles predictions and missing output", {
  pred <- repeated_cv_predict(toy_model(), toy_data(), toy_backend,
                              outcomes = "sat1", v = 3, repeats = 1,
                              seed = 1)
  expect_s3_class(pred, "stresspls_prediction_validation")
  expect_gt(nrow(compare_prediction_metrics(pred)), 0)

  no_pred_backend <- function(model, data, scenario = NULL, ...) list()
  expect_error(
    repeated_cv_predict(toy_model(), toy_data(), no_pred_backend,
                        outcomes = "sat1", v = 2,
                        continue_on_error = FALSE),
    "Backend did not return predictions",
    fixed = TRUE
  )
})

test_that("heterogeneity diagnostics validate subgroup sizes", {
  het <- subgroup_heterogeneity(toy_model(), toy_data(), toy_backend,
                                group = "group", min_n = 2)
  expect_s3_class(het, "stresspls_heterogeneity")
  expect_gt(nrow(compare_subgroup_paths(het)), 0)
  expect_gt(nrow(compare_subgroup_weights(het)), 0)
  expect_error(
    subgroup_heterogeneity(toy_model(), toy_data(), toy_backend,
                           group = "group", min_n = 100),
    "Subgroups below `min_n`",
    fixed = TRUE
  )
})

test_that("simulation utilities are reproducible", {
  sim_a <- simulate_formative_hoc_data(n = 20, seed = 123)
  sim_b <- simulate_formative_hoc_data(n = 20, seed = 123)

  expect_s3_class(sim_a, "stresspls_simulated_data")
  expect_equal(sim_a$data, sim_b$data)

  simulation <- run_simulation_grid(data.frame(n = 20), toy_backend,
                                    replications = 2, seed = 1)
  expect_s3_class(simulation, "stresspls_simulation")
  expect_equal(nrow(summarise_simulation_results(simulation)), 1)
})
