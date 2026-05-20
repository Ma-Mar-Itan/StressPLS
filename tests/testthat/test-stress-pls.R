test_that("stress_pls creates not_estimated results without backend", {
  grid <- stress_indicators(example_data(), example_model())
  result <- stress_pls(example_data(), example_model(), grid = grid)

  expect_s3_class(result, "stresspls_result")
  expect_equal(unique(result$results$status), "not_estimated")
})

test_that("stress_pls accepts modular backend functions", {
  backend <- function(data, model, scenario, seed) {
    data.frame(
      scenario_id = scenario$scenario_id,
      fragility_score = 0.5,
      stringsAsFactors = FALSE
    )
  }
  result <- stress_pls(
    example_data(),
    example_model(),
    grid = stress_bootstrap(example_data(), example_model(), R = 2, seed = 1),
    backend = backend,
    seed = 1
  )

  expect_equal(result$results$status, c("estimated", "estimated"))
  expect_equal(result$results$fragility_score, c(0.5, 0.5))
})

test_that("stress_pls validates backend return values", {
  bad_backend <- function(data, model, scenario, seed) list()

  expect_error(
    stress_pls(
      example_data(),
      example_model(),
      grid = stress_bootstrap(example_data(), example_model(), R = 1),
      backend = bad_backend
    ),
    "`backend` must return a data frame",
    fixed = TRUE
  )
})
