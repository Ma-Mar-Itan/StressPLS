test_that("summarise_stress creates summaries", {
  result <- stress_pls(example_data(), example_model())
  summary <- summarise_stress(result)

  expect_s3_class(summary, "stresspls_summary")
  expect_equal(summary$status_counts$status, "not_estimated")
})

test_that("rank_fragility returns empty ranking without scores", {
  result <- stress_pls(example_data(), example_model())
  ranking <- rank_fragility(result)

  expect_equal(nrow(ranking), 0)
  expect_equal(attr(ranking, "note"), "No `fragility_score` column is available.")
})

test_that("rank_fragility ranks backend scores", {
  backend <- function(data, model, scenario, seed) {
    data.frame(
      scenario_id = scenario$scenario_id,
      fragility_score = if (scenario$scenario_id == "baseline") 0.1 else 0.2,
      stringsAsFactors = FALSE
    )
  }
  result <- stress_pls(example_data(), example_model(), backend = backend)

  ranking <- rank_fragility(result)
  expect_equal(ranking$rank, 1L)
})

test_that("plot_stress returns a ggplot", {
  result <- stress_pls(example_data(), example_model())

  expect_s3_class(plot_stress(result), "ggplot")
})

test_that("sensitivity_report returns text lines", {
  report <- sensitivity_report(stress_pls(example_data(), example_model()))

  expect_s3_class(report, "stresspls_report")
  expect_true(any(grepl("Scenarios:", report)))
})

test_that("printed summaries are stable", {
  expect_snapshot(print(summarise_stress(stress_pls(example_data(), example_model()))))
})
