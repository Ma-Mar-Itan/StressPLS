test_that("stress_indicators creates deletion scenarios", {
  grid <- stress_indicators(example_data(), example_model())

  expect_s3_class(grid, "stresspls_grid")
  expect_equal(nrow(grid$scenarios), 3)
  expect_equal(unique(grid$scenarios$perturbation), "indicator_deletion")
})

test_that("stress_indicators validates inputs", {
  expect_error(
    stress_indicators(list(), example_model()),
    "`data` must be a data frame",
    fixed = TRUE
  )
  expect_error(
    stress_indicators(example_data(), list(indicators = "missing")),
    "missing: missing",
    fixed = TRUE
  )
})

test_that("stress_indicators creates swap scenarios", {
  swaps <- data.frame(from = "x1", to = "x2")
  grid <- stress_indicators(example_data(), example_model(), swaps = swaps)

  expect_true("indicator_swap" %in% grid$scenarios$perturbation)
})
