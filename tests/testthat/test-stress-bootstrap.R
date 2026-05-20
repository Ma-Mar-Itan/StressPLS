test_that("stress_bootstrap creates reproducible bootstrap scenarios", {
  a <- stress_bootstrap(example_data(), example_model(), R = 3, seed = 123)
  b <- stress_bootstrap(example_data(), example_model(), R = 3, seed = 123)

  expect_s3_class(a, "stresspls_grid")
  expect_equal(a$scenarios, b$scenarios)
  expect_equal(nrow(a$scenarios), 3)
})

test_that("stress_bootstrap validates R", {
  expect_error(
    stress_bootstrap(example_data(), example_model(), R = 0),
    "`R` must be a positive whole number",
    fixed = TRUE
  )
})
