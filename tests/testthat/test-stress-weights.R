test_that("stress_weights creates supported schemes", {
  grid <- stress_weights(c(x1 = 0.2, x2 = 0.3, x3 = 0.5), seed = 10)

  expect_s3_class(grid, "stresspls_grid")
  expect_true(all(c("equal", "jitter") %in% grid$scenarios$scheme))
})

test_that("stress_weights is reproducible with fixed seeds", {
  a <- stress_weights(c(x1 = 0.2, x2 = 0.3), seed = 99)
  b <- stress_weights(c(x1 = 0.2, x2 = 0.3), seed = 99)

  expect_equal(a$scenarios, b$scenarios)
})

test_that("stress_weights validates inputs", {
  expect_error(stress_weights(c(0.2, 0.8)), "`weights` must be a named")
  expect_error(
    stress_weights(c(x1 = 1), schemes = "unknown"),
    "Unsupported `schemes`",
    fixed = TRUE
  )
})
