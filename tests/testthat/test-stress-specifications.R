test_that("stress_specifications creates specification scenarios", {
  specs <- list(one = list(type = "repeated"), two = list(type = "two_stage"))
  grid <- stress_specifications(specs)

  expect_s3_class(grid, "stresspls_grid")
  expect_equal(nrow(grid$scenarios), 2)
  expect_equal(grid$scenarios$specification_name, c("one", "two"))
})

test_that("stress_specifications validates inputs", {
  expect_error(
    stress_specifications(list()),
    "`specifications` must be a non-empty list",
    fixed = TRUE
  )
})
