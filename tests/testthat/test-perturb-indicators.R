perturbation_model <- function(single = FALSE) {
  image <- specify_construct(
    "Image",
    if (single) "img1" else c("img1", "img2", "img3")
  )
  quality <- specify_construct("Quality", c("qual1", "qual2", "qual3"))
  satisfaction <- specify_construct("Satisfaction", c("sat1", "sat2"))
  brand_equity <- specify_hoc("BrandEquity", c("Image", "Quality"))
  paths <- specify_paths(from = "BrandEquity", to = "Satisfaction")
  specify_model(
    constructs = list(image, quality, satisfaction),
    hocs = list(brand_equity),
    paths = paths
  )
}

perturbation_data <- function() {
  data.frame(
    img1 = 1:5,
    img2 = 2:6,
    img3 = 3:7,
    qual1 = 4:8,
    qual2 = 5:9,
    qual3 = 6:10,
    sat1 = 7:11,
    sat2 = 8:12,
    alt1 = 9:13,
    alt2 = 10:14
  )
}

test_that("leave-one-indicator-out scenarios are generated correctly", {
  grid <- leave_one_indicator_out(
    perturbation_model(),
    constructs = c("Image", "Quality")
  )

  expect_s3_class(grid, "stresspls_perturbation_grid")
  expect_equal(nrow(grid$scenarios), 6)
  expect_equal(unique(grid$scenarios$method), "leave_one_out")
  expect_equal(
    unlist(grid$scenarios$removed_indicators[grid$scenarios$construct == "Image"]),
    c("img1", "img2", "img3")
  )
})

test_that("leave-one-indicator-out does not remove the final indicator", {
  grid <- leave_one_indicator_out(perturbation_model(single = TRUE),
                                  constructs = "Image")

  expect_equal(nrow(grid$scenarios), 0)
})

test_that("random indicator removal is reproducible with fixed seed", {
  first <- random_indicator_removal(
    perturbation_model(),
    constructs = "Image",
    n_remove = 1,
    n_scenarios = 3,
    seed = 42
  )
  second <- random_indicator_removal(
    perturbation_model(),
    constructs = "Image",
    n_remove = 1,
    n_scenarios = 3,
    seed = 42
  )

  expect_equal(first$scenarios, second$scenarios)
})

test_that("random indicator removal changes with different seeds", {
  first <- random_indicator_removal(
    perturbation_model(),
    constructs = c("Image", "Quality"),
    n_remove = 1,
    n_scenarios = 4,
    seed = 1
  )
  second <- random_indicator_removal(
    perturbation_model(),
    constructs = c("Image", "Quality"),
    n_remove = 1,
    n_scenarios = 4,
    seed = 2
  )

  expect_false(identical(first$scenarios$removed_indicators,
                         second$scenarios$removed_indicators))
})

test_that("combinatorial deletion generates expected scenarios", {
  grid <- combinatorial_indicator_deletion(
    perturbation_model(),
    constructs = "Image",
    n_remove = 2
  )

  expect_equal(nrow(grid$scenarios), 6)
  expect_equal(unique(lengths(grid$scenarios$removed_indicators)), c(1L, 2L))
})

test_that("combinatorial deletion rejects invalid deletion sizes", {
  expect_error(
    combinatorial_indicator_deletion(perturbation_model(), n_remove = 0),
    "`n_remove` must be a positive whole number",
    fixed = TRUE
  )
})

test_that("indicator replacement requires a replacement pool", {
  expect_error(
    indicator_replacement(perturbation_model(), constructs = "Image"),
    "`replacement_pool` is required",
    fixed = TRUE
  )
})

test_that("indicator replacement creates replacement metadata", {
  grid <- indicator_replacement(
    perturbation_model(),
    constructs = "Image",
    replacement_pool = c("alt1", "alt2")
  )

  expect_equal(nrow(grid$scenarios), 6)
  expect_equal(grid$scenarios$added_indicators[[1]], "alt1")
  expect_equal(grid$scenarios$replaced_indicators[[1]], c(img1 = "alt1"))
})

test_that("indicator replacement rejects candidates already in construct", {
  expect_error(
    indicator_replacement(
      perturbation_model(),
      constructs = "Image",
      replacement_pool = c("img1", "alt1")
    ),
    "already used in construct `Image`",
    fixed = TRUE
  )
})

test_that("noise injection requires positive numeric noise_sd", {
  expect_error(
    noise_injection(perturbation_model(), noise_sd = 0),
    "`noise_sd` must be a single positive number",
    fixed = TRUE
  )
  expect_error(
    noise_injection(perturbation_model(), noise_sd = "small"),
    "`noise_sd` must be a single positive number",
    fixed = TRUE
  )
})

test_that("noise injection creates noise metadata", {
  grid <- noise_injection(
    perturbation_model(),
    constructs = "Image",
    noise_sd = 0.25,
    seed = 123
  )

  expect_equal(nrow(grid$scenarios), 3)
  expect_equal(grid$scenarios$noise_spec[[1]]$indicator, "img1")
  expect_equal(grid$scenarios$noise_spec[[1]]$sd, 0.25)
  expect_true(is.numeric(grid$scenarios$noise_spec[[1]]$seed))
})

test_that("noise injection can materialize transformed data", {
  grid <- noise_injection(
    perturbation_model(),
    data = perturbation_data(),
    constructs = "Image",
    noise_sd = 0.1,
    seed = 123
  )

  expect_true(grid$metadata$data_materialized)
  expect_s3_class(grid$scenarios$data[[1]], "data.frame")
  expect_false(identical(grid$scenarios$data[[1]]$img1,
                         perturbation_data()$img1))
})

test_that("unknown construct names are rejected", {
  expect_error(
    leave_one_indicator_out(perturbation_model(), constructs = "Missing"),
    "Unknown constructs: Missing",
    fixed = TRUE
  )
})

test_that("scenario IDs are unique", {
  grid <- perturb_indicators(
    perturbation_model(),
    method = c("leave_one_out", "combinatorial_deletion"),
    constructs = "Image",
    n_remove = 2
  )

  expect_equal(anyDuplicated(grid$scenarios$scenario_id), 0)
})

test_that("perturbation grid print method does not error", {
  grid <- leave_one_indicator_out(perturbation_model(), constructs = "Image")

  expect_snapshot(print(grid))
})

test_that("perturb_indicators dispatches to each method", {
  model <- perturbation_model()

  loo <- perturb_indicators(model, method = "leave_one_out", constructs = "Image")
  random <- perturb_indicators(
    model,
    method = "random_removal",
    constructs = "Image",
    n_scenarios = 2,
    seed = 1
  )
  combo <- perturb_indicators(
    model,
    method = "combinatorial_deletion",
    constructs = "Image",
    n_remove = 2
  )
  replacement <- perturb_indicators(
    model,
    method = "replacement",
    constructs = "Image",
    replacement_pool = "alt1"
  )
  noise <- perturb_indicators(
    model,
    method = "noise_injection",
    constructs = "Image",
    noise_sd = 0.1,
    seed = 1
  )

  expect_equal(unique(loo$scenarios$method), "leave_one_out")
  expect_equal(unique(random$scenarios$method), "random_removal")
  expect_equal(unique(combo$scenarios$method), "combinatorial_deletion")
  expect_equal(unique(replacement$scenarios$method), "replacement")
  expect_equal(unique(noise$scenarios$method), "noise_injection")
})
