test_that("specify_construct creates valid construct specifications", {
  construct <- specify_construct(
    name = "Image",
    indicators = c("img1", "img2"),
    mode = "formative",
    description = "Brand image"
  )

  expect_s3_class(construct, "stresspls_construct_spec")
  expect_equal(construct$name, "Image")
  expect_equal(construct$indicators, c("img1", "img2"))
  expect_equal(construct$mode, "formative")
})

test_that("specify_construct rejects invalid construct names", {
  expect_error(
    specify_construct(name = "", indicators = "img1"),
    "`name` must be a single non-empty string",
    fixed = TRUE
  )
  expect_error(
    specify_construct(name = "Bad Name", indicators = "img1"),
    "`name` must not contain whitespace",
    fixed = TRUE
  )
})

test_that("specify_construct rejects empty indicator vectors", {
  expect_error(
    specify_construct(name = "Image", indicators = character()),
    "`indicators` must contain at least one indicator",
    fixed = TRUE
  )
})

test_that("specify_model rejects duplicate construct names", {
  image_a <- specify_construct("Image", "img1")
  image_b <- specify_construct("Image", "img2")

  expect_error(
    specify_model(list(image_a, image_b)),
    "Construct names must be unique",
    fixed = TRUE
  )
})

test_that("specify_hoc creates valid HOC specifications", {
  hoc <- specify_hoc(
    name = "BrandEquity",
    dimensions = c("Image", "Quality"),
    mode = "formative",
    approach = "two_stage"
  )

  expect_s3_class(hoc, "stresspls_hoc_spec")
  expect_equal(hoc$dimensions, c("Image", "Quality"))
  expect_equal(hoc$approach, "two_stage")
})

test_that("validate_model_spec rejects HOCs with missing lower-order constructs", {
  image <- specify_construct("Image", "img1")
  hoc <- specify_hoc("BrandEquity", c("Image", "Quality"))

  expect_error(
    specify_model(constructs = list(image), hocs = list(hoc)),
    "references missing lower-order constructs: Quality",
    fixed = TRUE
  )
})

test_that("specify_paths creates valid path specifications", {
  from_vectors <- specify_paths(from = "BrandEquity", to = "Satisfaction")
  from_data <- specify_paths(data.frame(
    from = "BrandEquity",
    to = "Satisfaction"
  ))

  expect_s3_class(from_vectors, "stresspls_path_spec")
  expect_equal(from_vectors$paths, from_data$paths)
})

test_that("validate_model_spec rejects invalid path endpoints", {
  image <- specify_construct("Image", "img1")
  paths <- specify_paths(from = "Image", to = "Missing")

  expect_error(
    specify_model(list(image), paths = paths),
    "Structural paths reference missing constructs: Missing",
    fixed = TRUE
  )
})

test_that("validate_model_spec rejects duplicate paths", {
  image <- specify_construct("Image", "img1")
  satisfaction <- specify_construct("Satisfaction", "sat1")
  paths <- specify_paths(
    from = c("Image", "Image"),
    to = c("Satisfaction", "Satisfaction")
  )

  expect_error(
    specify_model(list(image, satisfaction), paths = paths),
    "Duplicate structural paths are not allowed",
    fixed = TRUE
  )
})

test_that("validate_model_spec detects directed path cycles", {
  image <- specify_construct("Image", "img1")
  satisfaction <- specify_construct("Satisfaction", "sat1")
  paths <- specify_paths(
    from = c("Image", "Satisfaction"),
    to = c("Satisfaction", "Image")
  )

  expect_error(
    specify_model(list(image, satisfaction), paths = paths),
    "Structural paths contain a directed cycle",
    fixed = TRUE
  )
})

test_that("required_indicators returns manifest variables", {
  image <- specify_construct("Image", c("img1", "img2"))
  quality <- specify_construct("Quality", c("qual1", "qual2"))
  hoc <- specify_hoc("BrandEquity", c("Image", "Quality"))
  model <- specify_model(list(image, quality), hocs = list(hoc))

  expect_equal(
    required_indicators(model),
    c("img1", "img2", "qual1", "qual2")
  )
})

test_that("construct_names returns lower-order and higher-order constructs", {
  image <- specify_construct("Image", "img1")
  quality <- specify_construct("Quality", "qual1")
  hoc <- specify_hoc("BrandEquity", c("Image", "Quality"))
  model <- specify_model(list(image, quality), hocs = list(hoc))

  expect_equal(construct_names(model), c("Image", "Quality", "BrandEquity"))
})

test_that("validate_model_spec succeeds when data contains indicators", {
  image <- specify_construct("Image", c("img1", "img2"))
  model <- specify_model(list(image))
  dat <- data.frame(img1 = 1:3, img2 = 2:4)

  expect_invisible(validate_model_spec(model, data = dat))
})

test_that("validate_model_spec fails when data is missing indicators", {
  image <- specify_construct("Image", c("img1", "img2"))
  model <- specify_model(list(image))
  dat <- data.frame(img1 = 1:3)

  expect_error(
    validate_model_spec(model, data = dat),
    "`data` is missing required indicators: img2",
    fixed = TRUE
  )
})

test_that("model specs can be consumed by existing scaffold functions", {
  image <- specify_construct("Image", c("img1", "img2"))
  model <- specify_model(list(image))
  dat <- data.frame(img1 = 1:3, img2 = 2:4)

  grid <- stress_indicators(dat, model)
  result <- stress_pls(dat, model, grid = grid)

  expect_equal(grid$scenarios$indicator, c("img1", "img2"))
  expect_s3_class(result, "stresspls_result")
})

test_that("model specification print methods do not error", {
  image <- specify_construct("Image", "img1")
  quality <- specify_construct("Quality", "qual1")
  hoc <- specify_hoc("BrandEquity", c("Image", "Quality"))
  paths <- specify_paths(from = "BrandEquity", to = "Quality")
  model <- specify_model(list(image, quality), hocs = list(hoc), paths = paths)

  expect_snapshot(print(image))
  expect_snapshot(print(hoc))
  expect_snapshot(print(paths))
  expect_snapshot(print(model))
})
