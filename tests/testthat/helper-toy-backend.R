toy_model <- function() {
  image <- specify_construct("Image", c("img1", "img2", "img3"))
  quality <- specify_construct("Quality", c("qual1", "qual2", "qual3"))
  satisfaction <- specify_construct("Satisfaction", c("sat1", "sat2"),
                                    mode = "reflective")
  brand <- specify_hoc("BrandEquity", c("Image", "Quality"))
  paths <- specify_paths(from = "BrandEquity", to = "Satisfaction")
  specify_model(list(image, quality, satisfaction), list(brand), paths)
}

toy_data <- function(n = 12) {
  data.frame(
    img1 = seq_len(n),
    img2 = seq_len(n) + 1,
    img3 = seq_len(n) + 2,
    qual1 = seq_len(n) + 3,
    qual2 = seq_len(n) + 4,
    qual3 = seq_len(n) + 5,
    sat1 = seq_len(n) + 6,
    sat2 = seq_len(n) + 7,
    group = rep(c("a", "b"), length.out = n)
  )
}

toy_backend <- function(model, data, scenario = NULL, ...) {
  image_score <- rowMeans(data[c("img1", "img2", "img3")], na.rm = TRUE)
  quality_score <- rowMeans(data[c("qual1", "qual2", "qual3")], na.rm = TRUE)
  brand_score <- rowMeans(cbind(image_score, quality_score), na.rm = TRUE)
  sat_score <- rowMeans(data[c("sat1", "sat2")], na.rm = TRUE)
  estimate <- as.numeric(stats::coef(stats::lm(sat_score ~ brand_score))[2])
  paths <- data.frame(
    from = "BrandEquity",
    to = "Satisfaction",
    estimate = estimate,
    std_error = 0.01,
    statistic = estimate / 0.01,
    p_value = 0.01,
    ci_low = estimate - 0.02,
    ci_high = estimate + 0.02,
    significant = TRUE
  )
  weights <- do.call(rbind, lapply(required_indicators(model), function(ind) {
    data.frame(
      construct = if (grepl("^img", ind)) "Image" else if (grepl("^qual", ind)) "Quality" else "Satisfaction",
      indicator = ind,
      estimate = mean(data[[ind]], na.rm = TRUE) / 100,
      std_error = 0.01,
      statistic = 1,
      p_value = 0.05,
      ci_low = 0,
      ci_high = 1,
      significant = TRUE
    )
  }))
  vifs <- weights[, c("construct", "indicator")]
  vifs$vif <- 1 + abs(weights$estimate)
  prediction <- data.frame(outcome = "sat1", metric = "rmse", value = 1)
  predictions <- data.frame(
    outcome = character(),
    observed = numeric(),
    predicted = numeric()
  )
  if (!is.null(scenario) && "test_data" %in% names(scenario)) {
    test <- scenario$test_data[[1]]
    predictions <- data.frame(
      outcome = "sat1",
      observed = test$sat1,
      predicted = mean(data$sat1)
    )
  }
  list(
    paths = paths,
    weights = weights,
    vifs = vifs,
    r2 = data.frame(construct = "Satisfaction", r2 = 0.5),
    prediction = prediction,
    predictions = predictions,
    diagnostics = list(converged = TRUE, warnings = character(), errors = character()),
    metadata = list(toy_backend = TRUE)
  )
}

toy_bad_backend <- function(model, data, scenario = NULL, ...) {
  stop("toy failure")
}
