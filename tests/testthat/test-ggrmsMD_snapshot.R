# Only run these tests if vdiffr is available (prevents failures on CRAN/CI)
if (requireNamespace("vdiffr", quietly = TRUE)) {

  library(testthat)
  library(rms)
  library(ggplot2)
  library(vdiffr)

  # Helper: consistent setup of datadist
  setup_datadist <- function(data) {
    dd <- datadist(data)
    assign("dd", dd, envir = .GlobalEnv)
    options(datadist = "dd")
  }

  # Basic OLS spline plot
  test_that("OLS spline plot is visually stable", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data)
    vdiffr::expect_doppelganger(
      "OLS spline on age (default options)",
      p
    )
  })

  # OLS spline with custom axis limits
  test_that("OLS spline with custom limits", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, ylim = c(0, 60), xlims = list(age = c(20, 80)))
    vdiffr::expect_doppelganger(
      "OLS spline with custom axis limits",
      p
    )
  })

  # Logistic regression: odds ratio plot (default, with noeffline)
  test_that("Logistic spline (odds ratio)", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
    setup_datadist(data)
    fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data)
    vdiffr::expect_doppelganger(
      "Logistic spline on age (odds ratio)",
      p
    )
  })

  # Logistic regression: predicted probability (lrm_prob)
  test_that("Logistic spline (predicted probability)", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
    setup_datadist(data)
    fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, lrm_prob = TRUE)
    vdiffr::expect_doppelganger(
      "Logistic spline on age (predicted probability)",
      p
    )
  })

  # Cox model
  test_that("Cox spline plot", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::cph(Surv(lengthstay, majorcomplication) ~ rcs(age, 4), data = data, x = TRUE, y = TRUE)
    p <- ggrmsMD(fit, data)
    vdiffr::expect_doppelganger(
      "Cox spline on age (hazard ratio)",
      p
    )
  })

  # Test shade_inferior = 'higher'
  test_that("Logistic spline with shade_inferior = 'higher'", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
    setup_datadist(data)
    fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, shade_inferior = "higher")
    vdiffr::expect_doppelganger(
      "Logistic spline with inferior-shading-higher",
      p
    )
  })

  # Test shade_inferior = 'lower'
  test_that("Logistic spline with shade_inferior = 'lower'", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
    setup_datadist(data)
    fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, shade_inferior = "lower")
    vdiffr::expect_doppelganger(
      "Logistic spline with inferior-shading-lower",
      p
    )
  })

  # Log y axis
  test_that("OLS spline with log y-axis", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, log_y = TRUE)
    vdiffr::expect_doppelganger(
      "OLS spline log y-axis",
      p
    )
  })

  # Log x axis
  test_that("OLS spline with log x-axis", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    data$logage <- data$age + 30 # ensure positive
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(logage, 4), data = data)
    p <- ggrmsMD(fit, data, log_x_vars = "logage")
    vdiffr::expect_doppelganger(
      "OLS spline log x-axis",
      p
    )
  })

  # Combined multi-panel plot
  test_that("Combined plot (multi-panel)", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4) + rcs(bmi, 4), data = data)
    p <- ggrmsMD(fit, data, combined = TRUE)
    vdiffr::expect_doppelganger(
      "Combined multi-panel spline plot",
      p
    )
  })

  # Custom axis labels/titles
  test_that("Custom x/y labels and plot titles", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
    xlabs <- list(age = "Patient Age (years)")
    titles <- list(age = "Spline of Age")
    p <- ggrmsMD(fit, data, xlabs = xlabs, ylab = "Custom Y", titles = titles)
    vdiffr::expect_doppelganger(
      "Custom x y labels and titles",
      p
    )
  })

  # Custom number of prediction points (np)
  test_that("Custom np for prediction points", {
    skip_on_cran()
    data <- simulated_rmsMD_data()
    setup_datadist(data)
    fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
    p <- ggrmsMD(fit, data, np = 10)
    vdiffr::expect_doppelganger(
      "OLS spline np = 10",
      p
    )
  })

}
