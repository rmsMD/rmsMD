library(testthat)
library(rms)

## ---- CORE INPUT CHECKS ----

# Test that ggrmsMD errors if a non-rms object is supplied
test_that("Error if non-rms object is supplied", {
        data <- simulated_rmsMD_data()
        lmfit <- lm(lengthstay ~ age + bmi, data = data)
        expect_error(
                ggrmsMD(lmfit, data),
                "modelfit is not from an rms model"
        )
})

# Test that ggrmsMD errors if there are no RCS variables in the model
test_that("Error if no RCS variables in model", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ age + bmi, data = data)
        expect_error(
                ggrmsMD(fit, data),
                "No variables specified, and no RCS variables in model"
        )
})

# Test error if variable in `var` argument is not numeric
test_that("Error if var specified is not numeric", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4) + rcs(bmi, 4), data = data)
        expect_error(
                suppressWarnings(
                        ggrmsMD(fit, data, var = "sex")
                ),
                "All variables being plotted must be numeric."
        )
})

# Test warning if variable in `var` argument is not modelled as RCS
test_that("Warning if var specified is not in model as RCS", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        expect_warning(
                try(ggrmsMD(fit, data, var = "bmi"), silent = TRUE),
                "Some selected variables were not modelled as RCS"
        )
})

## ---- BASIC OUTPUT TYPES ----

# Test that ggrmsMD returns a ggplot object for a single RCS variable
test_that("Returns ggplot for a single RCS variable (OLS)", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        p <- ggrmsMD(fit, data)
        expect_s3_class(p, "ggplot")
})

# Test that ggrmsMD returns a list of ggplots for multiple RCS variables
test_that("Returns list of ggplots for multiple RCS variables", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4) + rcs(bmi, 4), data = data)
        plots <- ggrmsMD(fit, data, combined = FALSE)
        expect_type(plots, "list")
        expect_s3_class(plots[[1]], "ggplot")
        expect_equal(length(plots), 2)
})

# Test that ggrmsMD returns cowplot::plot_grid when combined = TRUE
test_that("Returns cowplot::plot_grid when combined = TRUE", {
        skip_if_not_installed("cowplot")
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4) + rcs(bmi, 4), data = data)
        combined_plot <- ggrmsMD(fit, data, combined = TRUE)
        expect_true(any(class(combined_plot) %in% c("gg", "gtable")))
})

## ---- DIFFERENT MODEL TYPES ----

# Test that ggrmsMD works for lrm (logistic regression) models (OR)
test_that("Works for lrm models (odds ratio)", {
        data <- simulated_rmsMD_data()
        data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
        p <- ggrmsMD(fit, data)
        expect_s3_class(p, "ggplot")
})

# Test that ggrmsMD works for lrm models with lrm_prob = TRUE (predicted probability)
test_that("Works for lrm models (predicted probability)", {
        data <- simulated_rmsMD_data()
        data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
        p <- ggrmsMD(fit, data, lrm_prob = TRUE)
        expect_s3_class(p, "ggplot")
        expect_match(p$labels$y, "Predicted probability")
})

# Test that ggrmsMD works for cph (Cox) models
test_that("Works for cph models", {
        data <- simulated_rmsMD_data()
        # Fake a time-to-event outcome just for test (lengthstay as time, majorcomplication as event)
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::cph(Surv(lengthstay, majorcomplication) ~ rcs(age, 4), data = data, x = TRUE, y = TRUE)
        p <- ggrmsMD(fit, data)
        expect_s3_class(p, "ggplot")
})

## ---- ARGUMENT/FEATURE TESTS ----

# Test custom axis labels and titles
test_that("Custom x/y labels and plot titles work", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        xlabs <- list(age = "Patient Age (years)")
        titles <- list(age = "Spline of Age")
        p <- ggrmsMD(fit, data, xlabs = xlabs, ylab = "Custom Y", titles = titles)
        expect_identical(p$labels$x, "Patient Age (years)")
        expect_identical(p$labels$y, "Custom Y")
        expect_identical(p$labels$title, "Spline of Age")
})

# Test custom limits and scaling
test_that("Custom y-axis and x-axis limits work", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        p <- ggrmsMD(fit, data, ylim = c(0, 50), xlims = list(age = c(20, 80)))
        # Test y-axis limits set in the coordinates
        expect_equal(p$coordinates$limits$y, c(0, 50))
        # Optionally, also test x-axis limits
        expect_equal(p$coordinates$limits$x, c(20, 80))
})

# Test log y and log x axes
test_that("log_y and log_x_vars options work without error", {
        data <- simulated_rmsMD_data()
        data$logage <- data$age + 25 # ensure >0
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(logage, 4), data = data)
        expect_s3_class(
                ggrmsMD(fit, data, log_y = TRUE, log_x_vars = "logage"),
                "ggplot"
        )
})

# Test shade_inferior argument
test_that("shade_inferior works for 'higher' and 'lower'", {
        data <- simulated_rmsMD_data()
        data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
        expect_s3_class(
                ggrmsMD(fit, data, shade_inferior = "higher"),
                "ggplot"
        )
        expect_s3_class(
                ggrmsMD(fit, data, shade_inferior = "lower"),
                "ggplot"
        )
})

# Test omitting the no-effect line
test_that("noeffline = FALSE omits the dashed line", {
        data <- simulated_rmsMD_data()
        data$bin_outcome <- as.factor(data$lengthstay > median(data$lengthstay))
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::lrm(bin_outcome ~ rcs(age, 4), data = data)
        p <- ggrmsMD(fit, data, noeffline = FALSE)
        # Just check it returns a plot, visual checks can be added if needed
        expect_s3_class(p, "ggplot")
})

# Test custom number of prediction points
test_that("np argument works for prediction points", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        p_10 <- ggrmsMD(fit, data, np = 10)
        expect_s3_class(p_10, "ggplot")
})

## ---- EDGE/ROBUSTNESS TESTS ----

# Test using only a subset of RCS variables via var
test_that("Subset of RCS variables via var works", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4) + rcs(bmi, 4), data = data)

        # Ask for just one RCS variable (e.g. "age")
        plot <- ggrmsMD(fit, data, var = "age", combined = FALSE)
        # Should return a single ggplot object, not a list
        expect_s3_class(plot, "ggplot")

        # If you request both, should return a list of two ggplots
        plotlist <- ggrmsMD(fit, data, var = c("age", "bmi"), combined = FALSE)
        expect_true(is.list(plotlist))
        expect_equal(length(plotlist), 2)
        expect_s3_class(plotlist[[1]], "ggplot")
        expect_s3_class(plotlist[[2]], "ggplot")
})

# Test that missing data does not cause unexpected crash
test_that("Function handles missing data gracefully", {
        data <- simulated_rmsMD_data()
        data$age[1:10] <- NA
        dd <- datadist(na.omit(data))
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit <- rms::ols(lengthstay ~ rcs(age, 4), data = na.omit(data))
        p <- ggrmsMD(fit, na.omit(data))
        expect_s3_class(p, "ggplot")
})

# Test repeated calls without resetting datadist
test_that("Multiple calls to ggrmsMD work without resetting datadist", {
        data <- simulated_rmsMD_data()
        dd <- datadist(data)
        assign("dd", dd, envir = .GlobalEnv)
        options(datadist = "dd")
        fit1 <- rms::ols(lengthstay ~ rcs(age, 4), data = data)
        fit2 <- rms::ols(lengthstay ~ rcs(bmi, 4), data = data)
        p1 <- ggrmsMD(fit1, data)
        p2 <- ggrmsMD(fit2, data)
        expect_s3_class(p1, "ggplot")
        expect_s3_class(p2, "ggplot")
})
