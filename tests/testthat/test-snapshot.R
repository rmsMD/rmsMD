library(testthat)
library(rms)

# ---- Prepare simulated data ----
sim_data <- simulated_rmsMD_data()

# Add simulated survival outcome
set.seed(123)
sim_data$time <- abs(rnorm(nrow(sim_data), mean = 50, sd = 10))
sim_data$status <- rbinom(nrow(sim_data), 1, 0.7)

# Set up datadist for rms models (required)
dd <- datadist(sim_data)
assign("dd", dd, envir = .GlobalEnv)
options(datadist = "dd")

## ------------------------------
## LM Tests using Simulated Data
## ------------------------------
test_that("Snapshot: Feed non-rms modelfit", {
        # Fit a non-rms model (using lm) on the simulated data
        fit_lm <- lm(lengthstay ~ age + bmi, data = sim_data)
        expect_snapshot_output(modelsummary_rms(fit_lm, exp_coef = FALSE))
})

test_that("Snapshot: Warning output for non-rms model without setting exp_coef", {
        fit_lm <- lm(lengthstay ~ age + bmi, data = sim_data)
        expect_snapshot_error({
                modelsummary_rms(fit_lm)
        })
})

## ------------------------------
## OLS Tests using Simulated Data
## ------------------------------
test_that("Snapshot: OLS tests - simple model", {
        fit_ols <- ols(lengthstay ~ age + bmi + sex + smoking, data = sim_data)
        summary_df <- modelsummary_rms(fit_ols)
        expect_snapshot_output(summary_df)
})

test_that("Snapshot: OLS tests - model with interactions", {
        fit_interact <- ols(lengthstay ~ age * bmi, data = sim_data)
        summary_interact <- modelsummary_rms(fit_interact)
        expect_snapshot_output(summary_interact)
})

test_that("Snapshot: OLS tests - model with splines", {
        fit_spline <- ols(lengthstay ~ rcs(age, 4) + bmi, data = sim_data)
        summary_spline <- modelsummary_rms(fit_spline)
        full_spline <- modelsummary_rms(fit_spline, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
        expect_snapshot_output(list(simple = summary_spline, full = full_spline))
})

test_that("Snapshot: OLS tests - model with splines and interactions", {
        fit_spline_interact <- ols(lengthstay ~ rcs(age, 4) * bmi + sex, data = sim_data)
        summary_spline_interact <- modelsummary_rms(fit_spline_interact)
        full_out <- modelsummary_rms(fit_spline_interact, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
        expect_snapshot_output(list(summary = summary_spline_interact, full = full_out))
})

## ------------------------------
## LRM Tests using Simulated Data
## ------------------------------
test_that("Snapshot: LRM tests - simple model", {
        sim_data$high_stay <- as.factor(sim_data$lengthstay > median(sim_data$lengthstay))
        fit_lrm <- lrm(high_stay ~ age + bmi + sex + smoking, data = sim_data)
        expect_snapshot_output(modelsummary_rms(fit_lrm))
})

test_that("Snapshot: LRM tests - model with interactions", {
        sim_data$high_stay <- as.factor(sim_data$lengthstay > median(sim_data$lengthstay))
        fit_lrm_int <- lrm(high_stay ~ age * bmi, data = sim_data)
        expect_snapshot_output(modelsummary_rms(fit_lrm_int))
})

test_that("Snapshot: LRM tests - model with splines", {
        sim_data$high_stay <- as.factor(sim_data$lengthstay > median(sim_data$lengthstay))
        fit_lrm_spline <- lrm(high_stay ~ rcs(age, 4) + bmi, data = sim_data)
        expect_snapshot_output(modelsummary_rms(fit_lrm_spline))
})

## ------------------------------
## CPH Tests using Simulated Data
## ------------------------------
test_that("Snapshot: CPH tests - simple model", {
        fit_cph <- cph(Surv(time, status) ~ age + sex, data = sim_data, x = TRUE, y = TRUE)
        expect_snapshot_output(modelsummary_rms(fit_cph))
})

test_that("Snapshot: CPH tests - model with interactions", {
        fit_cph_int <- cph(Surv(time, status) ~ age * sex, data = sim_data, x = TRUE, y = TRUE)
        expect_snapshot_output(modelsummary_rms(fit_cph_int))
})

test_that("Snapshot: CPH tests - model with splines", {
        fit_cph_spline <- cph(Surv(time, status) ~ rcs(age, 4) + sex, data = sim_data, x = TRUE, y = TRUE)
        expect_snapshot_output(modelsummary_rms(fit_cph_spline))
})

test_that("Snapshot: CPH tests - model with splines and interactions", {
        fit_cph_spline_int <- cph(Surv(time, status) ~ rcs(age, 4) * sex, data = sim_data, x = TRUE, y = TRUE)
        expect_snapshot_output(modelsummary_rms(fit_cph_spline_int))
})

## ------------------------------
## Variables Checks: Labels and Special Variable Names
## ------------------------------
test_that("Snapshot: Variables with labels and special names", {
        # Add labels to some existing variables
        attr(sim_data$sex, "label")    <- "Sex of patient"
        attr(sim_data$age, "label")    <- "Patient age (years)"
        attr(sim_data$bmi, "label")    <- "Body mass index (kg/m2)"

        set.seed(123)
        sim_data$random1 <- rnorm(nrow(sim_data))
        sim_data$`"random2"` <- rnorm(nrow(sim_data))

        attr(sim_data$random1, "label")   <- "Random normal variable 1"
        attr(sim_data$`"random2"`, "label")  <- "Random normal variable 2 with double quote in its name"

        fit_vars <- ols(lengthstay ~ rcs(age, 4) * bmi + sex + random1 + `"random2"`, data = sim_data)
        summary_vars <- modelsummary_rms(fit_vars)
        hidden_vars <- modelsummary_rms(fit_vars, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

        expect_snapshot_output(list(summary = summary_vars, hidden = hidden_vars))
})

test_that("Snapshot: Variables with reserved/special names", {
        sim_data$`if` <- rnorm(nrow(sim_data))
        attr(sim_data$`if`, "label") <- "Random variable with name 'if'"

        sim_data$`for` <- rnorm(nrow(sim_data))
        attr(sim_data$`for`, "label") <- "Random variable with name 'for'"

        sim_data$`while` <- rnorm(nrow(sim_data))
        attr(sim_data$`while`, "label") <- "Random variable with name 'while'"

        sim_data$`TRUE` <- rnorm(nrow(sim_data))
        attr(sim_data$`TRUE`, "label") <- "Random variable with name 'TRUE'"

        sim_data$`NULL` <- rnorm(nrow(sim_data))
        attr(sim_data$`NULL`, "label") <- "Random variable with name 'NULL'"

        # Check the structure of the updated data frame
        str_output <- capture.output(str(sim_data))

        fit_special <- ols(lengthstay ~ rcs(age, 4) * bmi + sex + `if` + `for` + `while` + `TRUE` + `NULL`, data = sim_data)
        summary_special <- modelsummary_rms(fit_special)
        hidden_special <- modelsummary_rms(fit_special, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

        expect_snapshot_output(list(structure = str_output,
                                    summary = summary_special,
                                    hidden = hidden_special))
})
