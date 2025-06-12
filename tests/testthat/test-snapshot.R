library(testthat)
library(rms)

# ---- Prepare simulated data ----
df_CC <- simulated_rmsMD_data()

# Prepare missing data and imputation setup
df_MI <- simulated_rmsMD_data(type = "missing_for_MI")

imp <- aregImpute(
  ~ age + bmi + sex + smoking + majorcomplication + lengthstay,
  data = df_MI,
  n.impute = 5
)

dd_MI <- datadist(df_MI)
assign("dd_MI", dd_MI, envir = .GlobalEnv)
options(datadist = "dd_MI")

# Set up datadist for rms models (required)
dd_CC <- datadist(df_CC)
assign("dd_CC", dd_CC, envir = .GlobalEnv)
options(datadist = "dd_CC")


## ------------------------------
## LM Tests using Simulated Data
## ------------------------------
test_that("Snapshot: Feed non-rms modelfit", {
  # Fit a non-rms model (using lm) on the simulated data
  fit_lm <- lm(lengthstay ~ age + bmi, data = df_CC)
  expect_snapshot_output(modelsummary_rms(fit_lm, exp_coef = FALSE))
})

test_that("Snapshot: Warning output for non-rms model without setting exp_coef", {
  fit_lm <- lm(lengthstay ~ age + bmi, data = df_CC)
  expect_snapshot_error({
    modelsummary_rms(fit_lm)
  })
})


## ------------------------------
## OLS Tests using Simulated Data
## ------------------------------
test_that("Snapshot: OLS tests - simple model", {
  fit_ols <- ols(lengthstay ~ age + bmi + sex + smoking, data = df_CC)
  summary_df <- modelsummary_rms(fit_ols)
  expect_snapshot_output(summary_df)
})

test_that("Snapshot: OLS tests - model with interactions", {
  fit_interact <- ols(lengthstay ~ age * bmi, data = df_CC)
  summary_interact <- modelsummary_rms(fit_interact)
  expect_snapshot_output(summary_interact)
})

test_that("Snapshot: OLS tests - model with splines", {
  fit_spline <- ols(lengthstay ~ rcs(age, 4) + bmi, data = df_CC)
  summary_spline <- modelsummary_rms(fit_spline)
  full_spline <- modelsummary_rms(fit_spline, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
  expect_snapshot_output(list(simple = summary_spline, full = full_spline))
})

test_that("Snapshot: OLS tests - model with splines and interactions", {
  fit_spline_interact <- ols(lengthstay ~ rcs(age, 4) * bmi + sex, data = df_CC)
  summary_spline_interact <- modelsummary_rms(fit_spline_interact)
  full_out <- modelsummary_rms(fit_spline_interact, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
  expect_snapshot_output(list(summary = summary_spline_interact, full = full_out))
})

test_that("Snapshot: Complete case OLS with splines and covariates", {
  fit_olsCC <- ols(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
  expect_snapshot_output(modelsummary_rms(fit_olsCC))
})

test_that("Snapshot: Complete case LRM without x and y", {
  fit_lrmCC <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
  expect_snapshot_output(modelsummary_rms(fit_lrmCC))
})

test_that("Snapshot: Complete case LRM with x and y (LR test)", {
  fit_lrmCC_lrt <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x=TRUE, y=TRUE)
  expect_snapshot_output(modelsummary_rms(fit_lrmCC_lrt))
})

test_that("Snapshot: Complete case CPH without x and y", {
  fit_cphCC <- cph(Surv(time, event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
  expect_snapshot_output(modelsummary_rms(fit_cphCC))
})

test_that("Snapshot: Complete case CPH with x and y (LR test)", {
  fit_cphCC_lrt <- cph(Surv(time, event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x=TRUE, y=TRUE)
  expect_snapshot_output(modelsummary_rms(fit_cphCC_lrt))
})


## ------------------------------
## LRM Tests using Simulated Data
## ------------------------------
test_that("Snapshot: LRM tests - simple model", {
  df_CC$high_stay <- as.factor(df_CC$lengthstay > median(df_CC$lengthstay))
  fit_lrm <- lrm(high_stay ~ age + bmi + sex + smoking, data = df_CC)
  expect_snapshot_output(modelsummary_rms(fit_lrm))
})

test_that("Snapshot: LRM tests - model with interactions", {
  df_CC$high_stay <- as.factor(df_CC$lengthstay > median(df_CC$lengthstay))
  fit_lrm_int <- lrm(high_stay ~ age * bmi, data = df_CC)
  expect_snapshot_output(modelsummary_rms(fit_lrm_int))
})

test_that("Snapshot: LRM tests - model with splines", {
  df_CC$high_stay <- as.factor(df_CC$lengthstay > median(df_CC$lengthstay))
  fit_lrm_spline <- lrm(high_stay ~ rcs(age, 4) + bmi, data = df_CC)
  expect_snapshot_output(modelsummary_rms(fit_lrm_spline))
})


## ------------------------------
## CPH Tests using Simulated Data
## ------------------------------
test_that("Snapshot: CPH tests - simple model", {
  fit_cph <- cph(Surv(time, event) ~ age + sex, data = df_CC, x = TRUE, y = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_cph))
})

test_that("Snapshot: CPH tests - model with interactions", {
  fit_cph_int <- cph(Surv(time, event) ~ age * sex, data = df_CC, x = TRUE, y = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_cph_int))
})

test_that("Snapshot: CPH tests - model with splines", {
  fit_cph_spline <- cph(Surv(time, event) ~ rcs(age, 4) + sex, data = df_CC, x = TRUE, y = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_cph_spline))
})

test_that("Snapshot: CPH tests - model with splines and interactions", {
  fit_cph_spline_int <- cph(Surv(time, event) ~ rcs(age, 4) * sex, data = df_CC, x = TRUE, y = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_cph_spline_int))
})


## ------------------------------
## Snaphshots with multiple imp
## ------------------------------
test_that("Snapshot: MI OLS with splines and covariates", {
  fit_MI_ols <- fit.mult.impute(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                                ols, imp, data = df_MI)
  expect_snapshot_output(modelsummary_rms(fit_MI_ols))
})

test_that("Snapshot: MI LRM with wald test", {
  fit_MI_lrm <- fit.mult.impute(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                                lrm, imp, data = df_MI)
  expect_snapshot_output(modelsummary_rms(fit_MI_lrm))
})

test_that("Snapshot: MI CPH with wald test", {
  fit_MI_cph <- fit.mult.impute(Surv(time, event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                                cph, imp, data = df_MI)
  expect_snapshot_output(modelsummary_rms(fit_MI_cph))
})


## ------------------------------
## MI with LR test = TRUE
## ------------------------------
test_that("Snapshot: MI LRM with LR test", {
  fit_MI_lrm_lrt <- fit.mult.impute(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                                    lrm, imp, data = df_MI, lrt = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_MI_lrm_lrt, MI_lrt = TRUE))
})

test_that("Snapshot: MI CPH with LR test", {
  fit_MI_cph_lrt <- fit.mult.impute(Surv(time, event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                                    cph, imp, data = df_MI, lrt = TRUE)
  expect_snapshot_output(modelsummary_rms(fit_MI_cph_lrt, MI_lrt = TRUE))
})


## ------------------------------
## Variables Checks: Labels and Special Variable Names
## ------------------------------
test_that("Snapshot: Variables with labels and special names", {
  # Add labels to some existing variables
  attr(df_CC$sex, "label")    <- "Sex of patient"
  attr(df_CC$age, "label")    <- "Patient age (years)"
  attr(df_CC$bmi, "label")    <- "Body mass index (kg/m2)"

  set.seed(123)
  df_CC$random1 <- rnorm(nrow(df_CC))
  df_CC$`"random2"` <- rnorm(nrow(df_CC))

  attr(df_CC$random1, "label")   <- "Random normal variable 1"
  attr(df_CC$`"random2"`, "label")  <- "Random normal variable 2 with double quote in its name"

  fit_vars <- ols(lengthstay ~ rcs(age, 4) * bmi + sex + random1 + `"random2"`, data = df_CC)
  summary_vars <- modelsummary_rms(fit_vars)
  hidden_vars <- modelsummary_rms(fit_vars, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

  expect_snapshot_output(list(summary = summary_vars, hidden = hidden_vars))
})

test_that("Snapshot: Variables with reserved/special names", {
  df_CC$`if` <- rnorm(nrow(df_CC))
  attr(df_CC$`if`, "label") <- "Random variable with name 'if'"

  df_CC$`for` <- rnorm(nrow(df_CC))
  attr(df_CC$`for`, "label") <- "Random variable with name 'for'"

  df_CC$`while` <- rnorm(nrow(df_CC))
  attr(df_CC$`while`, "label") <- "Random variable with name 'while'"

  df_CC$`TRUE` <- rnorm(nrow(df_CC))
  attr(df_CC$`TRUE`, "label") <- "Random variable with name 'TRUE'"

  df_CC$`NULL` <- rnorm(nrow(df_CC))
  attr(df_CC$`NULL`, "label") <- "Random variable with name 'NULL'"

  # Check the structure of the updated data frame
  str_output <- capture.output(str(df_CC))

  fit_special <- ols(lengthstay ~ rcs(age, 4) * bmi + sex + `if` + `for` + `while` + `TRUE` + `NULL`, data = df_CC)
  summary_special <- modelsummary_rms(fit_special)
  hidden_special <- modelsummary_rms(fit_special, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

  expect_snapshot_output(list(structure = str_output,
                              summary = summary_special,
                              hidden = hidden_special))
})
