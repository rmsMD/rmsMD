# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

library(testthat)
library(MASS)
library(rms)
library(survival)


### Test for warning if modelfit isn't an rms object
test_that("Warning for model not inheriting from 'rms'", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")
  # Fit a linear model (which does not inherit from "rms")
  modelfit <- lm(medv ~ lstat, data = Boston)
  # Expect a warning when calling modelsummary_rms on a non-rms model without specifying the exp_coef
  expect_error(
    modelsummary_rms(modelfit),
    "The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output."
  )
})

### Test for error message when exp_coef is missing for non-standard model
test_that("Error when exp_coef is missing for non-standard rms model", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")
  # Fit a linear model and then manually adjust the class to mimic a non-standard rms model.
  modelfit <- lm(medv ~ lstat, data = Boston)
  # Modify the class to include "rms" but not one of the recognised classes: "ols", "lrm", or "cph"
  class(modelfit) <- c("other", "rms", class(modelfit))

  # Expect an error since exp_coef is missing
  expect_error(
    modelsummary_rms(modelfit),
    "Model not ols, lrm or cph. You must specify exp_coef argument to determine table output."
  )
})


### Test for Model-specific behaviour for ols
test_that("Model-specific behaviour for ols", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")

  # Fit a linear model using rms::ols (ordinary least squares)
  modelfit <- ols(medv ~ lstat, data = Boston)

  # Run modelsummary_rms with exp_coef = FALSE (and default combine_ci = TRUE)
  output <- modelsummary_rms(modelfit)

  # Manually compute the expected values for the 'lstat' coefficient
  coef_val <- coef(modelfit)["lstat"]
  se_val <- sqrt(diag(vcov(modelfit)))["lstat"]
  lower_val <- coef_val + qnorm(0.025) * se_val
  upper_val <- coef_val + qnorm(0.975) * se_val

  # Format the values as expected (default rounding is 3 decimal places)
  expected_str <- paste0(sprintf("%.3f", coef_val),
                         " (", sprintf("%.3f", lower_val),
                         " to ", sprintf("%.3f", upper_val), ")")

  # Retrieve the row corresponding to 'lstat' and extract the combined CI column.
  # Assuming the combined column is named "coef_95CI" when exp_coef = FALSE.
  row_lstat <- output[output$variable == "lstat", ]
  combined_val <- row_lstat$coef_95CI

  # Test that the combined value matches the expected string
  expect_equal(combined_val, expected_str)
})


### Test for Model-specific behaviour for lrm
test_that("Model-specific behaviour for lrm", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")

  #### lrm test: create a binary outcome based on medv ####
  # Create a binary outcome: 1 if medv is above the median, 0 otherwise.
  Boston$high_medv <- as.factor(Boston$medv > median(Boston$medv))
  modelfit_lrm <- lrm(high_medv ~ lstat, data = Boston)

  # For lrm, exp_coef should be TRUE and output should include a column name containing "OR"
  output_lrm <- modelsummary_rms(modelfit_lrm, exp_coef = TRUE)
  expect_true(any(grepl("OR", colnames(output_lrm))))
})


### Test for Model-specific behaviour for cph using the lung dataset
test_that("Model-specific behaviour for cph using the lung dataset", {
  # Load the lung dataset directly from the survival package
  lung <- survival::lung

  # Fit a survival model using cph with the lung dataset.
  # Note: In the lung dataset, status is coded as 1=censored and 2=dead.
  modelfit_cph <- cph(Surv(time, status) ~ age, data = lung, x = TRUE, y = TRUE)

  # For cph, exp_coef should be TRUE and output should include a column name containing "HR"
  output_cph <- modelsummary_rms(modelfit_cph, exp_coef = TRUE)
  expect_true(any(grepl("HR", colnames(output_cph))))
})


### Test for RCS handling: error when no RCS terms found
test_that("RCS handling: warning when no RCS terms found", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")

  # Fit a linear model using rms::ols (which, in this case, has no RCS terms)
  modelfit <- ols(medv ~ lstat, data = Boston)

  # Expect a warning that informs the user that rcs_overallp was set to TRUE
  # but no RCS terms were found, so the setting is reset to FALSE.
  expect_warning(
    modelsummary_rms(modelfit, rcs_overallp = TRUE),
    "rcs_overallp was set to TRUE by the user but no RCS terms were found in the model fit. Setting rcs_overallp to FALSE."
  )
})


### Test that final output formatting works with RCS terms
test_that("Final output formatting works with RCS terms", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")

  # Fit an ols model that includes an RCS term (for 'lstat') and another linear term ('crim')
  modelfit <- ols(medv ~ rcs(lstat, 4) + crim, data = Boston)

  # Run modelsummary_rms with fullmodel = FALSE.
  # With fullmodel = FALSE, the intercept row should be removed,
  # and additional rows for RCS overall p-values should be appended.
  final_output <- modelsummary_rms(modelfit, rcs_overallp = TRUE)

  # Verify that the intercept row is removed.
  expect_false("Intercept" %in% final_output$variable)

  # Check that the final output contains exactly the expected columns.
  # When exp_coef is FALSE and combine_ci is TRUE, we expect "variable", "coef_95CI", and "Pvalue".
  expect_equal(colnames(final_output), c("variable", "coef_95CI", "Pvalue"))

  # Check rownames: they should either be NULL or the default sequential rownames.
  current_rownames <- rownames(final_output)
  acceptable_rownames <- is.null(current_rownames) || identical(current_rownames, as.character(seq_len(nrow(final_output))))
  expect_true(acceptable_rownames)

  # Additionally, check that at least one row corresponds to an overall RCS p-value.
  # Such rows have variable names starting with "RCSoverallP:".
  rcs_rows <- grep("^RCSoverallP:", final_output$variable, value = TRUE)
  expect_true(length(rcs_rows) >= 1)

  # When fullmodel is TRUE, all rows (including the intercept) should be returned.
  full_output <- modelsummary_rms(modelfit, fullmodel = TRUE, rcs_overallp = TRUE)
  expect_true("Intercept" %in% full_output$variable)
})


### Test that final output formatting works with RCS terms
test_that("Final output formatting works with RCS and interaction terms", {
  # Use the Boston dataset from MASS
  data("Boston", package = "MASS")

  # Fit an ols model that includes an RCS term (for 'lstat') and another linear term ('crim')
  modelfit <- ols(medv ~ rcs(lstat, 4) * crim, data = Boston)

  # Run modelsummary_rms with fullmodel = FALSE.
  # With fullmodel = FALSE, the intercept row should be removed,
  # and additional rows for RCS overall p-values should be appended.
  final_output <- modelsummary_rms(modelfit, rcs_overallp = TRUE, hide_rcs_coef = TRUE)

  expect_equal(nrow(final_output), 3)
})
