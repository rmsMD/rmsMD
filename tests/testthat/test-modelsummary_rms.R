# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

library(testthat)
library(rms)



### Test for warning if modelfit isn't an rms object
test_that("Warning for model not inheriting from 'rms'", {
  data <- simulated_rmsMD_data()
  # Fit a standard lm model (not an rms object).
  modelfit <- lm(lengthstay ~ age, data = data)
  # Should error with specific message if modelsummary_rms is called without exp_coef.
  expect_error(
    modelsummary_rms(modelfit),
    "The model fit does not belong to the 'rms' class.\nYou must specify the exp_coef argument to determine table output."
  )
})

### Test for error message when exp_coef is missing for non-standard model
test_that("Error when exp_coef is missing for non-standard rms model", {
  data <- simulated_rmsMD_data()

  # Set datadist to avoid rms warnings and ensure proper variable handling.
  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  # Fit a standard lm model, then spoof class to mimic a non-standard 'rms' subclass.
  modelfit <- lm(lengthstay ~ age, data = data)
  class(modelfit) <- c("other", "rms", class(modelfit))
  # Should error with message about specifying exp_coef for non-ols/lrm/cph models.
  expect_error(
    modelsummary_rms(modelfit),
    "Model not ols, lrm or cph.\nYou must specify the exp_coef argument to determine table output."
  )
})


### Test for Model-specific behaviour for ols
test_that("Model-specific behaviour for ols", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  # Set datadist to avoid rms warnings and ensure proper variable handling.
  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  # Fit a linear model using rms::ols (ordinary least squares)
  modelfit <- ols(lengthstay ~ age, data = data)

  # Run modelsummary_rms with exp_coef = FALSE (and default combine_ci = TRUE)
  output <- modelsummary_rms(modelfit)

  # Manually compute the expected values for the 'age' coefficient
  coef_val <- coef(modelfit)["age"]
  se_val <- sqrt(diag(vcov(modelfit)))["age"]
  lower_val <- coef_val + qnorm(0.025) * se_val
  upper_val <- coef_val + qnorm(0.975) * se_val

  # Format the values as expected (default rounding is 3 decimal places)
  expected_str <- paste0(sprintf("%.3f", coef_val),
                         " (", sprintf("%.3f", lower_val),
                         " to ", sprintf("%.3f", upper_val), ")")

  # Retrieve the row corresponding to 'age' and extract the combined CI column.
  # Assuming the combined column is named "coef_95CI" when exp_coef = FALSE.
  row_age <- output[output$variable == "age", ]
  combined_val <- row_age$coef_95CI

  # Test that the combined value matches the expected string
  expect_equal(combined_val, expected_str)
})


### Test for Model-specific behaviour for lrm
test_that("Model-specific behaviour for lrm", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  #### lrm test: create a binary outcome based on lengthstay ####
  # Create a binary outcome: 1 if lengthstay is above the median, 0 otherwise.
  data$longstay <- as.factor(data$lengthstay > median(data$lengthstay))
  modelfit_lrm <- lrm(longstay ~ age, data = data)

  # For lrm, exp_coef should be TRUE and output should include a column name containing "OR"
  output_lrm <- modelsummary_rms(modelfit_lrm, exp_coef = TRUE)
  expect_true(any(grepl("OR", colnames(output_lrm))))
})


### Test for Model-specific behaviour for cph using simulated data
test_that("Model-specific behaviour for cph using simulated data", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  # Simulate survival outcome: create time and status variables
  data$time <- abs(rnorm(nrow(data), mean = 50, sd = 10))
  data$status <- rbinom(nrow(data), 1, 0.7)

  # Fit a survival model using cph with the simulated data.
  modelfit_cph <- cph(Surv(time, status) ~ age, data = data, x = TRUE, y = TRUE)

  # For cph, exp_coef should be TRUE and output should include a column name containing "HR"
  output_cph <- modelsummary_rms(modelfit_cph, exp_coef = TRUE)
  expect_true(any(grepl("HR", colnames(output_cph))))
})


### Test for RCS handling: warning when no RCS terms found
test_that("RCS handling: warning when no RCS terms found", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  # Fit a linear model using rms::ols (which, in this case, has no RCS terms)
  modelfit <- ols(lengthstay ~ age, data = data)

  # Expect a warning that informs the user that rcs_overallp was set to TRUE
  # but no RCS terms were found, so the setting is reset to FALSE.
  expect_warning(
    modelsummary_rms(modelfit, rcs_overallp = TRUE),
    "rcs_overallp was set to TRUE by the user, but no RCS terms were found in the model fit.\nSetting rcs_overallp to FALSE."
  )
})


### Test that final output formatting works with RCS terms
test_that("Final output formatting works with RCS terms", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  # Fit an ols model that includes an RCS term (for 'age') and another linear term ('bmi')
  modelfit <- ols(lengthstay ~ rcs(age, 4) + bmi, data = data)

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


### Test that final output formatting works with RCS and interaction terms
test_that("Final output formatting works with RCS and interaction terms", {
  # Use the simulated dataset from the package helper
  data <- simulated_rmsMD_data()

  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")

  # Fit an ols model that includes an RCS term (for 'age') and another linear term ('bmi')
  modelfit <- ols(lengthstay ~ rcs(age, 4) * bmi, data = data)

  # Run modelsummary_rms with fullmodel = FALSE.
  # With fullmodel = FALSE, the intercept row should be removed,
  # and additional rows for RCS overall p-values should be appended.
  final_output <- modelsummary_rms(modelfit, rcs_overallp = TRUE, hide_rcs_coef = TRUE)

  expect_equal(nrow(final_output), 3)
})


### Test that errors are given as expect for following function calls
# Simulate data
df_CC <- simulated_rmsMD_data(type = "complete_case")
dd <- datadist(df_CC)
options(datadist = 'dd')

df_MI <- simulated_rmsMD_data(type = "missing_for_MI")
imp <- aregImpute(~ age+bmi+sex+smoking+majorcomplication+lengthstay, data = df_MI, n.impute = 5)
dd <- datadist(df_MI)
options(datadist = 'dd')

# ols
fit_MI_ols <- fit.mult.impute(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              ols, imp, data= df_MI)
modelsummary_rms(fit_MI_ols)

# lrm with wald default
fit_MI_lrm <- fit.mult.impute(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              lrm, imp, data= df_MI)
modelsummary_rms(fit_MI_lrm)

# CC cph for LR
fit_lrmCC <- cph(Surv(time,event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x = TRUE, y= TRUE)
modelsummary_rms(fit_lrmCC)

test_that("modelsummary_rms errors when MI_lrt=TRUE used on ols model", {
  # This test checks that calling modelsummary_rms with MI_lrt = TRUE
  # on an 'ols' model (which is unsupported) produces the expected error.
  expect_error(
    modelsummary_rms(fit_MI_ols, MI_lrt = TRUE),
    regexp = "MI_lrt = TRUE is currently available for lrm\\(\\) and cph\\(\\) `rms` models only"
  )
})

test_that("modelsummary_rms errors when MI_lrt=TRUE used on non-fit.mult.impute lrm model", {
  # This test confirms that setting MI_lrt = TRUE on a standard lrm model
  # (not produced by fit.mult.impute) triggers the correct error message.
  expect_error(
    modelsummary_rms(fit_lrmCC, MI_lrt = TRUE),
    regexp = "MI_lrt = TRUE was set, but the model object is not a fit.mult.impute\\(\\) object"
  )
})

test_that("modelsummary_rms errors when MI_lrt=TRUE used but lrt=TRUE not set in fit.mult.impute", {
  # This test verifies that if MI_lrt = TRUE is specified but the model
  # was fitted with fit.mult.impute() without lrt = TRUE, the proper error is raised.
  expect_error(
    modelsummary_rms(fit_MI_lrm, MI_lrt = TRUE),
    regexp = "MI_lrt = TRUE was set, but when fitting the model with fit.mult.impute\\(\\), \n`lrt = TRUE` was not used"
  )
})

