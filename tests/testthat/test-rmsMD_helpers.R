# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

library(testthat)
library(MASS)
library(rms)
library(survival)


# Test for rmsMD_extract_coef_and_se:
# This test fits an ols model and extracts coefficients and standard errors,
# then compares them to the expected values computed directly from the model.
test_that("rmsMD_extract_coef_and_se returns correct values", {
  data("Boston", package = "MASS")

  # Fit an ols model on the Boston data.
  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)

  # Expected coefficients and standard errors from the model.
  expected_coef <- coef(modelfit)
  expected_se <- sqrt(diag(vcov(modelfit)))[names(expected_coef)]

  # Verify that the helper returns the same coefficients and standard errors.
  expect_identical(ext$coef, expected_coef)
  expect_identical(ext$se, expected_se)
})

# Test for rmsMD_prepare_output_dataframe:
# This test ensures that the data frame created by the helper function contains
# the correct column names and the values match those of the extracted coefficients and SEs.
test_that("rmsMD_prepare_output_dataframe creates proper dataframe", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)

  # Check for expected column names.
  expect_true(all(c("variable", "coef", "SE") %in% colnames(df)))
  # Remove names for numeric comparison, since the data frame might not preserve names.
  expect_identical(unname(df$coef), unname(ext$coef))
  expect_identical(unname(df$SE), unname(ext$se))
})

# Test for rmsMD_calculate_raw_p_values:
# This test confirms that raw p-values are computed correctly using the standard formula.
test_that("rmsMD_calculate_raw_p_values computes correct p-values", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)
  df <- rmsMD_calculate_raw_p_values(df)

  # Expected raw p-values using the formula 2 * (1 - pnorm(|coef/SE|)).
  expected_p <- 2 * (1 - pnorm(abs(df$coef / df$SE)))
  expect_identical(df$p_values_raw, expected_p)
})

# Test for rmsMD_calculate_coef_confidence_intervals:
# This test ensures that the 95% confidence intervals for each coefficient are computed correctly.
test_that("rmsMD_calculate_coef_confidence_intervals adds correct CI columns", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)
  df <- rmsMD_calculate_coef_confidence_intervals(df)

  # Expected lower and upper bounds using qnorm for 2.5% and 97.5% quantiles.
  lower_expected <- df$coef + qnorm(0.025) * df$SE
  upper_expected <- df$coef + qnorm(0.975) * df$SE

  expect_identical(df$coef_lower95, lower_expected)
  expect_identical(df$coef_upper95, upper_expected)
})

# Test for rmsMD_format_column_output:
# This test checks that numeric values are correctly rounded to the specified number of decimal places.
test_that("rmsMD_format_column_output rounds numeric values correctly", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)
  df_formatted <- rmsMD_format_column_output(df, vars = "coef", round_dp_coef = 3)

  # For the first coefficient, check that the value is rounded as expected.
  expected_value <- sprintf("%.3f", df$coef[1])
  expect_identical(df_formatted$coef[1], expected_value)
})

# Test for rmsMD_combine_CI_for_output:
# This test verifies that the confidence intervals are combined into a single string column
# that follows the expected format (e.g., "number (number to number)").
test_that("rmsMD_combine_CI_for_output creates combined CI column correctly", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)
  df <- rmsMD_calculate_coef_confidence_intervals(df)
  key_vars <- c("coef", "coef_lower95", "coef_upper95")
  df <- rmsMD_format_column_output(df, vars = key_vars, round_dp_coef = 3)
  df <- rmsMD_combine_CI_for_output(df, key_vars)

  # The combined column should be named "coef_95CI" when exp_coef is FALSE.
  combined_column <- paste0("coef_95CI")
  # Check that the first element of the combined column matches a pattern like "number (number to number)".
  expect_match(df[[combined_column]][1],
               "^-?[0-9]+\\.?[0-9]* \\(-?[0-9]+\\.?[0-9]* to -?[0-9]+\\.?[0-9]*\\)$")
})

# Test for rmsMD_format_final_output:
# This test ensures that the final formatted output returns the correct columns.
# When fullmodel is FALSE, it should remove the intercept and only include specified columns.
# When fullmodel is TRUE, all rows are returned.
test_that("rmsMD_format_final_output returns correct columns", {
  data("Boston", package = "MASS")

  modelfit <- ols(medv ~ lstat, data = Boston)
  ext <- rmsMD_extract_coef_and_se(modelfit)
  df <- rmsMD_prepare_output_dataframe(ext$coef, ext$se)
  df <- rmsMD_calculate_raw_p_values(df)
  df <- rmsMD_calculate_coef_confidence_intervals(df)
  df <- rmsMD_format_column_output(df, vars = c("coef", "coef_lower95", "coef_upper95"), round_dp_coef = 3)
  df <- rmsMD_combine_CI_for_output(df, c("coef", "coef_lower95", "coef_upper95"))

  # Add a dummy Pvalue column so that final formatting can select it.
  df$Pvalue <- "0.05"

  # When fullmodel is FALSE, the intercept should be removed and only selected columns returned.
  final_output <- rmsMD_format_final_output(df, fullmodel = FALSE, combine_ci = TRUE, exp_coef = FALSE,
                                            key_vars = c("coef", "coef_lower95", "coef_upper95"))
  # Verify the intercept is removed.
  expect_false("Intercept" %in% final_output$variable)
  # Check that the returned column names match the expected subset.
  expect_identical(colnames(final_output), c("variable", "coef_95CI", "Pvalue"))

  # When fullmodel is TRUE, all rows should be returned.
  final_output_full <- rmsMD_format_final_output(df, fullmodel = TRUE, combine_ci = TRUE, exp_coef = FALSE,
                                                 key_vars = c("coef", "coef_lower95", "coef_upper95"))
  expect_identical(nrow(final_output_full), nrow(df))
})

