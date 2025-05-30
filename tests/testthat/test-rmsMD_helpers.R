library(testthat)
library(rms)

# Generate a simulated dataset using the helper function from the package.
data <- simulated_rmsMD_data()

test_that("rmsMD_format_final_output formats model results as expected", {
        # Fit an OLS regression model to the simulated surgical data.
        # Outcome: length of hospital stay; Predictors: age, bmi, sex, smoking, majorcomplication.
        modelfit <- ols(lengthstay ~ age + bmi + sex + smoking + majorcomplication, data = data)

        # Prepare the interim output_df to mimic what the formatting function expects.
        # This includes point estimates, confidence intervals, and p-values for each variable.

        coef_vals <- coef(modelfit)  # Extract coefficient estimates
        se_vals   <- sqrt(diag(vcov(modelfit)))  # Standard errors of coefficients
        se_vals   <- se_vals[names(coef_vals)]   # Ensure correct ordering

        # Calculate lower and upper 95% confidence intervals for coefficients
        lower95   <- coef_vals + qnorm(0.025) * se_vals
        upper95   <- coef_vals + qnorm(0.975) * se_vals

        # Create formatted strings for confidence intervals
        coef_95CI <- paste0(
                format(coef_vals, digits = 3), " (",
                format(lower95, digits = 3), " to ",
                format(upper95, digits = 3), ")"
        )
        # Same, but with exponentiated coefficients (for e.g. logistic models)
        exp_coef_95CI <- paste0(
                format(exp(coef_vals), digits = 3), " (",
                format(exp(lower95), digits = 3), " to ",
                format(exp(upper95), digits = 3), ")"
        )

        # Calculate two-sided p-values for coefficients
        Pvalue <- 2 * (1 - pnorm(abs(coef_vals / se_vals)))

        # Combine all pieces into a single data frame in the expected format
        output_df <- data.frame(
                variable       = names(coef_vals),
                coef_95CI      = coef_95CI,
                exp_coef_95CI  = exp_coef_95CI,
                coef           = coef_vals,
                coef_lower95   = lower95,
                coef_upper95   = upper95,
                Pvalue         = Pvalue,
                stringsAsFactors = FALSE
        )
        # List of columns for the 'key_vars' argument
        key_vars <- c("coef", "coef_lower95", "coef_upper95")

        # Test: combine_ci = TRUE, exp_coef = FALSE
        # Should return variable, formatted coefficient CI, and p-value columns (excluding intercept)
        res1 <- rmsMD_format_final_output(
                output_df,
                fullmodel  = FALSE,
                combine_ci = TRUE,
                exp_coef   = FALSE,
                key_vars   = key_vars
        )
        expect_identical(colnames(res1), c("variable", "coef_95CI", "Pvalue"))
        expect_false("Intercept" %in% res1$variable) # Intercept should be excluded

        # Test: combine_ci = TRUE, exp_coef = TRUE
        # Should return variable, formatted exponentiated coefficient CI, and p-value
        res2 <- rmsMD_format_final_output(
                output_df,
                fullmodel  = FALSE,
                combine_ci = TRUE,
                exp_coef   = TRUE,
                key_vars   = key_vars
        )
        expect_identical(colnames(res2), c("variable", "exp_coef_95CI", "Pvalue"))

        # Test: combine_ci = FALSE
        # Should return variable, raw coefficient columns, and p-value
        res3 <- rmsMD_format_final_output(
                output_df,
                fullmodel  = FALSE,
                combine_ci = FALSE,
                exp_coef   = FALSE,
                key_vars   = key_vars
        )
        expect_identical(colnames(res3), c("variable", key_vars, "Pvalue"))

        # Test: fullmodel = TRUE (should return all columns, unfiltered)
        res4 <- rmsMD_format_final_output(
                output_df,
                fullmodel  = TRUE,
                combine_ci = TRUE,
                exp_coef   = FALSE,
                key_vars   = key_vars
        )
        expect_identical(res4, output_df)
})
