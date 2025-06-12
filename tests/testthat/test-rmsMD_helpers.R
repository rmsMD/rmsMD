library(testthat)
library(rms)

# Generate a simulated dataset using the helper function from the package.
data <- simulated_rmsMD_data()


prepare_output_df <- function(modelfit) {
  coef_vals <- coef(modelfit)
  se_vals <- sqrt(diag(vcov(modelfit)))
  se_vals <- se_vals[names(coef_vals)]

  lower95 <- coef_vals + qnorm(0.025) * se_vals
  upper95 <- coef_vals + qnorm(0.975) * se_vals

  coef_95CI <- paste0(
    format(coef_vals, digits = 3), " (",
    format(lower95, digits = 3), " to ",
    format(upper95, digits = 3), ")"
  )
  exp_coef_95CI <- paste0(
    format(exp(coef_vals), digits = 3), " (",
    format(exp(lower95), digits = 3), " to ",
    format(exp(upper95), digits = 3), ")"
  )

  Pvalue <- 2 * (1 - pnorm(abs(coef_vals / se_vals)))

  data.frame(
    variable = names(coef_vals),
    coef_95CI = coef_95CI,
    exp_coef_95CI = exp_coef_95CI,
    coef = coef_vals,
    coef_lower95 = lower95,
    coef_upper95 = upper95,
    Pvalue = Pvalue,
    stringsAsFactors = FALSE
  )
}

key_vars <- c("coef", "coef_lower95", "coef_upper95")


# Wrap your tests in a function that accepts data as input
run_all_rmsMD_tests <- function(data, data_description = "complete") {

  ### OLS Model Tests ###
  ols_fit <- ols(lengthstay ~ age + bmi + sex + smoking + majorcomplication, data = data)
  output_df_ols <- prepare_output_df(ols_fit)

  test_that(paste("OLS:", data_description, "- combine_ci=TRUE, exp_coef=FALSE excludes intercept and returns correct columns"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_ols,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "coef_95CI", "Pvalue"))
    expect_false("Intercept" %in% res$variable)
  })

  test_that(paste("OLS:", data_description, "- combine_ci=TRUE, exp_coef=TRUE returns exponentiated CIs"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_ols,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = TRUE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "exp_coef_95CI", "Pvalue"))
  })

  test_that(paste("OLS:", data_description, "- combine_ci=FALSE returns raw coefficients with p-values"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_ols,
      fullmodel = FALSE,
      combine_ci = FALSE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", key_vars, "Pvalue"))
  })

  test_that(paste("OLS:", data_description, "- fullmodel=TRUE returns full unfiltered output_df"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_ols,
      fullmodel = TRUE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(res, output_df_ols)
  })


  ### Logistic Regression Model Tests ###
  lrm_fit <- lrm(majorcomplication ~ age + bmi + sex + smoking, data = data)
  output_df_log <- prepare_output_df(lrm_fit)

  test_that(paste("Logistic:", data_description, "- combine_ci=TRUE, exp_coef=FALSE excludes intercept and returns correct columns"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_log,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "coef_95CI", "Pvalue"))
    expect_false("Intercept" %in% res$variable)
  })

  test_that(paste("Logistic:", data_description, "- combine_ci=TRUE, exp_coef=TRUE returns exponentiated CIs"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_log,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = TRUE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "exp_coef_95CI", "Pvalue"))
  })

  test_that(paste("Logistic:", data_description, "- combine_ci=FALSE returns raw coefficients with p-values"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_log,
      fullmodel = FALSE,
      combine_ci = FALSE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", key_vars, "Pvalue"))
  })

  test_that(paste("Logistic:", data_description, "- fullmodel=TRUE returns full unfiltered output_df"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_log,
      fullmodel = TRUE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(res, output_df_log)
  })


  ### Cox Proportional Hazards Model Tests ###
  cph_fit <- cph(
    formula = Surv(time, event) ~ age + bmi + sex + smoking,
    data = data,
    x = TRUE,
    y = TRUE,
    surv = TRUE
  )
  output_df_cox <- prepare_output_df(cph_fit)

  test_that(paste("Cox:", data_description, "- combine_ci=TRUE, exp_coef=FALSE excludes intercept and returns correct columns"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_cox,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "coef_95CI", "Pvalue"))
    expect_false("Intercept" %in% res$variable)
  })

  test_that(paste("Cox:", data_description, "- combine_ci=TRUE, exp_coef=TRUE returns exponentiated CIs"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_cox,
      fullmodel = FALSE,
      combine_ci = TRUE,
      exp_coef = TRUE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", "exp_coef_95CI", "Pvalue"))
  })

  test_that(paste("Cox:", data_description, "- combine_ci=FALSE returns raw coefficients with p-values"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_cox,
      fullmodel = FALSE,
      combine_ci = FALSE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(colnames(res), c("variable", key_vars, "Pvalue"))
  })

  test_that(paste("Cox:", data_description, "- fullmodel=TRUE returns full unfiltered output_df"), {
    res <- rmsMD_format_final_output(
      output_df = output_df_cox,
      fullmodel = TRUE,
      combine_ci = TRUE,
      exp_coef = FALSE,
      key_vars = key_vars
    )
    expect_identical(res, output_df_cox)
  })
}

# Run tests on complete data (no missing)
run_all_rmsMD_tests(simulated_rmsMD_data(type = "complete_case"), data_description = "complete data")

# Run tests on incomplete data (with missingness)
run_all_rmsMD_tests(simulated_rmsMD_data(type = "missing_for_MI"), data_description = "incomplete data")
