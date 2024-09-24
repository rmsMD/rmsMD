#' Extracting Coefficients and Standard Errors
#'
#' takes model fit, returns two vectors: coef and se
#'
#' @param modelfit
#'
#' @return two vectors: coef and se
#' @keywords internal
rmsMD_extract_coef_and_se <- function(modelfit) {
  # Extract the coefficients from the model fit object
  coef_values <- coef(modelfit)

  # Extract the standard errors by taking the square root of the diagonal of the variance-covariance matrix
  se_values <- sqrt(diag(vcov(modelfit)))

  # Ensure standard errors are in the same order as coefficients
  se_values <- se_values[names(coef_values)]

  # Return a list containing both coefficients and their corresponding standard errors
  return(list(coef = coef_values, se = se_values))
}


#' Extracting Restricted Cubic Spline (RCS) Variables
#'
#' Identifies the variables associated with restricted cubic spline (RCS) terms from a model fit object.
#'
#' @param modelfit A fitted model object
#'
#' @return A list containing two vectors: `variables` (RCS variable names) and `knots` (number of knots).
#' @keywords internal
rmsMD_extract_rcs_variables <- function(modelfit) {
  # Extract the terms from the model fit object as a list of strings
  formula_str <- labels(terms(modelfit))

  # Define a regular expression pattern to detect RCS terms and extract the variable name and number of knots
  rcs_pattern <- "rcs\\(([^,]+),\\s*(\\d+)\\)"

  # Extract matches using the regular expression
  matches <- unlist(regmatches(formula_str, gregexpr(rcs_pattern, formula_str, perl = TRUE)))

  # Extract the variable names from the matched RCS terms
  rcs_variables <- gsub(rcs_pattern, "\\1", matches, perl = TRUE)

  # Extract the number of knots associated with each RCS term
  num_knots <- as.numeric(gsub(rcs_pattern, "\\2", matches, perl = TRUE))

  # Return a list containing the RCS variable names and their corresponding number of knots
  return(list(variables = rcs_variables, knots = num_knots))
}

#' Removing Restricted Cubic Spline (RCS) Coefficients
#'
#' Removes the coefficients and standard errors associated with RCS terms from a model.
#'
#' @param coef_values A named vector of coefficients.
#' @param se_values A named vector of standard errors.
#' @param rcs_variables A character vector of RCS variable names.
#' @param num_knots A numeric vector indicating the number of knots for each RCS variable.
#'
#' @return A list containing filtered `coef` and `se` vectors with RCS-related terms removed.
#' @keywords internal
rmsMD_remove_rcs_coefficients <- function(coef_values, se_values, rcs_variables, num_knots) {
  # Initialize the list of coefficients to remove with the RCS variable names
  coef_to_remove <- rcs_variables

  # Loop through each RCS variable
  for (i in seq_along(rcs_variables)) {
    # For each variable, append additional coefficient names based on the number of knots
    for (j in seq_len(num_knots[i] - 2)) {
      # Construct the coefficient name by appending apostrophes
      # For example, if the variable is 'age' and j=1, it becomes "age'"
      # If j=2, it becomes "age''", and so on
      coef_name <- paste0(rcs_variables[i], paste0(rep("'", j), collapse = ""))
      coef_to_remove <- append(coef_to_remove, coef_name)
    }
  }

  # Filter out RCS-related coefficients from the coefficients and standard errors
  coef_values_filtered <- coef_values[!names(coef_values) %in% coef_to_remove]
  se_values_filtered <- se_values[!names(se_values) %in% coef_to_remove]

  # Return the filtered coefficients and standard errors as a list
  return(list(coef = coef_values_filtered, se = se_values_filtered))
}


#' Preparing the Output Data Frame
#'
#' Creates a data frame with variable names, coefficients, and standard errors.
#'
#' @param coef_values A named vector of coefficients.
#' @param se_values A named vector of standard errors.
#'
#' @return A data frame with variables: `variable`, `coef`, and `SE`.
#' @keywords internal
rmsMD_prepare_output_dataframe <- function(coef_values, se_values) {
  # Create a data frame containing variable names, coefficients, standard errors, and raw p-values
  output_df <- data.frame(
    variable = names(coef_values), # Names of the variables
    coef = coef_values, # Rounded coefficients
    SE = se_values, # Standard errors
    stringsAsFactors = FALSE # Do not treat character columns as factors
  )

  # Return the prepared data frame
  return(output_df)
}

#' Calculating Raw p-Values
#'
#' Calculates p-values from coefficients and standard errors.
#'
#' @param output_df A data frame containing `coef` and `SE` columns.
#'
#' @return A data frame with an additional `p_values_raw` column containing raw p-values.
#' @keywords internal
rmsMD_calculate_raw_p_values <- function(output_df) {
  # Calculate raw p-values: 2 * (1 - CDF of the normal distribution) using the absolute value of the z-statistic (coef / SE)
  output_df$p_values_raw <- 2 * (1 - pnorm(abs(output_df$coef / output_df$SE)))
  return(output_df)
}

#' Calculating Coefficient Confidence Intervals
#'
#' Adds 95% confidence intervals for coefficients to the output data frame.
#'
#' @param output_df A data frame containing `coef` and `SE` columns.
#'
#' @return A data frame with additional `coef_lower95` and `coef_upper95` columns.
#' @keywords internal
rmsMD_calculate_coef_confidence_intervals <- function(output_df) {
  # Calculate the lower 95% confidence interval: coefficient + z-score at 2.5% * standard error
  output_df$coef_lower95 <- output_df$coef + qnorm(0.025) * output_df$SE

  # Calculate the upper 95% confidence interval: coefficient + z-score at 97.5% * standard error
  output_df$coef_upper95 <- output_df$coef + qnorm(0.975) * output_df$SE

  # Return the lower and upper confidence intervals as a list
  return(output_df)
}


#' Adding ANOVA Results
#'
#' Adds ANOVA results (e.g., overall p-values for RCS terms) to the output data frame.
#'
#' @param output_df A data frame containing model coefficients and statistics.
#' @param modelfit A fitted model object (e.g., from `rms::ols`, `rms::lrm`, or `rms::cph`).
#' @param rcs_variables A character vector of RCS variable names.
#'
#' @return A data frame with added ANOVA results for RCS terms.
#'
#' @import rms
#'
#' @keywords internal
rmsMD_add_anova_results <- function(output_df, modelfit, rcs_variables) {
  # Retrieve the ANOVA results as a matrix from the model fit object
  anova_result <- as.matrix(anova(modelfit))

  # Filter the ANOVA results to only include rows that match the RCS variables
  keep_rows <- apply(sapply(rcs_variables, grepl, rownames(anova_result)), 1, any)
  anova_result <- anova_result[keep_rows, , drop = FALSE]

  # Convert the filtered ANOVA results into a data frame
  anovadf <- as.data.frame(anova_result)

  # Create a new data frame with RCS variable names and their raw p-values from ANOVA results
  anova_res_df <- data.frame(
    variable = paste0("RCSoverallP:", rownames(anovadf)), # Add "RCSoverallP:" prefix to each variable
    p_values_raw = anovadf$P, # Raw p-values from ANOVA
    stringsAsFactors = FALSE, # Do not treat character columns as factors
    row.names = NULL # Remove row names
  )

  # Ensure the columns in the new ANOVA result data frame match the original output data frame
  output_columns <- colnames(output_df)
  for (col in output_columns) {
    if (!(col %in% colnames(anova_res_df))) {
      anova_res_df[[col]] <- NA # Add missing columns as NA
    }
  }

  # Reorder the columns in the ANOVA result data frame to match the original output data frame
  anova_res_df <- anova_res_df[output_columns]

  # Bind the ANOVA results to the original output data frame
  output_df <- rbind(output_df, anova_res_df)

  # Return the updated data frame with ANOVA results included
  return(output_df)
}

#' Formatting Column For Output
#'
#' Formats specific columns in the output data frame by rounding numeric values.
#'
#' @param output_df A data frame containing the output of model results.
#' @param vars A character vector of column names to format.
#' @param round_dp_coef The number of decimal places to round the values.
#'
#' @return A data frame with the specified columns formatted.
#' @keywords internal
rmsMD_format_column_output <- function(output_df, vars = c(), round_dp_coef){
  spr_text_coef <- paste0("%.", round_dp_coef, "f")
  for(var in vars){
    output_df[[var]] <- ifelse(is.na(output_df[[var]]),
                               NA,
                               sprintf(spr_text_coef, output_df[[var]]))
  }
  return(output_df)
}

#' Combining Confidence Intervals for Output
#'
#' Combines lower and upper confidence intervals into a single formatted string.
#'
#' @param output_df A data frame containing coefficient, lower, and upper confidence intervals.
#' @param key_vars A character vector containing column names for the effect estimate, lower, and upper confidence intervals.
#'
#' @return A data frame with a new column containing combined confidence intervals.
#' @keywords internal
rmsMD_combine_CI_for_output <- function(output_df, key_vars){

  effect_est <- key_vars[1]
  lower95 <- key_vars[2]
  upper95 <- key_vars[3]

  column_name <- paste0(effect_est,"_95CI")

  output_df[[column_name]] <- ifelse(is.na(output_df[[effect_est]]),
                                     "RCS terms",
                                     paste(output_df[[effect_est]],
                                           " (", output_df[[lower95]],
                                           " to ", output_df[[upper95]], ")", sep = ""))
  return(output_df)
}


#' Formatting Final Output
#'
#' Formats the final output data frame, including the option to exclude intercept and select columns.
#'
#' @param output_df A data frame containing model results.
#' @param fullmodel A logical indicating whether to return the full model output.
#' @param combine_ci A logical indicating whether to combine confidence intervals.
#' @param exp_coef A logical indicating whether to use exponentiated coefficients.
#' @param key_vars A character vector of key column names to include in the output.
#'
#' @return A formatted data frame for final output.
#' @keywords internal
rmsMD_format_final_output <- function(output_df, fullmodel, combine_ci,exp_coef,key_vars) {
  # If fullmodel is TRUE, return all variables
  if (fullmodel) {
    return(output_df)
  } else {
    # Otherwise, exclude the intercept
    output_df <- output_df[output_df$variable != "Intercept", ]

    # Depending on combine_ci, select appropriate columns
    if (combine_ci) {
      if(exp_coef){
        return(output_df[, c("variable", "exp_coef_95CI", "Pvalue")])
      }else{
        return(output_df[, c("variable", "coef_95CI", "Pvalue")])
      }
    } else {
      return(output_df[, c("variable", key_vars, "Pvalue")])
    }
  }
}


