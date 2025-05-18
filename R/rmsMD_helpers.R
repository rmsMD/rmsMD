

#' Preparing the Output Data Frame
#'
#' Creates a data frame with variable names, coefficients, and standard errors.
#'
#' @param coef_values A named vector of coefficients.
#' @param se_values A named vector of standard errors.
#'
#' @return A data frame with variables: `variable`, `coef`, and `SE`.
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
rmsMD_calculate_coef_confidence_intervals <- function(output_df) {
  # Calculate the lower 95% confidence interval: coefficient + z-score at 2.5% * standard error
  output_df$coef_lower95 <- output_df$coef + qnorm(0.025) * output_df$SE

  # Calculate the upper 95% confidence interval: coefficient + z-score at 97.5% * standard error
  output_df$coef_upper95 <- output_df$coef + qnorm(0.975) * output_df$SE

  # Return the lower and upper confidence intervals as a list
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


