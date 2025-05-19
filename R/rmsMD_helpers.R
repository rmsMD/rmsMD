
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


