#' @title Create model summary for rms models
#'
#' @description The `modelsummary_rms` function processes the output from models fitted using the **rms** package and generates a summarized dataframe of the results.
#' This summary is tailored for publication in medical journals, presenting effect estimates, confidence intervals, and p-values.
#'
#' @param modelfit The output from an rms model.
#' @param combine_ci If `TRUE`, combines the effect estimates and 95% confidence intervals into a single column. Default is `TRUE`.
#' @param round_dp_coef Specifies the number of decimal places to display for the effect estimates. Default is `3`.
#' @param round_dp_p Specifies the number of decimal places to display for P values. Default is `3`.
#' @param rcs_overallp If `TRUE`, provides an overall P value for Restricted Cubic Spline (RCS) terms, sourced from `anova(modelfit)`. Default is `FALSE`.
#' @param hide_rcs_coef If `TRUE`, hides the individual coefficients for Restricted Cubic Spline (RCS) variables. Default is `FALSE`.
#' @param exp_coef If `TRUE`, outputs the exponentiated coefficients (`exp(coef)`) as the effect estimates. Applicable only for model types other than `ols`, `lrm`, or `cph`. If `NULL`, no exponentiation is performed. Default is `NULL`.
#' @param fullmodel If `TRUE`, includes all intermediate steps in the summary, allowing users to verify and compare with standard model outputs. Default is `FALSE`.
#'
#' @return Returns a dataframe of results. This can easily be outputted to word using
#' packages such as flextable and officer.
#'
#' @import rms
#' @importFrom stats anova coef pnorm qnorm terms vcov
#' @export
#'
#'
modelsummary_rms <- function(modelfit,
                             combine_ci = TRUE,
                             round_dp_coef = 3,
                             round_dp_p = 3,
                             rcs_overallp = FALSE,
                             hide_rcs_coef = FALSE,
                             exp_coef = NULL,
                             fullmodel = FALSE) {

  ########## add some logic here that looks at the model types via class and gives
  ########## you what the combine CI names etc will be (ie the HR OR etc)
  ######### and also sets exp_coef to true or false, or it is isn't ols cph or lrm
  ######### gives an error message saying exp_coef must be specified

  # warning if modelfit isn't an rms object
  if (!inherits(modelfit, "rms")) {
    warning("The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output.")
  }

  ########## defining arguments based on model class ##########

  if (inherits(modelfit, "ols")) {
    exp_coef <- FALSE
  } else if (inherits(modelfit, "lrm")) {
    exp_coef <- TRUE
    exp_coef_name <- "OR"
  } else if (inherits(modelfit, "cph")) {
    exp_coef = TRUE
    exp_coef_name <- "HR"
  } else if(is.null(exp_coef)){
    stop("Model not ols, lrm or cph. You must specify exp_coef argument to determine table output.")
  }

  # Ensure rcs_overallp is TRUE if hide_rcs_coef is TRUE
  if (hide_rcs_coef & rcs_overallp == FALSE) {
    stop("Both hide_rcs_coef & rcs_overallp cannot be set to FALSE.\nMust have rcs_overallp as TRUE if hide_rcs_coef is FALSE")
  }

  ########## Extract coefficients and standard errors ##########
  coef_se_list <- rmsMD_extract_coef_and_se(modelfit) # Helper function to extract coefficients and SE
  coef_values <- coef_se_list$coef               # Extracted coefficients
  se_values <- coef_se_list$se                   # Extracted standard errors

  ########## Handle RCS terms (if applicable) ##########
  if (rcs_overallp) {
    # Extract RCS variable names and number of knots using helper
    rcs_data <- rmsMD_extract_rcs_variables(modelfit)
    rcs_variables <- rcs_data$variables
    num_knots <- rcs_data$knots

    if (length(rcs_variables)== 0) {
      stop("rcs_overallp set to TRUE but no RCS terms found in the model fit.")
    }

    # Remove RCS coefficients if hide_rcs_coef is TRUE
    if (hide_rcs_coef) {
      # Call the new helper function to remove RCS coefficients
      rcs_filtered <- rmsMD_remove_rcs_coefficients(
        coef_values, se_values, rcs_variables, num_knots)

      # Update coef_values and se_values with the filtered results
      coef_values <- rcs_filtered$coef
      se_values <- rcs_filtered$se
    }
  }

  ## we now have a coef_values and se_values list (minus the RCS terms if
  # hide_rcs_coef is TRUE). Also list of rcs_variabes to also use later

  ########## make coef and SE into a dataframe ##########
  output_df <- rmsMD_prepare_output_dataframe(coef_values, se_values)


  ########## Calculate raw p-values ##########
  # note, don't want to round here, just want raw values and then a seperate
  # function to format the pvalues after the anova raw p vals have been added

  output_df <- rmsMD_calculate_raw_p_values(output_df)

  ########## Calculate coef confidence intervals ##########
  # note, don't want to round here for functions that need exp()
  output_df <- rmsMD_calculate_coef_confidence_intervals(output_df)


  ############### make exp_coef if needed ######################
  if(exp_coef){
    for(var in c("coef", "coef_lower95", "coef_upper95")){
      output_df[[paste0("exp_",var)]] <- exp(output_df[[var]])
    }
  }

  ########## Add in ANOVA results for RCS terms (if applicable) ##########
  if (rcs_overallp) {
    output_df <- rmsMD_add_anova_results(output_df, modelfit, rcs_variables)
  }

  ########## Format p-values ##########
  threshold <- 10^(-round_dp_p)
  spr_text_p <- paste("%.", round_dp_p, "f", sep = "")
  output_df$Pvalue <- ifelse(output_df$p_values_raw < threshold,
                             paste0("<", format(threshold, scientific = FALSE)),
                             sprintf(spr_text_p, output_df$p_values_raw))

  ############# make list of effect estimate columns to output ###########
  # key_vars are those that will be formatted with round_dp_coef and
  # combined with CI

  if(exp_coef){
    key_vars <- c("exp_coef","exp_coef_lower95","exp_coef_upper95")
  } else {
    key_vars <- c("coef","coef_lower95","coef_upper95")
  }


  ########## format effect estimates and confidence intervals ############
  # dynamic based on key vars

  output_df <- rmsMD_format_column_output(output_df,
                                          key_vars,
                                          round_dp_coef)

  ##################### make a combined CI column ###############################
  # dynamic based on key_vars

  if(combine_ci){
    output_df <- rmsMD_combine_CI_for_output(output_df,
                                             key_vars)
  }


  ########## Reset row names to numeric indices ##########
  rownames(output_df) <- NULL

  ########## Return final output ##########
  # Use the new helper function to format the final output
  ## will need to change quite a bit with exp_coef stuff
  final_output <- rmsMD_format_final_output(output_df, fullmodel,
                                            combine_ci, exp_coef, key_vars)

  if(exp_coef & exists("exp_coef_name")){
    colnames(final_output) <- gsub("exp_coef", exp_coef_name, colnames(final_output))
  }


  # Return the formatted output
  return(final_output)
}

