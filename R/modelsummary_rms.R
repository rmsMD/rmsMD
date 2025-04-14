#' @title Create model summary for rms models
#'
#' @description The `modelsummary_rms` function processes the output from models fitted using the `rms` package and generates a summarized dataframe of the results.
#' This summary is tailored for publication in medical journals, presenting effect estimates, confidence intervals, and p-values.
#'
#' @param modelfit The output from an rms model.
#' @param combine_ci If `TRUE`, combines the effect estimates and 95% confidence intervals into a single column. Default is `TRUE`.
#' @param round_dp_coef Specifies the number of decimal places to display for the effect estimates. Default is `3`.
#' @param round_dp_p Specifies the number of decimal places to display for P values. Default is `3`.
#' @param rcs_overallp If `TRUE`, provides an overall P value for Restricted Cubic Spline (RCS) terms, sourced from `anova(modelfit)`.
#' @param hide_rcs_coef If `TRUE`, hides the individual coefficients for Restricted Cubic Spline (RCS) variables.
#' @param exp_coef If `TRUE`, outputs the exponentiated coefficients (`exp(coef)`) as the effect estimates. Applicable only for model types other than `ols`, `lrm`, or `cph`. If `NULL`, no exponentiation is performed. Default is `NULL`.
#' @param fullmodel If `TRUE`, includes all intermediate steps in the summary, allowing users to verify and compare with standard model outputs.
#'
#' @return Returns a dataframe of results. This can easily be outputted to word using
#' packages such as flextable and officer.
#'
#' @import rms
#' @importFrom stats anova coef pnorm qnorm terms vcov
#'
#' @examples
#' # For detailed examples please see the provided vignettes
#'
#' @export
#'
#'
modelsummary_rms <- function(modelfit,
                             combine_ci = TRUE,
                             round_dp_coef = 3,
                             round_dp_p = 3,
                             rcs_overallp = TRUE,
                             hide_rcs_coef = TRUE,
                             exp_coef = NULL,
                             fullmodel = FALSE) {

  ########## add some logic here that looks at the model types via class and gives
  ########## you what the combine CI names etc will be (ie the HR OR etc)
  ######### and also sets exp_coef to true or false, or it is isn't ols cph or lrm
  ######### gives an error message saying exp_coef must be specified

  # Warning if modelfit isn't an rms object
  if (!inherits(modelfit, "rms")) {
    # Check if the user explicitly provided an exp_coef argument
    user_set_exp_coef <- "exp_coef" %in% names(match.call())
    if (!user_set_exp_coef) {
      stop("The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output.")
    }
  }

  ########## defining arguments based on model class ##########

  if (inherits(modelfit, "ols")) {
    exp_coef <- FALSE
  } else if (inherits(modelfit, "lrm")) {
    exp_coef <- TRUE
    exp_coef_name <- "OR"
  } else if (inherits(modelfit, "cph")) {
    exp_coef <- TRUE
    exp_coef_name <- "HR"
  } else if(is.null(exp_coef)){
    stop("Model not ols, lrm or cph. You must specify exp_coef argument to determine table output.")
  }

  # this checks if rcs_overallp is set to TRUE by default or if the user specifically set it to TRUE
  user_set_rcs <- "rcs_overallp" %in% names(match.call())

  if (!inherits(modelfit, "rms")) {
    rcs_overallp <- FALSE
    hide_rcs_coef <- FALSE
  } else {
    no_rcs <- all(unlist(modelfit$Design$nonlinear) == FALSE)
    if (no_rcs) {
      if (user_set_rcs && rcs_overallp) {
        warning("rcs_overallp was set to TRUE by the user but no RCS terms were found in the model fit. Setting rcs_overallp to FALSE.")
      }
      rcs_overallp <- FALSE
      hide_rcs_coef <- FALSE
    }
  }

  # Ensure rcs_overallp is TRUE if hide_rcs_coef is TRUE
  if (hide_rcs_coef && !rcs_overallp) {
    warning("When hiding RCS coefficients (hide_rcs_coef = TRUE),\nthe overall RCS p-value should be provided (rcs_overallp = TRUE).\nWe recommend you have rcs_overallp as TRUE if hide_rcs_coef is TRUE")
  }

  ########## Extract coefficients and standard errors ##########
  coef_se_list <- rmsMD_extract_coef_and_se(modelfit) # Helper function to extract coefficients and SE
  coef_values <- coef_se_list$coef               # Extracted coefficients
  se_values <- coef_se_list$se                   # Extracted standard errors

  ########## Handle RCS terms (if applicable) ##########
  if (rcs_overallp) {
    # Extract indices and terms associated with RCS from the model design
    nonlinear_res <- modelfit$Design$nonlinear

    # Check if there are any RCS terms
    if (is.null(nonlinear_res) || all(!sapply(nonlinear_res, any))) {
      stop("rcs_overallp set to TRUE but no RCS terms found in the model fit.")
    }

    # Identify terms with nonlinear components
    int_spline_indices <- sapply(nonlinear_res, any)
    any_rcs_coef_index <- unlist(modelfit$assign[int_spline_indices])  # Indices of coefficients with spline components

    # Get the names of terms with nonlinear components
    rcs_terms_incl_interaction <- modelfit$Design$name[int_spline_indices]

    # Remove RCS coefficients if hide_rcs_coef is TRUE
    if (hide_rcs_coef) {
      coef_values <- coef_values[-any_rcs_coef_index]
      se_values <- se_values[-any_rcs_coef_index]
    }

    # Generate anova results for RCS terms if needed
    if (rcs_overallp) {
      anova_result <- do.call(anova, c(
        list(modelfit),
        as.list(rcs_terms_incl_interaction),
        list(india = FALSE, indnl = FALSE)
      ))
      rownames(anova_result)  # Print or process the ANOVA results if necessary
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
    # Generate anova results for RCS terms
    anova_result <- do.call(anova, c(
      list(modelfit),
      as.list(rcs_terms_incl_interaction),
      list(india = FALSE, indnl = FALSE)
    ))

    # Get list of variable names from the model
    model_vars <- modelfit$Design$name

    # Get all row names from anova result
    anova_rows <- rownames(anova_result)

    # Identify rows that have 'TOTAL', 'ERROR' or 'REGRESSION' in their names
    suspicious_rows <- anova_rows[grepl("TOTAL|ERROR|REGRESSION", anova_rows, ignore.case = TRUE)]

    # Remove any that are actually model variables
    rows_to_remove <- setdiff(suspicious_rows, model_vars)

    # Keep only rows that are not in the final removal list
    keep_rows <- !(anova_rows %in% rows_to_remove)
    filtered_anova_result <- anova_result[keep_rows, , drop = FALSE]

    # Extract the relevant rows and p-values for RCS terms
    anova_df <- data.frame(
      variable = paste0("RCSoverallP: ", rownames(filtered_anova_result)),
      p_values_raw = filtered_anova_result[, "P"],
      stringsAsFactors = FALSE
    )

    # Add missing columns to match `output_df` structure
    output_columns <- colnames(output_df)
    for (col in output_columns) {
      if (!col %in% colnames(anova_df)) {
        anova_df[[col]] <- NA
      }
    }

    # Merge the filtered ANOVA results with the main output
    output_df <- rbind(output_df, anova_df[, output_columns])
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

  ########## Return final output ##########
  # Use the new helper function to format the final output
  ## will need to change quite a bit with exp_coef stuff
  final_output <- rmsMD_format_final_output(output_df, fullmodel,
                                            combine_ci, exp_coef, key_vars)

  if(exp_coef & exists("exp_coef_name")){
    colnames(final_output) <- gsub("exp_coef", exp_coef_name, colnames(final_output))
  }

  # ensure no rownames
  rownames(final_output) <- NULL

  # Return the formatted output
  return(final_output)
}

