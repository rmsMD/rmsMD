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
#' @param MI_lrt If `TRUE` then overall p-values for RCS terms from models with multiple imputed data from `fit.mult.impute` with `lrt = TRUE` will represent likelihood ratio chi-square tests from `rms::processMI()`, rather than Wald tests.
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
                             fullmodel = FALSE,
                             MI_lrt = FALSE) {

  # Warning if modelfit isn't an rms object and no exp_coef given to specify if coef or exp(coef) should be given
  if (!inherits(modelfit, "rms") && !"exp_coef" %in% names(match.call())) {
    stop(
      "The model fit does not belong to the 'rms' class.\nYou must specify the exp_coef argument to determine table output."
    )
  }

  # note the categorical variable ref groups won't pull if datadist not set
  if (inherits(modelfit, "rms") && is.null(getOption("datadist"))) {
    warning(
      "Please ensure data distribution was set before the rms model was fit\n",
      "using:\n  dd <- datadist(data)\n  options(datadist = 'dd')"
    )
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
    stop("Model not ols, lrm or cph.\nYou must specify the exp_coef argument to determine table output.")
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
        warning(
          "rcs_overallp was set to TRUE by the user, but no RCS terms were found in the model fit.\nSetting rcs_overallp to FALSE."
        )
      }
      #set defaults to false if model happpens to have no RCS
      rcs_overallp <- FALSE
      hide_rcs_coef <- FALSE
    }
  }

  # Ensure rcs_overallp is TRUE if hide_rcs_coef is TRUE
  if (hide_rcs_coef && !rcs_overallp) {
    warning(
      "When hiding RCS coefficients (hide_rcs_coef = TRUE),\n",
      "the overall RCS p-value should be provided (rcs_overallp = TRUE).\n",
      "We recommend you set rcs_overallp = TRUE if hide_rcs_coef = TRUE."
    )
  }

  ########## Extract coefficients and standard errors ##########
  coef_values <- coef(modelfit)
  se_values <- sqrt(diag(vcov(modelfit)))
  # Ensure standard errors are in the same order as coefficients
  se_values <- se_values[names(coef_values)]

  ########## Handle RCS terms (if applicable) ##########
  if (rcs_overallp) {
    # Extract indices and terms associated with RCS from the model design
    nonlinear_res <- modelfit$Design$nonlinear
    int_spline_indices <- sapply(nonlinear_res, any)
    any_rcs_coef_index <- unlist(modelfit$assign[int_spline_indices])  # Indices of coefficients with spline components

    # Get the names of terms with nonlinear components
    rcs_terms_incl_interaction <- modelfit$Design$name[int_spline_indices]

    # Remove RCS coefficients if hide_rcs_coef is TRUE
    if (hide_rcs_coef) {
      coef_values <- coef_values[-any_rcs_coef_index]
      se_values <- se_values[-any_rcs_coef_index]
    }
  }

  ########## make coef and SE into a dataframe ##########

  output_df <- data.frame(
    variable = names(coef_values), # Names of the variables
    coef = coef_values, # Rounded coefficients
    SE = se_values, # Standard errors
    stringsAsFactors = FALSE # Do not treat character columns as factors
  )

  ############### p-values and CI   ###############

  # note, don't want to round here, as still have to add in anova p vals and deal with exp(coef)
  output_df$p_values_raw <- 2 * (1 - pnorm(abs(output_df$coef / output_df$SE)))
  output_df$coef_lower95 <- output_df$coef + qnorm(0.025) * output_df$SE
  output_df$coef_upper95 <- output_df$coef + qnorm(0.975) * output_df$SE

  if(exp_coef){
    for(var in c("coef", "coef_lower95", "coef_upper95")){
      output_df[[paste0("exp_",var)]] <- exp(output_df[[var]])
    }
  }

  ########## Add in anova results for RCS terms (if applicable) ##########

  if (rcs_overallp) {


    # first get the anova_result
    # deals with all combos of MI vs complete case, and ols/lrm/cph. if none of those, defaults to anova.rms default

    test_arg <- NULL

    if(!MI_lrt){
      if(!inherits(modelfit, "fit.mult.impute")){
        if(inherits(modelfit, "ols")){
          # complete case and ols
          test_arg <- "F"
        }
        if(inherits(modelfit, c("lrm", "cph"))) {
          # complete case and lrm/cph
          # attempt to give LR, but if not able then give wald and warning
          if(!is.null(modelfit$x) && !is.null(modelfit$y)){
            test_arg <- "LR"
          } else {
            test_arg <- "Chisq"
            message(
              "RCS overall p-values displayed are from Wald tests.\n",
              "To use the recommended test for this model type (LR test),\n",
              "please set 'x = TRUE, y = TRUE' when fitting the model."
            )
          }
        }
      } else {
        # for fit.mult.impute model fit objects
        # nb if MI_lrt for LR tests this is handled below in a seperate block
        if(inherits(modelfit, "ols")){
          # MI and ols
          test_arg <- "Chisq"
        }
        if(inherits(modelfit, c("lrm", "cph"))) {
          # MI and lrm/cph, but MI_lrt not TRUE
          test_arg <- "Chisq"
          message(
            "RCS overall p-values displayed are from Wald tests.\n",
            "To use LR test set `MI_lrt = TRUE` in modelsummary_rms(), and set ",
            "`lrt = TRUE` in fit.mult.impute() when fitting the model."
          )
        }

      }


      # making anova result based on test_arg
      # if no test_arg then it does the anova.rms defaults
      if(!is.null(test_arg)){
        anova_result <- do.call(anova, c(
          list(modelfit),
          as.list(rcs_terms_incl_interaction),
          list(india = FALSE, indnl = FALSE, test = test_arg)
        ))
      } else {
        # use anova.rms default test as back-up
        anova_result <- do.call(anova, c(
          list(modelfit),
          as.list(rcs_terms_incl_interaction),
          list(india = FALSE, indnl = FALSE)
        ))
      }

    } else {
      # uses processMI for when MI_lrt is TRUE

      if (!inherits(modelfit, c("cph","lrm"))) {
        stop("MI_lrt = TRUE is currently available for lrm() and cph() `rms` models only.")
      }

      if (!inherits(modelfit, "fit.mult.impute")) {
        stop(
          "MI_lrt = TRUE was set, but the model object is not a fit.mult.impute() object.\n",
          "MI_lrt is only applicable to fit.mult.impute() objects."
        )
      }

      if(modelfit$fmimethod == "ordinary"){
        stop("MI_lrt = TRUE was set, but when fitting the model with fit.mult.impute(), \n`lrt = TRUE` was not used.")
      }

      anova_result <- processMI(modelfit, "anova")
      anova_rows <- rownames(anova_result)

      # Keep only rows containing any rcs terms (can't pass variables into processMI like you can with anova.rms)
      rows_keep <- anova_rows[
        Reduce(`|`, lapply(rcs_terms_incl_interaction, function(term) grepl(term, anova_rows, fixed = TRUE)))
      ]
      anova_result <- anova_result[rows_keep, , drop = FALSE]

      # just for labelling later
      test_arg <- "LR"

    }

    # Get list of variable names from the model
    model_vars <- modelfit$Design$name

    # Get all row names from anova result
    anova_rows <- rownames(anova_result)

    # doing this approach as variables we want may be re-labelled 'or higher order factor' etc, so can't just use anova_rows %in% rcs_terms_incl_interaction
    rows_rem <- anova_rows[grepl("TOTAL|ERROR|REGRESSION", anova_rows, ignore.case = TRUE)]

    # make sure we aren't getting rid of an actual term
    rows_to_remove <- setdiff(rows_rem, model_vars)

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

  spr_text_coef <- paste0("%.", round_dp_coef, "f")

  for(var in key_vars){
    output_df[[var]] <- ifelse(is.na(output_df[[var]]),
                               NA,
                               sprintf(spr_text_coef, output_df[[var]]))
  }

  ##################### make a combined CI column ###############################
  # dynamic based on key_vars

  if(combine_ci){
    effect_est <- key_vars[1]
    lower95 <- key_vars[2]
    upper95 <- key_vars[3]

    column_name <- paste0(effect_est,"_95CI")

    if(rcs_overallp){
      rcs_test <- if(test_arg == "LR"){
        "LR test"
      } else if(test_arg == "Chisq"){
        "Wald test"
      } else if(test_arg == "F"){
        "F test"
      } else{
        "RCS terms"
      }
    }

    output_df[[column_name]] <- ifelse(is.na(output_df[[effect_est]]),
                                       rcs_test,
                                       paste(output_df[[effect_est]],
                                             " (", output_df[[lower95]],
                                             " to ", output_df[[upper95]], ")", sep = ""))
  }

  ########## adding ref levels for categorical variables ##########

  # nb relies on model structure, so only for main model types
  # nb the combined column and p values already done and formatted
  if (inherits(modelfit, "rms") && any(class(modelfit) %in% c("ols", "lrm", "cph"))){

    # indexes and variables that are categorical
    index_categorical <- modelfit$Design$assume == "category"
    vars_cat <- modelfit$Design$name[index_categorical]

    # getting the reference levels and adding them in
    limits <- modelfit$Design$limits

    for (i in seq_along(vars_cat)) {
      cat_var <- vars_cat[[i]]
      ref_group <- levels(limits[[cat_var]])[1]

      # find first index
      row_index <- which(grepl(paste0("^", cat_var, "="), output_df$variable))[1]

      if (!is.na(row_index)) {
        ref_row <- output_df[1, ]
        ref_row[] <- "Ref"
        ref_row$variable <- paste0(cat_var, "=", ref_group)
        ref_row$Pvalue <- "-"
        output_df <- rbind(
          output_df[seq_len(row_index - 1), ],
          ref_row,
          output_df[seq(row_index, nrow(output_df)), ]
        )
      }

    }


  }

  ########## Return final output ##########
  # helper function to format the final output
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

