
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


#' Simulated data for the Vignette
#'
#' Formats the final output data frame, including the option to exclude intercept and select columns.
#'
#' @export

simulated_rmsMD_data <- function(type = c("complete_case", "missing_for_MI")){
  type <- match.arg(type) # ensures type is a single string from the choices
  set.seed(124)

  # Simulate data
  # major complications after surgery
  n <- 5000
  age <- round(rnorm(n, mean = 50, sd = 12),1)
  bmi <- round(rnorm(n, mean = 25, sd = 4),1)
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  smoking <- factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE),
    levels = c("Never", "Former", "Current")  # sets "Never" as the reference
  )

  # Simulate linear predictor. Age linear, BMI U-shaped, smoking increases risk.
  lp <- -3 + 0.02 * age + 0.0045 * (bmi - 25.5)^2 +
    0.75 * (smoking == "Current")

  # Convert linear predictor to probability and simulate major complication
  p <- 1 / (1 + exp(-lp))
  majorcomplication <- rbinom(n, 1, prob = p)

  # simulate time to event using same lp
  haz <- 0.03  * exp(lp)
  survtime <- rexp(n, rate = haz)
  censor_time <- quantile(survtime, probs = 0.9)
  event <- as.integer(survtime <= censor_time)
  observed_time <- pmin(survtime, censor_time)

  length_of_stay <- 15 + 0.3 * age + 0.1 * bmi +
    0.04 * (smoking == "Current") + 3 * majorcomplication +
    rnorm(n, mean = 0, sd = 5.5)

  # Create data frame and clean environment
  data <- data.frame(age = age, bmi = bmi, sex = sex, smoking = smoking,
                     majorcomplication = majorcomplication, lengthstay = length_of_stay, time = observed_time, event = event)

  if(type == "complete_case"){
    return(data)
  } else if(type == "missing_for_MI"){
    # Randomly introduce 10% missing values in each predictor column
    predictors <- c("age", "bmi", "sex", "smoking")
    for (col in predictors) {
      na_idx <- sample(1:n, size = floor(0.1 * n), replace = FALSE)
      data[na_idx, col] <- NA
    }
    return(data)
  }

}
