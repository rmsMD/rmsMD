# Create model summary for rms models

The `modelsummary_rms` function processes the output from models fitted
using the `rms` package and generates a summarized dataframe of the
results. This summary is tailored for publication in medical journals,
presenting effect estimates, confidence intervals, and p-values.

## Usage

``` r
modelsummary_rms(
  modelfit,
  combine_ci = TRUE,
  round_dp_coef = 3,
  round_dp_p = 3,
  rcs_overallp = TRUE,
  hide_rcs_coef = TRUE,
  exp_coef = NULL,
  fullmodel = FALSE,
  MI_lrt = FALSE
)
```

## Arguments

- modelfit:

  The output from an rms model.

- combine_ci:

  If `TRUE`, combines the effect estimates and 95% confidence intervals
  into a single column. Default is `TRUE`.

- round_dp_coef:

  Specifies the number of decimal places to display for the effect
  estimates. Default is `3`.

- round_dp_p:

  Specifies the number of decimal places to display for P values.
  Default is `3`.

- rcs_overallp:

  If `TRUE`, provides an overall P value for Restricted Cubic Spline
  (RCS) terms, sourced from `anova(modelfit)`. Automatically selects
  appropriate test (LR, F or Wald)

- hide_rcs_coef:

  If `TRUE`, hides the individual coefficients for Restricted Cubic
  Spline (RCS) variables.

- exp_coef:

  If `TRUE`, outputs the exponentiated coefficients (`exp(coef)`) as the
  effect estimates. Applicable only for model types other than `ols`,
  `lrm`, or `cph`. If `NULL`, no exponentiation is performed. Default is
  `NULL`.

- fullmodel:

  If `TRUE`, includes all intermediate steps in the summary, allowing
  users to verify and compare with standard model outputs.

- MI_lrt:

  If `TRUE` then overall p-values for RCS terms from models with
  multiple imputed data from `fit.mult.impute` will represent likelihood
  ratio chi-square tests from
  [`rms::processMI()`](https://rdrr.io/pkg/rms/man/processMI.html),
  rather than Wald tests.

## Value

Returns a dataframe of results. This can easily be outputted to word
using packages such as flextable and officer.

## Examples

``` r
# For detailed examples please see the provided vignettes
```
