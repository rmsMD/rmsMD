# Further details modelsummary_rms

``` r
library(rms)
library(rmsMD)
```

## Introduction

The `modelsummary_rms` function is designed to process output from
models fitted using the **rms** package and generate a summarised
dataframe of the results. The goal is to produce publication-ready
summaries of the models.

For standard use with `rms` models that use restricted cubic splines,
please refer to the vignette
**Standard_workflow_with_restricted_cubic_splines**.

For these vignettes we will use a simulated dataset to predict the
impact of age, BMI, Sex and Smoking status on outcome after surgery. The
models are for illustration purposes only.

``` r
# Load in the simulated data
data <- simulated_rmsMD_data()

# Set the datadist which is required for rms modelling 
# (these two lines are standard)
dd <- datadist(data)    
options(datadist='dd') 
```

## Basic Usage

Here is a simple example using a linear regression model (“ordinary
least squares”; OLS). We use simulated data for the impact of patient
factors on length of stay after an operation.

The output dataframe contains the estimated coefficients, their 95%
confidence intervals, and the associated p-values. These are in a
publication ready format.

``` r
# Fit a linear regression model using the rms package:
fit_ols <- ols(lengthstay ~ age + 
                 bmi + 
                 sex + 
                 smoking, 
               data = data)

# Generate a model summary and display it as a dataframe
modelsummary_rms(fit_ols)
#>          variable               coef_95CI Pvalue
#> 1             age  0.306 (0.293 to 0.319) <0.001
#> 2             bmi  0.095 (0.056 to 0.135) <0.001
#> 3      sex=Female                     Ref      -
#> 4        sex=Male 0.271 (-0.040 to 0.582)  0.087
#> 5   smoking=Never                     Ref      -
#> 6  smoking=Former 0.131 (-0.248 to 0.510)  0.499
#> 7 smoking=Current  0.431 (0.050 to 0.812)  0.027

# rmsMD dataframe as a table
knitr::kable(modelsummary_rms(fit_ols))
```

| variable        | coef_95CI               | Pvalue  |
|:----------------|:------------------------|:--------|
| age             | 0.306 (0.293 to 0.319)  | \<0.001 |
| bmi             | 0.095 (0.056 to 0.135)  | \<0.001 |
| sex=Female      | Ref                     | \-      |
| sex=Male        | 0.271 (-0.040 to 0.582) | 0.087   |
| smoking=Never   | Ref                     | \-      |
| smoking=Former  | 0.131 (-0.248 to 0.510) | 0.499   |
| smoking=Current | 0.431 (0.050 to 0.812)  | 0.027   |

### Customising the modelsummary_rms output

By default, the function uses the following stylistic settings:

- **combine_ci = TRUE:** Combines the effect estimate and the 95%
  confidence interval into a single column.
- **round_dp_coef = 3:** Rounds the effect estimates to three decimal
  places.
- **round_dp_p = 3:** Rounds the p-values to three decimal places.

You can modify these defaults to adjust the appearance of the output.

``` r
# Generate a model summary with custom styling options
# & store as summary_custom
summary_custom <- modelsummary_rms(fit_ols, 
                                   combine_ci = FALSE, 
                                   round_dp_coef = 2, 
                                   round_dp_p = 4)

# to display the dataframe as a table
knitr::kable(summary_custom)
```

| variable        | coef | coef_lower95 | coef_upper95 | Pvalue   |
|:----------------|:-----|:-------------|:-------------|:---------|
| age             | 0.31 | 0.29         | 0.32         | \<0.0001 |
| bmi             | 0.10 | 0.06         | 0.13         | \<0.0001 |
| sex=Female      | Ref  | Ref          | Ref          | \-       |
| sex=Male        | 0.27 | -0.04        | 0.58         | 0.0874   |
| smoking=Never   | Ref  | Ref          | Ref          | \-       |
| smoking=Former  | 0.13 | -0.25        | 0.51         | 0.4990   |
| smoking=Current | 0.43 | 0.05         | 0.81         | 0.0267   |

### Exponentiating Coefficients (including hazard ratios and odds ratios)

Exponentiating the coefficients of certain models makes the
interpretation more intuitive (e.g. as odds ratios in logistic
regression and hazard ratios in Cox models).

The **modelsummary_rms** package does this automatically for the core
**rms** models `ols`, `lrm`, and `cph`. This ensures OR and HR are
displayed for logistic regression and Cox regression models
respectively. Below is an example using **modelsummary_rms** on an
**rms** logistic regression model for postoperative complications. Note
this automatically provides OR:

``` r
# fitting the model
fit_lrm <- lrm(majorcomplication ~ age + 
                 bmi + 
                 sex + 
                 smoking, 
               data = data)

# rmsMD summary
modelsummary_rms(fit_lrm)
#>          variable                OR_95CI Pvalue
#> 1             age 1.025 (1.018 to 1.031) <0.001
#> 2             bmi 0.996 (0.978 to 1.016)  0.717
#> 3      sex=Female                    Ref      -
#> 4        sex=Male 1.080 (0.928 to 1.257)  0.319
#> 5   smoking=Never                    Ref      -
#> 6  smoking=Former 0.977 (0.799 to 1.196)  0.824
#> 7 smoking=Current 2.063 (1.719 to 2.476) <0.001

# displaying as a table
knitr::kable(modelsummary_rms(fit_lrm))
```

| variable        | OR_95CI                | Pvalue  |
|:----------------|:-----------------------|:--------|
| age             | 1.025 (1.018 to 1.031) | \<0.001 |
| bmi             | 0.996 (0.978 to 1.016) | 0.717   |
| sex=Female      | Ref                    | \-      |
| sex=Male        | 1.080 (0.928 to 1.257) | 0.319   |
| smoking=Never   | Ref                    | \-      |
| smoking=Former  | 0.977 (0.799 to 1.196) | 0.824   |
| smoking=Current | 2.063 (1.719 to 2.476) | \<0.001 |

The **modelsummary_rms** from **rmsMD** package is also capable of
working with non-rms models, such as those fitted using base R functions
like lm(). However, in these cases the package does not automatically
determine the appropriate value for exp_coef, so it must be set
manually.

For example, when using a linear model (where exponentiation of
coefficients is not required), you should explicitly set exp_coef =
FALSE.

``` r
# Fit a simple linear model using lm() from base R 
# (an example model fit without using rms package)
fit_lm <- lm(majorcomplication ~ age + 
               bmi + 
               sex + 
               smoking, 
             data = data)

# Generate a model summary for the non-RMS model 
# by explicitly setting exp_coef = FALSE
modelsummary_rms(fit_lm, exp_coef = FALSE)
#>         variable                coef_95CI Pvalue
#> 1    (Intercept) -0.023 (-0.103 to 0.057)  0.569
#> 2            age   0.003 (0.002 to 0.004) <0.001
#> 3            bmi -0.001 (-0.003 to 0.002)  0.688
#> 4        sexMale  0.011 (-0.010 to 0.031)  0.311
#> 5  smokingFormer -0.003 (-0.028 to 0.022)  0.822
#> 6 smokingCurrent   0.105 (0.080 to 0.130) <0.001

# display rmsMD results as a table
knitr::kable(modelsummary_rms(fit_lm, exp_coef = FALSE))
```

| variable       | coef_95CI                | Pvalue  |
|:---------------|:-------------------------|:--------|
| (Intercept)    | -0.023 (-0.103 to 0.057) | 0.569   |
| age            | 0.003 (0.002 to 0.004)   | \<0.001 |
| bmi            | -0.001 (-0.003 to 0.002) | 0.688   |
| sexMale        | 0.011 (-0.010 to 0.031)  | 0.311   |
| smokingFormer  | -0.003 (-0.028 to 0.022) | 0.822   |
| smokingCurrent | 0.105 (0.080 to 0.130)   | \<0.001 |

## Restricted Cubic Splines

Restricted Cubic Splines (RCS) are a flexible modelling tool used to
capture non-linear relationships between predictors and outcomes. In
medicine, for the majority of continuous variables (e.g. age, blood
pressure, or biomarker levels) the assumption of linearity may not hold.
A key highlight of the **rms** package is the ability to analyse
variables using RCS.

The **rmsMD** package is designed to report and summarise models that
include RCS terms. Individual coefficients for RCS terms are difficult
to interpret in isolation. Instead, an overall p-value can be generated
to assess whether the overall relationship between the RCS variable and
outcome is significant. By default `modelsummary_rms` removes the
individual RCS coefficients, replacing them with the overall p-value for
that variable. We recommend that these are then plotted using the
`ggrmsM` function, shown below.

- **Display an overall p-value for the spline terms using the
  `rcs_overallp` option.**  
  When this option is set to `TRUE` (which is the default), the function
  computes a single p-value that tests the overall significance of the
  spline terms for each variable. This overall p-value provides insight
  into whether the relationship between the predictor and the dependent
  variable is significant.

- **Hide the individual spline coefficients using the `hide_rcs_coef`
  option.**  
  Hiding the individual spline coefficients can be advantageous because
  these lack straightforward clinical interpretation. Instead, the focus
  is on the overall association captured by all RCS terms for that
  specific variable. This helps simplify the output. If the variable has
  a signficant association with outcome, we recommend plotting this
  relationship.

Here is an example model predicting occurence of complications after
surgery (binary), with the continuous variables age and BMI modelled
using restricted cubic splines with 4 knots:

``` r
# Fit a logistic regression model including a RCS for Age & BMI (with 4 knots)
fit_lrm <- lrm(
        majorcomplication ~ 
                rcs(age, 4) +   # Age modelled using RCS with 4 knots
                rcs(bmi, 4) +   # BMI also modelled with RCS with 4 knots
                sex +           # Binary variable for sex
                smoking,        # Categorical variable for smoking status
                data = data,
                # set x = TRUE, y = TRUE to allow 
                # subsequent LR tests for lrm() and cph() models
                x = TRUE, y = TRUE             
)

# Generate an rmsMD model summary using default settings
modelsummary_rms(fit_lrm)
#>           variable                OR_95CI Pvalue
#> 1       sex=Female                    Ref      -
#> 2         sex=Male 1.078 (0.927 to 1.255)  0.330
#> 3    smoking=Never                    Ref      -
#> 4   smoking=Former 0.986 (0.806 to 1.207)  0.892
#> 5  smoking=Current 2.078 (1.731 to 2.496) <0.001
#> 6 RCSoverallP: age                LR test <0.001
#> 7 RCSoverallP: bmi                LR test  0.034

# Outputting this as a table
knitr::kable(modelsummary_rms(fit_lrm))
```

| variable         | OR_95CI                | Pvalue  |
|:-----------------|:-----------------------|:--------|
| sex=Female       | Ref                    | \-      |
| sex=Male         | 1.078 (0.927 to 1.255) | 0.330   |
| smoking=Never    | Ref                    | \-      |
| smoking=Former   | 0.986 (0.806 to 1.207) | 0.892   |
| smoking=Current  | 2.078 (1.731 to 2.496) | \<0.001 |
| RCSoverallP: age | LR test                | \<0.001 |
| RCSoverallP: bmi | LR test                | 0.034   |

### Displaying RCS individual coefficients with modelsummary_rms

Displaying individual RCS coefficients is not the default behaviour. If
individual RCS coefficients are required, these can be added in by
setting `hide_rcs_coef` to `FALSE`:

``` r

# Generate a model summary with rcs_overallp set to TRUE 
# and hide_rcs_coef set to TRUE
modelsummary_rms(fit_lrm, hide_rcs_coef = FALSE)
#>            variable                OR_95CI Pvalue
#> 1               age 1.005 (0.980 to 1.030)  0.701
#> 2              age' 1.061 (0.992 to 1.134)  0.085
#> 3             age'' 0.799 (0.617 to 1.033)  0.087
#> 4               bmi 0.937 (0.881 to 0.996)  0.038
#> 5              bmi' 1.103 (0.923 to 1.320)  0.281
#> 6             bmi'' 0.855 (0.408 to 1.792)  0.677
#> 7        sex=Female                    Ref      -
#> 8          sex=Male 1.078 (0.927 to 1.255)  0.330
#> 9     smoking=Never                    Ref      -
#> 10   smoking=Former 0.986 (0.806 to 1.207)  0.892
#> 11  smoking=Current 2.078 (1.731 to 2.496) <0.001
#> 12 RCSoverallP: age                LR test <0.001
#> 13 RCSoverallP: bmi                LR test  0.034

# Outputting this as a table
knitr::kable(modelsummary_rms(fit_lrm, hide_rcs_coef = FALSE))
```

| variable         | OR_95CI                | Pvalue  |
|:-----------------|:-----------------------|:--------|
| age              | 1.005 (0.980 to 1.030) | 0.701   |
| age’             | 1.061 (0.992 to 1.134) | 0.085   |
| age’’            | 0.799 (0.617 to 1.033) | 0.087   |
| bmi              | 0.937 (0.881 to 0.996) | 0.038   |
| bmi’             | 1.103 (0.923 to 1.320) | 0.281   |
| bmi’’            | 0.855 (0.408 to 1.792) | 0.677   |
| sex=Female       | Ref                    | \-      |
| sex=Male         | 1.078 (0.927 to 1.255) | 0.330   |
| smoking=Never    | Ref                    | \-      |
| smoking=Former   | 0.986 (0.806 to 1.207) | 0.892   |
| smoking=Current  | 2.078 (1.731 to 2.496) | \<0.001 |
| RCSoverallP: age | LR test                | \<0.001 |
| RCSoverallP: bmi | LR test                | 0.034   |

If overall p-values for the variables modelled with RCS are not wanted,
`rcs_overallp` can be set to `FALSE`:

``` r
# Fit an OLS model including a restricted cubic spline for Age (with 4 knots)
summary_spline_hide <- modelsummary_rms(fit_lrm, 
                                        hide_rcs_coef = FALSE,
                                        rcs_overallp = FALSE)

# Outputting this as a table
knitr::kable(summary_spline_hide)
```

| variable        | OR_95CI                | Pvalue  |
|:----------------|:-----------------------|:--------|
| age             | 1.005 (0.980 to 1.030) | 0.701   |
| age’            | 1.061 (0.992 to 1.134) | 0.085   |
| age’’           | 0.799 (0.617 to 1.033) | 0.087   |
| bmi             | 0.937 (0.881 to 0.996) | 0.038   |
| bmi’            | 1.103 (0.923 to 1.320) | 0.281   |
| bmi’’           | 0.855 (0.408 to 1.792) | 0.677   |
| sex=Female      | Ref                    | \-      |
| sex=Male        | 1.078 (0.927 to 1.255) | 0.330   |
| smoking=Never   | Ref                    | \-      |
| smoking=Former  | 0.986 (0.806 to 1.207) | 0.892   |
| smoking=Current | 2.078 (1.731 to 2.496) | \<0.001 |

### Interactions with RCS variables

The **rms** package allows interactions with variables modelled using
restricted cubic splines. In this setting, the individual coefficients
for RCS terms and their interactions are difficult to interpret.
`modelsummary_rms` handles this situation by providing overall p-values
for RCS variables (which give the overall p-value taking into account
all spline terms and all of their interaction terms), and overall
p-values for the interactions (takes into account linear and non-linear
terms), instead of the individual coefficients. As above, this can be
altered by changing `rcs_overallp` and `hide_rcs_coef`.

``` r
# Fit an OLS model with a restricted cubic spline for Age 
# and an interaction between Age and Exer.
fit_lrm_interaction <- lrm(majorcomplication ~ 
                             rcs(age,4)*smoking + 
                             rcs(bmi,4) + 
                             sex + 
                             smoking, 
                           data = data,
                           x = TRUE, y = TRUE)

# Generate a model summary with default RCS output
modelsummary_rms(fit_lrm_interaction)
#>                                                    variable
#> 1                                             smoking=Never
#> 2                                            smoking=Former
#> 3                                           smoking=Current
#> 4                                                sex=Female
#> 5                                                  sex=Male
#> 6           RCSoverallP: age  (Factor+Higher Order Factors)
#> 7                                          RCSoverallP: bmi
#> 8 RCSoverallP: age * smoking  (Factor+Higher Order Factors)
#>                  OR_95CI Pvalue
#> 1                    Ref      -
#> 2 0.263 (0.023 to 3.061)  0.286
#> 3 0.490 (0.061 to 3.908)  0.501
#> 4                    Ref      -
#> 5 1.069 (0.919 to 1.245)  0.386
#> 6                LR test <0.001
#> 7                LR test  0.038
#> 8                LR test  0.063

# Format the output as a nice table
knitr::kable(modelsummary_rms(fit_lrm_interaction))
```

| variable                                                  | OR_95CI                | Pvalue  |
|:----------------------------------------------------------|:-----------------------|:--------|
| smoking=Never                                             | Ref                    | \-      |
| smoking=Former                                            | 0.263 (0.023 to 3.061) | 0.286   |
| smoking=Current                                           | 0.490 (0.061 to 3.908) | 0.501   |
| sex=Female                                                | Ref                    | \-      |
| sex=Male                                                  | 1.069 (0.919 to 1.245) | 0.386   |
| RCSoverallP: age (Factor+Higher Order Factors)            | LR test                | \<0.001 |
| RCSoverallP: bmi                                          | LR test                | 0.038   |
| RCSoverallP: age \* smoking (Factor+Higher Order Factors) | LR test                | 0.063   |

To get further information, for example for the overall effect of
smoking (the variable interacted with the spine for age here), use
`anova` as below:

``` r
anova(fit_lrm_interaction, test = "LR")
#>                 Likelihood Ratio Statistics          Response: majorcomplication 
#> 
#>  Factor                                       Chi-Square d.f. P     
#>  age  (Factor+Higher Order Factors)            72.23      9   <.0001
#>   All Interactions                             11.95      6   0.0630
#>   Nonlinear (Factor+Higher Order Factors)      12.39      6   0.0538
#>  smoking  (Factor+Higher Order Factors)       100.20      8   <.0001
#>   All Interactions                             11.95      6   0.0630
#>  bmi                                            8.42      3   0.0380
#>   Nonlinear                                     8.30      2   0.0157
#>  sex                                            0.75      1   0.3863
#>  age * smoking  (Factor+Higher Order Factors)  11.95      6   0.0630
#>   Nonlinear                                     9.53      4   0.0490
#>   Nonlinear Interaction : f(A,B) vs. AB         9.53      4   0.0490
#>  TOTAL NONLINEAR                               20.94      8   0.0073
#>  TOTAL NONLINEAR + INTERACTION                 23.49     10   0.0091
#>  TOTAL                                        170.84     15   <.0001
```
