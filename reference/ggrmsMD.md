# Create plots for RCS variables from an `rms` model

The `ggrmsMD` function processes the output from models fitted using the
`rms` package and produces one or more `ggplot2` objects visualising
restricted cubic splines (RCS). The function detects RCS terms in the
model and plots them all, with a suitable y-axis selected based on the
model type. This outputs a list of plots, or a multi-panel figure using
the `combined` argument. As outputs are `ggplot` objects they can easily
be further customised by the user.

## Usage

``` r
ggrmsMD(
  modelfit,
  data,
  noeffline = TRUE,
  shade_inferior = "none",
  combined = TRUE,
  ylab = NULL,
  xlabs = NULL,
  titles = NULL,
  ylim = NULL,
  log_y = FALSE,
  log_y_breaks = NULL,
  xlims = NULL,
  log_x_vars = NULL,
  log_x_breaks = NULL,
  lrm_prob = FALSE,
  var = NULL,
  np = 400,
  ...
)
```

## Arguments

- modelfit:

  A model object from `ols`, `lrm`, or `cph` (from the `rms` package).

- data:

  The dataset used to fit the model.

- noeffline:

  Logical. If `TRUE` (default), adds a horizontal dashed line at 1 for
  odds/hazard ratio plots.

- shade_inferior:

  Character. Options are `"none"` (default), `"higher"`, or `"lower"`.
  Applies red/green shading above or below 1 on the y-axis to indicate
  worse/better outcomes.

- combined:

  Logical. If `TRUE`, returns a single multi-panel plot using
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

- ylab:

  Optional character. Override the default y-axis label.

- xlabs:

  A named list of x-axis labels for each variable. E.g.,
  `list(age = "Age (years)", bmi = "BMI (kg/m²"))`.

- titles:

  A named list of plot titles for each variable.

- ylim:

  Numeric vector (length 2). y-axis limits applied to all plots. E.g.,
  `c(0.5, 2)`.

- log_y:

  Logical. If `TRUE`, y-axis is log10-transformed.

- log_y_breaks:

  Optional numeric vector specifying y-axis tick marks when
  `log_y = TRUE`. E.g., `c(0.25, 0.5, 1, 2, 4)`.

- xlims:

  A named list of x-axis limits per variable. E.g.,
  `list(age = c(20, 80))`.

- log_x_vars:

  Character vector. Names of variables for which x-axis should be
  log10-transformed.

- log_x_breaks:

  A named list specifying x-axis tick marks for variables with
  log10-transformed x-axis.

- lrm_prob:

  Logical. If `TRUE` and model is `lrm`, plots predicted probabilities
  instead of odds ratios.

- var:

  Character vector. Optional. Variables to plot. If `NULL` (default),
  all RCS variables in the model will be plotted.

- np:

  Integer. Number of points used to predict spline curves. Default is
  `400`. Consider increasing when using log-transformed x-axes.

- ...:

  Additional arguments passed to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
  when `combined = TRUE`.

## Value

A `ggplot` object (if one variable is plotted), a list of `ggplot`
objects (if multiple variables), or a single combined `cowplot` plot if
`combined = TRUE`.

## Examples

``` r
# For details examples and plots please see the provided vignettes
```
