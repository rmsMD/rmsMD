#' @title Create plots for RCS variables from an `rms` model
#'
#' @description The `ggrmsMD` function processes the output from models fitted using the `rms` package and produces one or more `ggplot2` objects visualising restricted cubic splines (RCS).
#' The function detects RCS terms in the model and plots them all, with a suitable y-axis selected based on the model type. This outputs a list of plots, or a multi-panel figure using the `combined` argument.
#' As outputs are `ggplot` objects they can easily be further customised by the user.
#'
#' @param modelfit A model fit object from `ols`, `lrm`, or `cph` from the `rms` package.
#' @param data The dataframe which was used to fit the model.
#' @param noeffline Logical. If `TRUE` (default), adds a horizontal reference line at 1 for odds/hazard ratio plots.
#' @param shade_inferior Options are `"none"` (default), `"higher"`, or `"lower"`. Applies red/green shading to indicate worse/better outcomes relative to 1 on the y-axis.
#' @param combined Logical. If `TRUE`, returns a single multi-panel plot using `cowplot::plot_grid()`.
#' @param ylab Character. Override the default y-axis label (applies to all plots).
#' @param xlabs A named list to specify custom x-axis labels for each variable. E.g., `list(variablename = "Desired x axis label (units)", var2 = "variable 2")`.
#' @param titles A named list of titles to be added to each plot. E.g., `list(variablename = "Desired title", var2 = "variable 2 title")`.
#' @param ylim Provide desired y axis limits. E.g., `c(0,3)`. Applies this to all plots using `coord_cartesian()`.
#' @param log_y Logical. If `TRUE`, y-axis is log10-transformed.
#' @param log_y_breaks Numeric vector to specify desired y-axis breaks when `log_y = TRUE`. E.g., `c(0.25,0.5,1,2,4)`.
#' @param xlims A named list of desired x-axis limits per variable. E.g., `list(variablename = c(20, 80), var2 = c(0,15))`.
#' @param lrm_prob Logical. If `TRUE` and model is `lrm`, plots predicted probabilities rather than odds ratios.
#' @param var If `NULL` (default), the function will automatically select RCS variables and plot them. Alternatively, provide a character vector of variables to plot.
#' @param ... Additional arguments passed to `cowplot::plot_grid()` when `combined = TRUE`. E.g, `align = "vh"`.
#'
#' @return A `ggplot` object (if one variable is plotted), a list of `ggplot` objects (if multiple variables), or a single combined `cowplot` plot if `combined = TRUE`.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs annotate geom_hline
#' @importFrom ggplot2 coord_cartesian scale_y_log10 ggtitle theme element_blank
#' @importFrom ggplot2 element_text element_rect element_line
#' @importFrom cowplot plot_grid
#' @importFrom rms Predict datadist
#' @importFrom stats plogis
#' @importFrom rlang sym
#'
#' @examples
#' # For details examples and plots please see the provided vignettes
#'
#' @export
#'

ggrmsMD <- function(modelfit, data,
                    noeffline = TRUE, # set FALSE to get rid of line of no effect
                    shade_inferior = "none", # whether above or below one is shaded red, and other side set green. can be set to "none" "higher" (i.e. higher is inferior/red) "lower"
                    combined = FALSE, # TRUE to return a cowplot plot_grid combined plot (rather than a list of plots)
                    ylab = NULL, # mannually set the ylab rather than defaulting to predicted, OR or HR
                    xlabs = NULL, # provide a list of the labels. list("age" = "Age in years", "bmi" = "Body mass index")
                    titles = NULL, # provide a list of the variables and titles: list("age" = "Age in years", "bmi" = "Body mass index")
                    ylim = NULL, # set ylim that would be used by coord cartesian for the plots. if multiple plots they all get the same ylim (which makes sense as all from same model)
                    log_y = FALSE, # have a log transformed y axis
                    log_y_breaks = NULL, # specify breaks if the y axis is log transformed (as defaults can be awful) e.g. c(0.25, 0.5, 1, 2, 4).
                    xlims = NULL, #provide a list of the variables and x limits: list("age" = c(20,40), "bmi" = c(15,30))
                    lrm_prob = FALSE, # set to true to have the plots for lrm be probability rather than OR
                    var = NULL, # character vector of variables. leave null for automatic selection of fit rcs variables
                    ... # allows any plot_grid functions to be passed
){

  # stop if modelfit isn't an rms object
  if (!inherits(modelfit, "rms")) stop("modelfit is not from an rms model")

  # Ensure datadist is set
  if (is.null(options("datadist")$datadist)) {
    dd <- datadist(data)
    options(datadist = "dd")
  }

  # list of rcs vars from model
  rcs_vars <- names(which(sapply(modelfit$Design$nonlinear, any)))

  # if no var specified, automatically use rcs_vars
  if(is.null(var)){
    if(length(rcs_vars)<1) stop("No variables specified, and no RCS variables in model")
    var <- rcs_vars
  } else {
    if (!is.character(var) || length(var) < 1) stop("var must be a character string (or a vector of character strings) containing one or more variable names.")
    if (!all(var %in% rcs_vars)) warning("Some selected variables were not modelled as RCS")
    if (!all(sapply(var, function(x) is.numeric(data[[x]])))) {
      stop("All variables being plotted must be numeric.")
    }
  }

  # define model type
  type <- intersect(c("ols","lrm","cph"), class(modelfit))
  if(length(type)<1) stop("ggrmsMD currently only supports ols, lrm, or cph.")
  if(lrm_prob && type != "lrm") stop("lrm_prob can only be set to TRUE for logistic regresssion models (lrm)")

  # do this as a list so multiple variables can be provided. nb doing this as a list so that it works nicely with plot_grid from cowplot

  plot_list <- lapply(var,function(v){

    # get pred, y axis label +/- line of no effect
    no_eff_line <- FALSE
    if(type == "ols"){
      pred <- do.call(Predict, list(modelfit, v))
      yaxislab <- "Predicted outcome"
    } else if (type == "lrm") {
      if (lrm_prob) {
        pred <- do.call(Predict, list(modelfit, v, fun = plogis))
        yaxislab <- "Predicted probability"
        no_eff_line <- FALSE
      } else {
        pred <- do.call(Predict, list(modelfit, v, fun = exp, ref.zero = TRUE))
        yaxislab <- "Odds ratio"
        no_eff_line <- TRUE
      }

    } else if (type == "cph") {
      pred <- do.call(Predict, list(modelfit, v, fun = exp, ref.zero = TRUE))
      yaxislab <- "Hazard ratio"
      no_eff_line <- TRUE
    }
    pred <- as.data.frame(pred)

    ylab <- ifelse(is.null(ylab), yaxislab, ylab)

    var_label <- attr(data[[v]], "label") # Get label attribute if it exists
    xlab <- if (!is.null(xlabs) && v %in% names(xlabs)) {
      xlabs[[v]]
    } else if (!is.null(var_label)) {
      var_label
    } else {
      v
    }

    # specify background for plots of OR or HR. nb specifying here as it needs to be first layer and ggplot doesn't like conditionals within the + chain. but it will ignore NULL
    # specify background for plots of OR or HR
    bg_layer_high <- NULL
    bg_layer_low <- NULL
    if (shade_inferior != "none" && ((type == "cph") || (type == "lrm" && !lrm_prob))) {
      if (shade_inferior == "higher") {
        colour_high <- "red"
        colour_low <- "green"
      } else if (shade_inferior == "lower") {
        colour_high <- "green"
        colour_low <- "red"
      } else {
        stop('shade_inferior must be one of "none", "higher", or "lower"')
      }
      bg_layer_high <- annotate("rect",
                                xmin = -Inf, xmax = Inf,
                                ymin = 1, ymax = Inf,
                                fill = colour_high, alpha = 0.1)
      bg_layer_low <- annotate("rect",
                               xmin = -Inf, xmax = Inf,
                               ymin = if(log_y) 1e-10 else -Inf, #so that log_y doesn't break it
                               ymax = 1,
                               fill = colour_low, alpha = 0.1)
    }

    p <- ggplot(pred,aes(x = !!sym(v), y = yhat)) +
      bg_layer_high + bg_layer_low + #nb just gets ignored if NULL
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = "grey80", alpha = 0.6) +
      geom_line(linewidth = 1.2) +
      labs(x = xlab,
           y = ylab) +
      theme(plot.caption = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))

    if(no_eff_line & noeffline){
      p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", linewidth = 0.7)
    }

    # nb important that the order of coordcartesian and scale y is in correct order, and avoid double calling coordcartesian (or it overwrites)
    # set xlim to provided values or NULL
    xlim <- if (!is.null(xlims) && v %in% names(xlims)) xlims[[v]] else NULL
    # for scaling y log 10, it breaks if you don't set y limits or y breaks
    if(log_y){
      if(is.null(ylim)) ylim <- c(min(pred$lower),max(pred$upper))
      if(any(ylim <= 0)) stop("y axis limits contain zero or negative so cannot log scaled")
      p <- p + coord_cartesian(ylim = ylim, xlim = xlim) + if (is.null(log_y_breaks)) {
        scale_y_log10()
      } else {
        scale_y_log10(breaks = log_y_breaks)
      }
    } else {
      # apply coord_cartesian (nb it just ignores any null arguments)
      p <- p + coord_cartesian(ylim = ylim, xlim = xlim)
    }

    if (!is.null(titles) && v %in% names(titles)) {
      p <- p + ggtitle(titles[[v]])
    }

    return(p)}
  )

  # return single plot, list of plots or combined plot (cowplot)
  if (length(var) == 1) {
    return(plot_list[[1]])
  } else {
    if (combined) {
      if (!requireNamespace("cowplot", quietly = TRUE)) {
        stop("Package 'cowplot' is required for combined plotting. Please install it with install.packages('cowplot').")
      }
      return(cowplot::plot_grid(plotlist = plot_list, labels = "AUTO", ...))
    }
    return(plot_list)
  }

}


