#' Create a single plotly figure that switches between multiple ggplot/plotly plots via a dropdown
#'
#' Convert a named list of ggplot (or plotly) objects into a single combined plotly object
#' whose traces are toggled by a dropdown menu (plotly updatemenus). The function places
#' all traces from each ggplot/plotly into one figure, and each dropdown entry shows only
#' the traces belonging to the selected plot and updates the title accordingly.
#'
#' This is useful if you want multiple interactive ggplotly charts available in the same
#' canvas and switch between them with a plotly dropdown (no external HTML/JS wrapper needed).
#'
#' @param plots_list A named list of ggplot objects or plotly objects. Names will be used as dropdown labels.
#' @param title Character. Base title for the combined figure. The dropdown will append the selected name.
#' @param initial Integer. Index (1-based) of the plot to show initially. Defaults to 1.
#' @param menus_x Numeric. x position of the updatemenus in plot coordinate (default 0).
#' @param menus_y Numeric. y position of the updatemenus in plot coordinate (default 1.25).
#' @param menus_xanchor Character. xanchor for the updatemenus list (default "left").
#' @param legend Logical. Whether to show the legend on the combined plot (default TRUE).
#'
#' @return A plotly htmlwidget (combined plotly object). Print this object to display the interactive plot.
#'
#' @details
#' - Each ggplot in \code{plots_list} is converted to a plotly object via \code{ggplotly()}.
#' - All traces are concatenated. The dropdown uses the plotly \code{update} method to toggle
#'   visibility of traces so that only the traces of the selected plot are visible.
#' - The function attempts to preserve the individual traces' appearance and legend entries.
#' - Hidden traces are set to \code{"legendonly"} so users can optionally toggle them in the legend.
#'
#' @examples
#' if (interactive()) {
#'   library(ggplot2)
#'   library(plotly)
#'   p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + ggtitle("Scatter")
#'   p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot() + ggtitle("Boxes")
#'   plots <- list(Scatter = p1, Boxes = p2)
#'   p_comb <- create_plotly_dropdown(plots, title = "My ggplots", initial = 1)
#'   p_comb
#' }
#'
#' @export
#' @importFrom plotly ggplotly plotly_build layout
create_plotly_dropdown <- function(plots_list, title = "Interactive plots", initial = 1,
                                   menus_x = 0, menus_y = 1.25,
                                   menus_xanchor = "left", legend = TRUE) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Please install 'plotly'")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # ggplot2 is only required if the user passes ggplot objects.
    # We don't force install here; check later when converting.
    # message is omitted to keep behavior consistent.
    NULL
  }
  if (!is.list(plots_list) || length(plots_list) == 0) stop("plots_list must be a non-empty named list of ggplot/plotly objects")
  if (is.null(names(plots_list)) || any(names(plots_list) == "")) names(plots_list) <- paste0("Plot", seq_along(plots_list))
  nplots <- length(plots_list)
  if (!(initial %in% seq_len(nplots))) initial <- 1

  # convert each to plotly object (if needed) and get traces
  plotly_objs <- vector("list", nplots)
  trace_counts <- integer(nplots)

  for (i in seq_len(nplots)) {
    p <- plots_list[[i]]
    # if it's a ggplot, convert; if it's a plotly htmlwidget, keep
    if (inherits(p, "ggplot")) {
      plotly_objs[[i]] <- plotly::ggplotly(p)
    } else if (inherits(p, "plotly") || inherits(p, "plotly_htmlwidget")) {
      plotly_objs[[i]] <- p
    } else {
      stop("Element ", i, " of plots_list is neither a ggplot nor a plotly object")
    }
    # ensure built structure
    pb <- plotly::plotly_build(plotly_objs[[i]])
    trace_counts[i] <- length(pb$x$data)
    # store the built object (use pb for consistent structure)
    plotly_objs[[i]] <- pb
  }

  # Combine traces: start with first object's data and layout
  combined <- plotly_objs[[1]]
  all_traces <- combined$x$data
  # append other traces
  if (nplots > 1) {
    for (i in 2:nplots) {
      all_traces <- c(all_traces, plotly_objs[[i]]$x$data)
    }
  }
  combined$x$data <- all_traces

  total_traces <- length(all_traces)

  # Build visibility masks for each button: only traces of that plot are visible
  # Determine indices for each plot's traces in the combined trace vector
  idx_start <- cumsum(c(1, trace_counts))[1:nplots]
  idx_end <- cumsum(trace_counts)
  vis_list <- lapply(seq_len(nplots), function(k) {
    vis <- rep(FALSE, total_traces)
    vis[idx_start[k]:idx_end[k]] <- TRUE
    vis
  })

  # Buttons: update visible traces and relayout (title)
  buttons <- lapply(seq_len(nplots), function(k) {
    restyle_args <- list(visible = vis_list[[k]])
    relayout_args <- list(title = paste0(title, " - ", names(plots_list)[k]))
    list(
      method = "update",
      args = list(restyle_args, relayout_args),
      label = names(plots_list)[k]
    )
  })

  # set initial visibility so only the 'initial' plot traces are visible
  combined$x$data <- lapply(seq_len(total_traces), function(i) {
    tr <- combined$x$data[[i]]
    tr$visible <- ifelse(vis_list[[initial]][i], TRUE, "legendonly")  # use "legendonly" so hidden traces can appear in legend optionally
    tr
  })

  # Merge layouts: keep combined layout but update title/legend
  combined <- plotly::layout(combined,
                             title = paste0(title, " - ", names(plots_list)[initial]),
                             updatemenus = list(list(active = initial - 1,
                                                     buttons = buttons,
                                                     x = menus_x,
                                                     y = menus_y,
                                                     xanchor = menus_xanchor,
                                                     direction = "down")),
                             showlegend = legend)

  # Return the combined plotly object
  return(combined)
}