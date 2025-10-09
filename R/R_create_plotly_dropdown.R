#' Create a single plotly figure that switches between multiple ggplot/plotly plots via a dropdown
#'
#' Combine multiple ggplot/plotly objects into one plotly htmlwidget with a dropdown menu
#' to toggle which set of traces is visible. This variant does not set or update a title.
#'
#' @param plots_list A named list of ggplot or plotly objects.
#' @param initial Integer. 1-based index of which plot to show initially.
#' @param menus_x Numeric. x-position of the dropdown menu.
#' @param menus_y Numeric. y-position of the dropdown menu.
#' @param menus_xanchor Character. x-anchor for the updatemenu ("left", "center", "right").
#' @param legend Logical. Whether to show the legend for the combined plot.
#' @return A plotly htmlwidget.
#' @export
create_plotly_dropdown <- function(plots_list,
                                   initial = 1,
                                   menus_x = -0.5,
                                   menus_y = 1.15,
                                   menus_xanchor = "left",
                                   legend = TRUE) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Please install 'plotly'")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2' if you pass ggplot objects")
  if (!is.list(plots_list) || length(plots_list) == 0) stop("plots_list must be a non-empty named list of ggplot/plotly objects")
  if (is.null(names(plots_list)) || any(names(plots_list) == "")) names(plots_list) <- paste0("Plot", seq_along(plots_list))
  nplots <- length(plots_list)
  if (!(initial %in% seq_len(nplots))) initial <- 1

  # convert each to plotly object (if needed) and get traces
  plotly_objs <- vector("list", nplots)
  trace_counts <- integer(nplots)

  for (i in seq_len(nplots)) {
    p <- plots_list[[i]]
    if (inherits(p, "ggplot")) {
      plotly_objs[[i]] <- plotly::ggplotly(p)
    } else if (inherits(p, "plotly") || inherits(p, "plotly_htmlwidget")) {
      plotly_objs[[i]] <- p
    } else {
      stop("Element ", i, " of plots_list is neither a ggplot nor a plotly object")
    }
    pb <- plotly::plotly_build(plotly_objs[[i]])
    trace_counts[i] <- length(pb$x$data)
    plotly_objs[[i]] <- pb
  }

  # Combine traces
  combined <- plotly_objs[[1]]
  all_traces <- combined$x$data
  if (nplots > 1) {
    for (i in 2:nplots) {
      all_traces <- c(all_traces, plotly_objs[[i]]$x$data)
    }
  }

  total_traces <- length(all_traces)
  # Build stable trace names (fallback to generated names) and assign legendgroup
  trace_names <- character(total_traces)
  for (i in seq_len(total_traces)) {
    tr <- all_traces[[i]]
    nm <- NULL
    if (!is.null(tr$name) && nzchar(as.character(tr$name))) nm <- as.character(tr$name)
    if (is.null(nm) || !nzchar(nm)) nm <- paste0("trace_", i)
    trace_names[i] <- nm
    # set legendgroup to group identical names (helps grouping)
    all_traces[[i]]$legendgroup <- trace_names[i]
    # ensure a consistent name exists for tooltip/legend grouping
    all_traces[[i]]$name <- trace_names[i]
  }
  combined$x$data <- all_traces

  # Determine indices mapping for each plot
  idx_start <- cumsum(c(1, trace_counts))[1:nplots]
  idx_end <- cumsum(trace_counts)

  # visibility masks per plot
  vis_list <- lapply(seq_len(nplots), function(k) {
    vis <- rep(FALSE, total_traces)
    vis[idx_start[k]:idx_end[k]] <- TRUE
    vis
  })

  # For each plot, compute showlegend mask: among visible traces, only first occurrence of each name shows legend
  showlegend_list <- lapply(seq_len(nplots), function(k) {
    vis <- vis_list[[k]]
    mask <- rep(FALSE, total_traces)
    if (any(vis)) {
      vis_idx <- which(vis)
      vis_names <- trace_names[vis_idx]
      seen <- character(0)
      for (j in seq_along(vis_idx)) {
        nm <- vis_names[j]
        if (!(nm %in% seen)) {
          mask[vis_idx[j]] <- TRUE
          seen <- c(seen, nm)
        } else {
          mask[vis_idx[j]] <- FALSE
        }
      }
    }
    mask
  })

  # Build buttons: update both visible and showlegend. No relayout/title updates are performed.
  buttons <- lapply(seq_len(nplots), function(k) {
    restyle_args <- list(visible = vis_list[[k]], showlegend = showlegend_list[[k]])
    # Provide an empty relayout argument to ensure the update method has the right shape
    list(method = "update", args = list(restyle_args, list()), label = names(plots_list)[k])
  })

  # set initial visibility and showlegend: only initial traces visible and their legend entries shown
  initial_vis <- vis_list[[initial]]
  initial_showlegend <- showlegend_list[[initial]]
  combined$x$data <- lapply(seq_len(total_traces), function(i) {
    tr <- combined$x$data[[i]]
    tr$visible <- if (initial_vis[i]) TRUE else FALSE
    tr$showlegend <- if (initial_showlegend[i]) TRUE else FALSE
    tr
  })

  # Build layout with updatemenus (no title)
  combined <- plotly::layout(combined,
                             updatemenus = list(list(active = initial - 1,
                                                     buttons = buttons,
                                                     x = menus_x,
                                                     y = menus_y,
                                                     xanchor = menus_xanchor,
                                                     direction = "down")),
                             showlegend = legend)

  return(combined)
}