\name{create_plotly_dropdown}
\alias{create_plotly_dropdown}
\title{Combine multiple ggplot/plotly objects into a single Plotly figure with a dropdown (no title)}
\usage{
create_plotly_dropdown(plots_list, initial = 1, menus_x = 0, menus_y = 1.25,
  menus_xanchor = "left", legend = TRUE)
}
\arguments{
  \item{plots_list}{A named list of \code{ggplot} or \code{plotly} objects. If names are missing they will be generated (e.g. "Plot1", "Plot2"). Each list element becomes one dropdown item that shows the traces belonging to that element.}
  \item{initial}{Integer. 1-based index of which plot to show initially. If not in range, defaults to \code{1}.}
  \item{menus_x}{Numeric. x-position (in normalized plot coordinates) of the dropdown menu. Default is \code{0}.}
  \item{menus_y}{Numeric. y-position (in normalized plot coordinates) of the dropdown menu. Default is \code{1.25}. Values > 1 place the menu above the plotting area; using \code{1} or \code{<1} reduces top whitespace.}
  \item{menus_xanchor}{Character. x-anchor for the updatemenu ("left", "center", "right"). Default is \code{"left"}.}
  \item{legend}{Logical. Whether to show the legend for the combined plot (default \code{TRUE}). The function manages per-button \code{showlegend} masks so only legend items for the active selection are shown.}
}
\description{
Combine traces from multiple \code{ggplot} or \code{plotly} objects into a single interactive Plotly htmlwidget and add a dropdown (Plotly updatemenu) that toggles which set of traces is visible. This variant does not add or update a title in the layout.
}
\details{
- Elements of \code{plots_list} that are \code{ggplot} objects are converted with \code{plotly::ggplotly()} and then built with \code{plotly::plotly_build()} to enumerate traces.
- All traces are concatenated into a single Plotly object. For each dropdown item the function computes both a \code{visible} mask and a \code{showlegend} mask so that when a selection is active only the traces for that selection are visible and only a single legend entry per group is shown. This avoids long/duplicated legends that can appear if traces from all inputs are present but hidden in legend-only state.
- If a trace lacks a \code{name}, the function generates a stable fallback name which is used for grouping and legend entries.
- Because the dropdown is a Plotly updatemenu, its vertical position (\code{menus_y}) can affect top whitespace in the resulting figure (values > 1 place the menu above the plotting area and can increase top margin).
}
\value{
An htmlwidget of class \code{plotly} containing all traces from the input plots and an attached dropdown menu that toggles which traces are visible. Print the returned object to display the interactive plot.
}
\author{Generated for the package maintainer}
\seealso{
\code{\link[plotly]{ggplotly}}, \code{\link[plotly]{plotly_build}}, \code{\link[plotly]{layout}}
}
\examples{
\dontrun{
library(ggplot2)
library(plotly)

p1 <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(gear))) + geom_boxplot()
p2 <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()

plots <- list(A = p1, B = p2)

# Create combined plotly widget with dropdown (no title)
pw <- create_plotly_dropdown(plots, initial = 1, menus_x = 0, menus_y = 1, menus_xanchor = "left")
pw
}
}
\keyword{graphics}
\keyword{interactive}
\keyword{plotly}