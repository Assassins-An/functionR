\name{create_plotly_dropdown}
\alias{create_plotly_dropdown}
\title{Create a single plotly figure that switches among multiple ggplot/plotly plots via a dropdown}
\usage{
create_plotly_dropdown(plots_list, title = "Interactive plots", initial = 1,
                       menus_x = 0, menus_y = 1.25,
                       menus_xanchor = "left", legend = TRUE)
}
\arguments{
\item{plots_list}{A named list of \code{ggplot} or \code{plotly} objects. Names are used as labels in the dropdown.}
\item{title}{Character scalar. Base title for the figure; the selected plot name will be appended.}
\item{initial}{Integer. Index (1-based) of the plot to show initially.}
\item{menus_x}{Numeric. X position for the updatemenus.}
\item{menus_y}{Numeric. Y position for the updatemenus.}
\item{menus_xanchor}{Character. X anchor for the updatemenus (e.g. "left", "center", "right").}
\item{legend}{Logical. Whether to show the legend in the combined plot.}
}
\description{
Combine multiple ggplot/plotly objects into a single plotly htmlwidget and switch
between them with a dropdown menu. All traces from each input plot are included
in the combined figure; the dropdown toggles trace visibility so that only
traces from the selected plot are displayed.
}
\details{
This function is useful when you want to present multiple interactive ggplotly
charts in the same canvas and let the user switch between them without loading
separate plots or embedding external HTML/JS. The function converts ggplot
objects to plotly using \code{ggplotly} if needed; plotly objects are accepted
as-is.
}
\value{
A \code{plotly} htmlwidget (interactive plot). Print or return this object to display it.
}
\examples{
if (interactive()) {
  library(ggplot2)
  library(plotly)
  p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + ggtitle("Scatter")
  p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot() + ggtitle("Boxes")
  plots <- list(Scatter = p1, Boxes = p2)
  p_comb <- create_plotly_dropdown(plots, title = "My ggplots", initial = 1)
  p_comb
}
}
\keyword{interactive}
\keyword{graphics}