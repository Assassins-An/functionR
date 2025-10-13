\name{create_plot_dropdown}
\alias{create_plot_dropdown}
\title{Create a dropdown preview of ggplot objects embedded as PNG data URIs}
\encoding{UTF-8}
\description{
Render a named list of \pkg{ggplot2} objects into in-memory PNG images and output
HTML for a select dropdown and a preview \code{<img>} suitable for inclusion in
Quarto / R Markdown documents (use in a chunk with \code{results='asis'}).
}
\usage{
create_plot_dropdown(plots_list,
  select_id = "plot-picker",
  preview_id = "plot-preview",
  width = 7,
  height = 4,
  units = c("in", "cm", "mm"),
  dpi = 300,
  max_items_warn = 50,
  show_border = FALSE,
  show_label = FALSE,
  select_width = "160px",
  select_height = 36,
  select_left = "8px",
  place = c("inside", "outside"))
}
\arguments{
  \item{plots_list}{Named list of \pkg{ggplot2} objects, e.g. \code{list(A = p1, B = p2)}.}
  \item{select_id}{Character; HTML \code{id} for the \code{<select>} element.}
  \item{preview_id}{Character; HTML \code{id} for the preview \code{<img>} element.}
  \item{width, height}{Numeric physical dimensions (same units as \pkg{ggplot2} / \code{ggsave}).}
  \item{units}{Character; one of \code{"in"}, \code{"cm"} or \code{"mm"}.}
  \item{dpi}{Numeric; dots per inch used to convert physical units to pixels.}
  \item{max_items_warn}{Integer; threshold to warn about embedding many images.}
  \item{show_border, show_label}{Logical; control simple presentation options.}
  \item{select_width, select_height, select_left}{CSS sizing/positioning for the select control.}
  \item{place}{Character; \code{"inside"} or \code{"outside"} — position of the select.}
}
\details{
The function uses \pkg{magick} to render ggplots into in-memory PNGs and
\pkg{base64enc} to produce data URIs embedded directly in the generated HTML.
If \pkg{magick} is not installed, the function will raise an informative error.
}
\value{
Invisibly returns a list with elements:
\itemize{
\item{\code{data_uris}:}{named character vector of PNG data URIs (one per plot).}
\item{\code{select_id}, \code{preview_id}:}{the HTML ids used.}
\item{\code{px_width}, \code{px_height}:}{pixel dimensions computed from \code{width}, \code{height}, and \code{dpi}.}
\item{\code{units}, \code{dpi}:}{the units and dpi used.}
}
}
\examples{
## In a Quarto/R Markdown chunk use:
## {r, results='asis', echo=FALSE}
# library(functionR)
# library(ggplot2)
# p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
# create_plot_dropdown(list(Scatter = p1, Boxes = p2),
#                      width = 7, height = 4, units = "in", dpi = 150)
}
\keyword{html}