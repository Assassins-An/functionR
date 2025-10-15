\name{slick_center_carousel}
\alias{slick_center_carousel}
\alias{gsea_slick}
\title{Centered image carousel with numeric pagination (slickR)}

\description{
Build a centered image carousel using \pkg{slickR}: the active slide is enlarged,
side slides are partially visible and dimmed, with numeric pagination.\cr
This is a generic image carousel (not limited to GSEA outputs).\cr
Height is adapted via \code{adaptiveHeight} and an \code{onRender} hook to avoid bottom whitespace.
}

\usage{
slick_center_carousel(
  imgs,
  width          = "90\%",
  height         = "380px",
  centerPadding  = "100px",
  side_scale     = 0.80,
  center_scale   = 1.00,
  side_opacity   = 0.45,
  dots           = TRUE,
  arrows         = TRUE,
  autoplay       = FALSE,
  focusOnSelect  = TRUE,
  adaptiveHeight = TRUE,
  sort_numeric   = FALSE,
  scope_class    = NULL
)
}

\arguments{
  \item{imgs}{Character vector of image file paths or URLs.}
  \item{width}{Carousel width, e.g. \code{"90\%"}.}
  \item{height}{Carousel height, e.g. \code{"380px"}. Adjusted to content after render.}
  \item{centerPadding}{Padding on both sides in center mode (character with unit), e.g. \code{"100px"}.}
  \item{side_scale}{Scale factor for side slides.}
  \item{center_scale}{Scale factor for the centered slide.}
  \item{side_opacity}{Opacity for side slides in [0,1].}
  \item{dots}{Show numeric pagination.}
  \item{arrows}{Show left/right arrows.}
  \item{autoplay}{Autoplay slides.}
  \item{focusOnSelect}{Click side slide to navigate to it.}
  \item{adaptiveHeight}{Enable slick's \code{adaptiveHeight}.}
  \item{sort_numeric}{Sort by trailing number in file names (e.g. \code{..._1.png}).}
  \item{scope_class}{Optional wrapper class to scope styles.}
}

\value{
A \code{\link[htmltools]{tagList}} that contains the styles and the \pkg{slickR} widget.
}

\details{
- Center mode (\code{centerMode = TRUE}) with \code{centerPadding} controls how much of the side slides is visible.\cr
- CSS controls enlargement and dimming: \code{center_scale}, \code{side_scale}, \code{side_opacity}.\cr
- An \code{\link[htmlwidgets]{onRender}} hook triggers slick's \code{setPosition} on initial render,
  image load, Bootstrap Tab activation, and window resize, and recomputes container height.
}

\examples{
\dontrun{
imgs <- list.files("path/to/images", pattern = "\\\\.(png|jpg)$", full.names = TRUE)

# Basic usage
slick_center_carousel(
  imgs,
  width = "90\%",
  centerPadding = "120px",
  side_scale = 0.90,
  center_scale = 1.08,
  side_opacity = 0.40,
  sort_numeric = TRUE
)

# Alias kept for backward compatibility:
gsea_slick(imgs)
}
}

\seealso{
\code{\link[slickR]{slickR}}, \code{\link[htmltools]{tagList}},
\code{\link[htmlwidgets]{onRender}}
}

\author{
functionR 开发团队
}