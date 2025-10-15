#' Centered image carousel with numeric pagination (slickR)
#'
#' Build a centered image carousel using \pkg{slickR}: the active slide is enlarged,
#' side slides are partially visible and dimmed, with numeric pagination.
#' The widget adapts its height via \code{adaptiveHeight} and an \code{onRender}
#' hook to avoid bottom whitespace and reflow correctly when shown in hidden tabs.
#'
#' This function is generic and works for any set of images (not limited to GSEA results).
#'
#' @param imgs Character vector of image file paths or URLs.
#' @param width Carousel width, e.g. "90%".
#' @param height Carousel height, e.g. "380px". Will be adjusted to content height after render.
#' @param centerPadding Visible padding on both sides in center mode (character with unit), e.g. "100px".
#' @param side_scale Scale factor for side slides (numeric).
#' @param center_scale Scale factor for the centered slide (numeric).
#' @param side_opacity Opacity for side slides in [0,1].
#' @param dots Show numeric pagination.
#' @param arrows Show left/right arrows.
#' @param autoplay Autoplay slides.
#' @param focusOnSelect Click side slide to navigate to it.
#' @param adaptiveHeight Enable slick's adaptiveHeight option.
#' @param sort_numeric If TRUE, sort by trailing number in file names (e.g. ..._1.png, ..._2.png).
#' @param scope_class Optional outer wrapper class to scope styles.
#'
#' @return htmltools::tagList with styles and the slickR widget.
#'
#' @examples
#' \dontrun{
#' imgs <- list.files("path/to/images", pattern = "\\\\.(png|jpg)$", full.names = TRUE)
#' slick_center_carousel(imgs, width = "90%", centerPadding = "120px",
#'                       side_scale = 0.9, center_scale = 1.08, side_opacity = 0.4,
#'                       sort_numeric = TRUE)
#' }
#'
#' @export
#' @aliases gsea_slick
#' @importFrom htmltools tags HTML tagList div
#' @importFrom htmlwidgets onRender
slick_center_carousel <- function(
  imgs,
  width          = "90%",
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
) {
  stopifnot(length(imgs) > 0)

  # Optional: natural sort by trailing number
  if (isTRUE(sort_numeric)) {
    ids <- suppressWarnings(
      as.integer(sub(".*_(\\\n\\d+)\\.(png|jpg|jpeg|gif)$", "\\1", basename(imgs)))
    )
    imgs <- imgs[order(ids, na.last = NA)]
  }

  prefix <- if (!is.null(scope_class)) paste0(".", scope_class, " ") else ""

  css_dots <- htmltools::tags$style(htmltools::HTML(sprintf('
    %s.slick-dots {
      list-style: none; padding: 0; margin: 12px 0;
      display: flex; gap: 6px; justify-content: center; counter-reset: dot;
    }
    %s.slick-dots li { margin: 0; display: inline-block; counter-increment: dot; }
    %s.slick-dots li button:before {
      content: counter(dot);
      display: inline-block; text-align: center;
      width: 28px; height: 28px; line-height: 28px;
      background: #e5e7eb; color: #111827; border: 1px solid #9ca3af; border-radius: 4px;
      font-weight: 700; font-size: 13px; opacity: 1 !important;
    }
    %s.slick-dots li.slick-active button:before {
      background: #2563eb; color: #fff; border-color: #1d4ed8; opacity: 1 !important;
    }
    %s.slick-slide img { width: 100%% !important; height: auto !important; display: block; }
  ', prefix, prefix, prefix, prefix, prefix)))

  css_fx <- htmltools::tags$style(htmltools::HTML(sprintf('
    %s.slick-slide { opacity: %s; transition: transform .22s ease, opacity .22s ease; }
    %s.slick-center { opacity: 1; }
    %s.slick-slide img { transform: scale(%s); }
    %s.slick-center img { transform: scale(%s); }
  ', prefix, as.character(side_opacity), prefix, prefix, as.character(side_scale), prefix, as.character(center_scale))))

  w <- slickR::slickR(imgs, slideType = "img", width = width, height = height) +
    slickR::settings(
      dots = dots, autoplay = autoplay, arrows = arrows,
      adaptiveHeight = adaptiveHeight,
      centerMode = TRUE,
      centerPadding = centerPadding,
      slidesToShow = 1,
      focusOnSelect = focusOnSelect
    )

  w <- htmlwidgets::onRender(w, "
    function(el){
      function setAutoHeight(){
        el.style.height = 'auto';
        var $ = window.jQuery;
        if($){
          var $slider = $(el).find('.slick-slider');
          var $dots   = $(el).find('.slick-dots');
          var h = 0;
          if ($slider.length) h += $slider.outerHeight(true);
          if ($dots.length)   h += $dots.outerHeight(true);
          if (h > 0) el.style.height = h + 'px';
        }
      }
      function refresh(){
        var $ = window.jQuery;
        if($){
          var $s = $(el).find('.slick-initialized');
          if($s.length){ $s.slick('setPosition'); }
        }
        setAutoHeight();
      }
      setTimeout(refresh, 0);
      if (window.jQuery) window.jQuery(el).find('img').on('load', refresh);
      document.addEventListener('shown.bs.tab', refresh);
      window.addEventListener('resize', refresh);
    }
  ")

  out <- htmltools::tagList(css_dots, css_fx, w)
  if (!is.null(scope_class)) out <- htmltools::div(class = scope_class, out)
  out
}

# Backward-compatible alias (keeps existing user code working)
#' @export
gsea_slick <- slick_center_carousel