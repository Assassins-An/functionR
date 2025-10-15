#' GSEA 图片轮播（居中放大 + 数字分页）
#'
#' 使用 slickR 生成带“中间放大、两侧半露、两侧更淡”的轮播，同时包含数字分页，
#' 并通过 adaptiveHeight 与 onRender 钩子让高度自适应，避免底部空白。
#'
#' @param imgs 字符向量，图片文件的本地路径或 URL。
#' @param width 轮播宽度，字符，如 "90%"。
#' @param height 轮播高度，字符，如 "380px"。会被 onRender 调整为内容自适应。
#' @param centerPadding 居中模式下左右露出的宽度（字符，需带单位），如 "100px"。
#' @param side_scale 两侧图片的缩放比例，数值，默认 0.80。
#' @param center_scale 中间图片的缩放比例，数值，默认 1.00。
#' @param side_opacity 两侧图片的不透明度，0~1，默认 0.45。
#' @param dots 是否显示分页（数字）点，逻辑值。
#' @param arrows 是否显示左右箭头，逻辑值。
#' @param autoplay 是否自动播放，逻辑值。
#' @param focusOnSelect 点击两侧图片是否切换到该图片，逻辑值。
#' @param adaptiveHeight 是否开启自适应高度（由 slick 提供的选项），逻辑值。
#' @param sort_numeric 是否按文件名结尾数字进行自然排序（如 ..._1.png, ..._2.png），逻辑值。
#' @param scope_class 可选，给轮播外面包一层 class，用于限定样式作用域，避免影响页面其他 slick。
#'
#' @return 一个 htmltools::tagList，可直接在 Shiny/Quarto/Rmd 中渲染。
#'
#' @examples
#' \dontrun{
#' imgs <- list.files("path/to/H", pattern = "_gseaplot2_.*\\.png$", full.names = TRUE)
#' gsea_slick(imgs, width = "90%", centerPadding = "120px",
#'            side_scale = 0.88, center_scale = 1.08, side_opacity = 0.4,
#'            sort_numeric = TRUE)
#' }
#'
#' @export
#' @importFrom htmltools tags HTML tagList div
#' @importFrom htmlwidgets onRender
gsea_slick <- function(
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

  # 可选：按文件名结尾数字进行自然排序
  if (isTRUE(sort_numeric)) {
    ids <- suppressWarnings(
      as.integer(sub(".*_(\\d+)\\.(png|jpg|jpeg|gif)$", "\\1", basename(imgs)))
    )
    imgs <- imgs[order(ids, na.last = NA)]
  }

  # 作用域前缀（把样式限定在 scope_class 容器内，避免影响其他 slick）
  prefix <- if (!is.null(scope_class)) paste0(".", scope_class, " ") else ""

  # 数字分页 + 图片自适应
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

  # 中间大、两侧小且更淡
  css_fx <- htmltools::tags$style(htmltools::HTML(sprintf('
    %s.slick-slide { opacity: %s; transition: transform .22s ease, opacity .22s ease; }
    %s.slick-center { opacity: 1; }
    %s.slick-slide img { transform: scale(%s); }
    %s.slick-center img { transform: scale(%s); }
  ', prefix, as.character(side_opacity), prefix, prefix, as.character(side_scale), prefix, as.character(center_scale))))

  # 轮播本体
  w <- slickR::slickR(imgs, slideType = "img", width = width, height = height) +
    slickR::settings(
      dots = dots, autoplay = autoplay, arrows = arrows,
      adaptiveHeight = adaptiveHeight,
      centerMode = TRUE,
      centerPadding = centerPadding,
      slidesToShow = 1,
      focusOnSelect = focusOnSelect
    )

  # 渲染后强制排版 + 自适应高度，避免底部空白
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