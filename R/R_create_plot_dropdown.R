#' Create a dropdown preview of ggplot objects embedded as PNG data URIs
#'
#' Render a named list of ggplot objects into in-memory PNGs and output an HTML dropdown
#' and preview image suitable for inclusion in Quarto / R Markdown documents (results='asis').
#'
#' @param plots_list Named list of ggplot objects, e.g. list(A = p1, B = p2)
#' @param select_id HTML id for the <select> element.
#' @param preview_id HTML id for the preview <img> element.
#' @param width Numeric width in physical units (same as ggsave).
#' @param height Numeric height in physical units (same as ggsave).
#' @param units Units for width/height: "in", "cm", or "mm". Default "in".
#' @param dpi DPI used to convert physical units to pixels (and passed as res to magick).
#' @param max_items_warn If many images, warn that embedding increases HTML size.
#' @param show_border Logical, whether to show a container border.
#' @param show_label Logical, whether to include a textual label before the select.
#' @param select_width CSS width for the select (e.g. "160px").
#' @param select_height Height of the select in pixels (int).
#' @param select_left Left offset for the select inside the container (CSS, e.g. "8px").
#' @param place "inside" or "outside" positioning for the select.
#'
#' @return Invisibly a list with elements data_uris (named character vector of data URIs),
#'         select_id and preview_id and pixel width/height used.
#' @export
create_plot_dropdown <- function(plots_list,
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
                                 place = c("inside", "outside")) {
  place <- match.arg(place)
  units <- match.arg(units)

  if (!requireNamespace("magick", quietly = TRUE)) stop("Please install 'magick'")
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Please install 'base64enc'")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("Please install 'htmltools'")

  if (!is.list(plots_list) || length(plots_list) == 0) stop("plots_list must be a non-empty named list of ggplot objects.")
  if (is.null(names(plots_list)) || any(names(plots_list) == "")) {
    old_names <- names(plots_list)
    new_names <- ifelse(is.na(old_names) | old_names == "" | is.null(old_names),
                        paste0("Plot", seq_along(plots_list)),
                        old_names)
    names(plots_list) <- new_names
  }
  if (length(plots_list) > max_items_warn) warning("Large number of images; embedding many images will make the HTML large.")

  unit_to_inches <- function(x, units) {
    switch(units,
           "in" = as.numeric(x),
           "cm" = as.numeric(x) / 2.54,
           "mm" = as.numeric(x) / 25.4,
           stop("Unsupported units"))
  }
  width_in  <- unit_to_inches(width, units)
  height_in <- unit_to_inches(height, units)
  px_width  <- ceiling(width_in  * dpi)
  px_height <- ceiling(height_in * dpi)

  make_data_uri <- function(gg) {
    img <- magick::image_graph(width = px_width, height = px_height, res = dpi)
    print(gg)
    dev.off()
    raw_png <- magick::image_write(img, format = "png")
    con <- rawConnection(raw_png, "rb")
    on.exit(close(con), add = TRUE)
    paste0("data:image/png;base64,", base64enc::base64encode(con))
  }

  data_uris <- vapply(plots_list, FUN = make_data_uri, FUN.VALUE = character(1), USE.NAMES = TRUE)

  top_padding <- if (place == "inside") paste0(as.integer(select_height) + 8, "px") else "0"
  container_style <- if (isTRUE(show_border)) {
    sprintf("position:relative;border:1px solid #ddd;padding-top:%s;padding-left:0;padding-right:0;background:transparent;", top_padding)
  } else {
    sprintf("position:relative;border:none;padding-top:%s;padding-left:0;padding-right:0;background:transparent;", top_padding)
  }

  cat(sprintf('<div style="%s">\n', container_style))

  if (place == "inside") {
    select_style <- sprintf("position:absolute;top:8px;left:%s;z-index:9999;min-width:%s;height:%spx;background:rgba(255,255,255,0.95);border:1px solid #ccc;padding:2px 6px;border-radius:4px;",
                            select_left, select_width, as.integer(select_height))
  } else {
    select_style <- sprintf("position:fixed;top:8px;left:%s;z-index:9999;min-width:%s;height:%spx;background:rgba(255,255,255,0.95);border:1px solid #ccc;padding:2px 6px;border-radius:4px;",
                            select_left, select_width, as.integer(select_height))
  }

  if (isTRUE(show_label)) {
    cat(sprintf('<label for="%s" style="font-weight:600; margin-right:8px;"></label>\n', htmltools::htmlEscape(select_id)))
  }

  cat(sprintf('<select id="%s" style="%s">', htmltools::htmlEscape(select_id), select_style), "\n")
  for (nm in names(data_uris)) {
    val <- data_uris[[nm]]
    cat(sprintf('<option value="%s">%s</option>\n', htmltools::htmlEscape(val, attribute = TRUE), htmltools::htmlEscape(nm)))
  }
  cat('</select>\n')

  first_uri <- data_uris[[1]]
  img_style <- "max-width:100%;height:auto;display:block;margin:0 auto;border:none;"
  cat(sprintf('<div style="width:100%%;">\n<img id="%s" src="%s" style="%s">\n</div>\n',
              htmltools::htmlEscape(preview_id), htmltools::htmlEscape(first_uri, attribute = TRUE), img_style))

  cat('</div>\n')

  js <- sprintf('
<script>
(function(){
  var picker = document.getElementById("%s");
  var preview = document.getElementById("%s");
  if (!picker || !preview) { console.warn("plot picker or preview not found"); return; }
  picker.addEventListener("change", function(){
    var v = this.value;
    preview.src = v;
  });
})();
</script>
', select_id, preview_id)
  cat(js)

  invisible(list(data_uris = data_uris,
                 select_id = select_id,
                 preview_id = preview_id,
                 px_width = px_width,
                 px_height = px_height,
                 units = units,
                 dpi = dpi))
}