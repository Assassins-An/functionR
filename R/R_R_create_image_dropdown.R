#' Create a dropdown preview of image files
#'
#' Display a dropdown of image labels and render the selected image preview.
#'
#' @param img_df A data frame containing image metadata and file paths.
#' @param label_col Column name used for dropdown labels. Default "sample".
#' @param file_col Column name used for image file paths. Default "png_file".
#' @param select_id HTML id for the <select> element.
#' @param preview_id HTML id for the preview container.
#' @param width CSS width for the outer container. Default "90%".
#' @param max_height CSS max-height for the previewed image. Default "620px".
#'
#' @return Invisibly prints HTML markup for the dropdown and image preview.
#' @export
create_image_dropdown <- function(
  img_df,
  label_col = "sample",
  file_col = "png_file",
  select_id = "image-dropdown-select",
  preview_id = "image-dropdown-preview",
  width = "90%",
  max_height = "620px",
  embed = TRUE
) {
  if (is.null(img_df) || nrow(img_df) == 0) {
    cat("<p>No images to display.</p>")
    return(invisible(NULL))
  }

  img_df <- img_df[!is.na(img_df[[file_col]]) & nzchar(img_df[[file_col]]), , drop = FALSE]

  if (nrow(img_df) == 0) {
    cat("<p>No image files found.</p>")
    return(invisible(NULL))
  }

  img_input <- as.character(img_df[[file_col]])

  is_data_uri <- grepl("^data:image/", img_input)

  if (embed) {
    img_src <- ifelse(
      is_data_uri,
      img_input,
      vapply(img_input, knitr::image_uri, character(1))
    )
  } else {
    keep <- is_data_uri | file.exists(img_input)
    img_df <- img_df[keep, , drop = FALSE]
    img_input <- img_input[keep]
    is_data_uri <- is_data_uri[keep]

    if (nrow(img_df) == 0) {
      cat("<p>No image files found.</p>")
      return(invisible(NULL))
    }

    img_src <- ifelse(
      is_data_uri,
      img_input,
      normalizePath(img_input, winslash = "/", mustWork = TRUE)
    )
  }

  labels <- as.character(img_df[[label_col]])

  options_html <- paste0(
    vapply(seq_along(labels), function(i) {
      paste0(
        '<option value="', i, '">',
        htmltools::htmlEscape(labels[i]),
        '</option>'
      )
    }, character(1)),
    collapse = "\n"
  )

  img_tags <- vapply(seq_along(img_src), function(i) {
    paste0(
      '<img src="',
      img_src[i],
      '" style="max-width:100%; max-height:',
      max_height,
      '; object-fit:contain; display:block; margin:0 auto;">'
    )
  }, character(1))

  js_images <- jsonlite::toJSON(img_tags, auto_unbox = TRUE)

  html <- paste0(
    '<div class="cellchat-image-dropdown" style="width:', width, '; margin: 0 auto;">',
    '<select id="', select_id, '" style="margin-bottom:12px; max-width:100%;">',
    options_html,
    '</select>',
    '<div id="', preview_id, '"></div>',
    '</div>',
    '<script>',
    '(function(){',
    'const images = ', js_images, ';',
    'const select = document.getElementById("', select_id, '");',
    'const preview = document.getElementById("', preview_id, '");',
    'function render(){ preview.innerHTML = images[Number(select.value)-1] || ""; }',
    'select.addEventListener("change", render);',
    'render();',
    '})();',
    '</script>'
  )

  cat(as.character(htmltools::HTML(html)))
  invisible(NULL)
}
