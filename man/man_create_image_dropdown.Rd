\name{create_image_dropdown}
\alias{create_image_dropdown}
\title{Create a dropdown preview of image files}
\description{
Display a dropdown of image labels and render the selected image preview.
}
\usage{
create_image_dropdown(
  img_df,
  label_col = "sample",
  file_col = "png_file",
  select_id = "image-dropdown-select",
  preview_id = "image-dropdown-preview",
  width = "90%",
  max_height = "620px"
)
}
\arguments{
  \item{img_df}{A data frame containing image metadata and file paths.}
  \item{label_col}{Column name used for dropdown labels. Default "sample".}
  \item{file_col}{Column name used for image file paths. Default "png_file".}
  \item{select_id}{HTML id for the <select> element.}
  \item{preview_id}{HTML id for the preview container.}
  \item{width}{CSS width for the outer container. Default "90%".}
  \item{max_height}{CSS max-height for the previewed image. Default "620px".}
}
\value{
Invisibly prints HTML markup for the dropdown and image preview.
}
\export