library(testthat)

test_that("create_plot_dropdown input validation", {
  expect_error(functionR::create_plot_dropdown(NULL))
  expect_error(functionR::create_plot_dropdown(list(1,2)))
})

test_that("create_plot_dropdown returns expected structure", {
  skip_on_ci() # skip heavy rendering on CI if desired
  p <- ggplot2::qplot(mpg, wt, data = mtcars)
  res <- create_plot_dropdown(list(A = p), width = 4, height = 3, units = "in", dpi = 72)
  expect_type(res, "list")
  expect_true(all(c("data_uris", "select_id", "preview_id") %in% names(res)))
  expect_true(is.character(res$data_uris) && length(res$data_uris) == 1)
})