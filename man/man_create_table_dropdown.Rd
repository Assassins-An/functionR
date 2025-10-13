\name{create_table_dropdown}
\alias{create_table_dropdown}
\title{表格下拉（v9 配置），并复用通用下拉容器 dropdown_container}
\description{
把每个 \code{data.frame} 用 \code{DT::datatable} 按 v9 参数渲染为交互表，然后用 \code{dropdown_container()} 生成一个与 \code{create_plot_dropdown} 相同下拉 id（\code{select_plot}）的切换区域。
}
\usage{
create_table_dropdown(dfs,
  style = "bootstrap4",
  filter = "top",
  pageLength = 10,
  scrollY = "400px",
  scrollX = TRUE,
  dom = "Bfrtip",
  buttons = c("copy","csv","excel","pdf"),
  dt_rownames = TRUE,
  use_rowname_col = FALSE,
  select_label = "Choose table:",
  select_id = "select_plot")
}
\arguments{
  \item{dfs}{命名的 \code{list}，每个元素是 \code{data.frame}。list 的名称用于下拉选项和表格 caption。}
  \item{style}{传给 \code{DT::datatable} 的 \code{style}（默认 \code{"bootstrap4"}）。}
  \item{filter}{传给 \code{DT::datatable} 的 \code{filter}（默认 \code{"top"}）。}
  \item{pageLength}{每页行数（默认 10）。}
  \item{scrollY}{纵向滚动高度（默认 \code{"400px"}；设为 \code{NULL} 可禁用）。}
  \item{scrollX}{是否启用横向滚动（默认 \code{TRUE}）。}
  \item{dom}{DataTables 的 DOM 布局（默认 \code{"Bfrtip"}）。}
  \item{buttons}{Buttons 扩展的按钮（默认 \code{c("copy","csv","excel","pdf")}）。}
  \item{dt_rownames}{是否显示 DT 行号列（默认 \code{TRUE}）。若 \code{use_rowname_col=TRUE}，为避免重复会自动置为 \code{FALSE}。}
  \item{use_rowname_col}{是否将行名作为第一列 \code{Row} 插入（默认 \code{FALSE}）。}
  \item{select_label}{下拉菜单标签文本（默认 \code{"Choose table:"}）。}
  \item{select_id}{下拉菜单 id（默认 \code{"select_plot"}），与 \code{create_plot_dropdown} 一致。}
}
\details{
- 依赖由 \pkg{DT}/htmlwidgets 自动注入，不显式加载 jQuery，减少与 \pkg{plotly} 等冲突。\cr
- 为避免隐藏状态初始化导致列宽为 0，前端会先让所有表完成一次初始渲染，然后只保留选中的表，并在切换时轻量调整列宽。\cr
- 若不需要竖向滚动，把 \code{scrollY = NULL} 即可。
}
\value{
返回一个 \pkg{htmltools} 可渲染对象。
}
\examples{
\dontrun{
library(DT); library(htmltools)
dfs <- list(
  C1 = head(mtcars),
  C2 = head(iris)
)
# v9 参数
create_table_dropdown(dfs)

# 去掉竖向滚动
create_table_dropdown(dfs, scrollY = NULL)

# 使用行名作为第一列，且不显示 DT 行号列
create_table_dropdown(dfs, use_rowname_col = TRUE, dt_rownames = FALSE)
}
}